{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Apply
-- Description : Apply migrations to a database.
-- Copyright   : (c) Brian Hurt, 2023
-- License     : None
-- Maintainer  : Brian Hurt <bhurt42@gmail.com>
-- Stability   : experimental
--
-- This is an internal module of
-- "Database.PostgreSQL.Simple.Migrate.Internal", you probably want
-- that module instead.
--

module Database.PostgreSQL.Simple.Migrate.Internal.Apply (
    makeFingerprint,
    apply
) where

    import           Control.DeepSeq                        (force)
    import           Control.Exception                      (throw)
    import qualified Control.Exception                      as Exception
    import           Control.Monad                          (when)
    import qualified Data.CaseInsensitive                   as CI
    import           Data.Map.Strict                        (Map)
    import qualified Data.Map.Strict                        as Map
    import           Data.Text                              (Text)
    import qualified Database.PostgreSQL.Simple             as PG
    import           Database.PostgreSQL.Simple.SqlQQ
    import qualified Database.PostgreSQL.Simple.Transaction as PG

    -- These screw up the formatting, so they get their own,
    -- unformatted, block.
    import Database.PostgreSQL.Simple.Migrate.Internal.Types
    import Database.PostgreSQL.Simple.Migrate.Internal.Order

    -- | Calculate the fingerprint of a given migration.
    makeFingerprint :: Migration -> Text
    makeFingerprint = undefined

    -- | The verbosity level.
    --
    -- How much detail to print out.
    data Verbose =

            -- | Only print out error messages.
            Quiet

            -- | Print out error messages and phase changes.
            | Low

            -- | Also print out which migrations we are applying
            | Medium

            -- | Also print out what database queries we are performing.
            | High
        deriving (Show, Read, Ord, Eq, Enum, Bounded)

    -- | Apply a set of migrations to a database.
    --
    -- If the `Database.PostgreSQL.Simple` library throws an exception,
    -- it is rethrown by this function.  Otherwise, if there is an
    -- error, the error printed and the application of migrations
    -- is halted.
    --
    apply :: Verbose
                -- ^ What messages to print out

                -> [ Migration ]
                -- ^ The list of migrations to apply.

                -> IO PG.Connection
                -- ^ How to create a database connection.
                --
                -- The connection is not made unless and until it is
                -- needed.  The create connection will be closed when
                -- the function exits (even if it exits by throwing an
                -- exception).

                -> IO Bool
                -- ^ True means all migrations were applied successfully.
                --
                -- False means a failure occurred.
                
    apply _       []    _        = do
        putStrLn "Error: empty migrations list!"
        pure False
    apply verbose migs1 makeConn = do
        when (verbose >= Low) $ putStrLn "Ordering migrations."
        case orderMigrations migs1 of
            Left err   -> do
                putStrLn $ "Error: " ++ formatOrderMigrationsError err
                pure False
            Right migs2 -> do
                -- Do *all* the ordering work before opening the connection!
                migs <- Exception.evaluate $ force migs2
                when (verbose >= High) $
                    putStrLn "Opening database connection."

                -- Make the connection and apply the migrations
                r <- Exception.bracket makeConn PG.close $ \conn -> do
                        r <- applyMigrations verbose migs conn
                        when (verbose >= High) $
                            putStrLn "Closing database connection."
                        pure r
                when (verbose >= Low) $
                    if r
                    then putStrLn "Success."
                    else putStrLn "Failed."
                pure r


    data AbortMigration = AbortMigration
        deriving (Show)

    instance Exception.Exception AbortMigration

    applyMigrations :: Verbose -> [ Migration ] -> PG.Connection -> IO Bool
    applyMigrations verbose migs conn = do
            r :: Either AbortMigration () <- Exception.try go
            case r of
                Left AbortMigration -> pure False
                Right ()            -> pure True
        where
            go :: IO ()
            go = do
                -- Make sure our tables exist
                createTable verbose conn
                checkUnknown verbose conn migs
                when (verbose >= Low) $
                    putStrLn "Starting to apply migrations."
                if (verbose >= Low)
                then do
                    case migs of
                        []    -> pure ()
                        x : _ -> do
                            putStrLn $ "Starting phase " ++ show (phase x)
                    loop migs
                else
                    mapM_ (applyAMigration verbose conn) migs

            -- | Apply migrations, printing phase changes.
            --
            -- We can't do a mapM_ because we need to catch the phase
            -- changes and print them out.
            --
            loop :: [ Migration ] -> IO ()
            loop []                      = pure ()
            loop [x]                     = applyAMigration verbose conn x
            loop (x1 : x2 : xs)
                | (phase x1 /= phase x2) = do
                    applyAMigration verbose conn x1
                    when (verbose >= Low) $
                        putStrLn $ "Starting phase " ++ show (phase x2)
                    loop (x2 : xs)
                | otherwise              = do
                    applyAMigration verbose conn x1
                    loop (x2 : xs)


    -- | Check if the schema_migrations table exists, and create it if it
    -- doesn't.
    createTable :: Verbose -> PG.Connection -> IO ()
    createTable verbose conn = do
        when (verbose >= Medium) $
            putStrLn "Initializing database..."
        PG.withTransactionLevel PG.Serializable conn $ do
            r1 :: [ PG.Only Bool ]
                <- myQuery_ verbose conn
                    [sql|   SELECT EXISTS (
                                SELECT 1
                                FROM pg_tables
                                WHERE tablename = 'schema_migrations'
                                LIMIT 1
                            ); |]
            case r1 of
                -- If the table exists, we assume it's OK and just go on.
                -- This is the normal case.
                (PG.Only True) : _ -> pure ()
                _                  -> do
                    -- We need to create the table.
                    myExecute_ verbose conn
                        [sql|
                            CREATE TABLE IF NOT EXISTS schema_migrations(
                                name TEXT PRIMARY KEY,
                                fingerprint CHAR(44) UNIQUE NOT NULL,
                                executed_at TIMESTAMP WITH TIME ZONE
                                    NOT NULL DEFAULT NOW());
                        |]
                    myExecute_ verbose conn
                        [sql|
                            CREATE INDEX ON schema_migrations(executed_at);
                        |]

    -- | Check to make sure there are not schema that have already
    -- been applied that we don't know about.
    --
    -- This is a sure sign that something hinky is going on.
    --
    checkUnknown :: Verbose -> PG.Connection -> [ Migration ] -> IO ()
    checkUnknown verbose conn migs = do
            when (verbose >= Medium) $
                putStrLn "Checking for unknown migrations..."
            r :: [ PG.Only Text ]
                <- myQuery verbose conn
                    [sql|
                        SELECT name FROM schema_migrations
                            WHERE name NOT IN ?;
                    |]
                    (PG.Only (PG.In (migs >>= migNames)))
            case r of
                [] -> pure ()
                _  -> do
                    putStrLn "Error: The following migrations have already\
                                \ been applied but aren't"
                    putStrLn "in our current list:"
                    mapM_ formatName r
                    putStrLn "(Are you applying the wrong set of\
                                \ migrations to the wrong database?)"
                    throw AbortMigration

        where
            migNames :: Migration -> [ Text ]
            migNames mig = CI.foldedCase <$>
                            (name mig : (rName <$> replaces mig))

            formatName :: PG.Only Text -> IO ()
            formatName (PG.Only nm) = putStrLn $ "    " ++ show nm


    applyAMigration :: Verbose -> PG.Connection -> Migration -> IO ()
    applyAMigration verbose conn mig = do
            when (verbose >= Medium) $
                putStrLn $ "Applying Migration "
                            ++ show (CI.original (name mig))
            PG.withTransactionLevel PG.Serializable conn $ do
                case replaces mig of
                    [] -> do
                        -- This is a normal, non-history-rewriting,
                        -- migration.
                        b <- isApplied
                        if b
                        then
                            -- Nothing to do
                            pure ()
                        else do
                            runCommand
                            addMigration

                    rs -> do
                        -- This is a history-rewriting migration.
                        b1 <- isApplied
                        if b1
                        then ensureNoneExist rs
                        else do
                            b2 <- testRequiredExist rs
                            if b2
                            then deleteReplaced rs
                            else runCommand
                            addMigration

        where
            -- The fingerprint of the current migration.
            fingerprint :: Text
            fingerprint = makeFingerprint mig

            -- | Check to see if this migration has already been applied.
            --
            -- Throws AbortMigration if the migration has been applied,
            -- but has the wrong fingerprint.
            isApplied :: IO Bool
            isApplied = do
                r :: [ PG.Only Text ]
                    <- myQuery verbose conn [sql| SELECT fingerprint
                                            FROM schema_migrations
                                            WHERE name = ?
                                            LIMIT 1; |]
                            (PG.Only (CI.foldedCase (name mig)))
                case r of
                    []                   -> pure False
                    (PG.Only fprint) : _ ->
                        if (fprint == fingerprint)
                        then pure True
                        else do
                            putStrLn $
                                "Error: fingerprint of the migration "
                                ++ show (CI.original (name mig))
                                ++ " does not match entry in database!"
                            throw AbortMigration

            -- | Run this migration's command
            runCommand :: IO ()
            runCommand = do
                _ <- myExecute_ verbose conn (command mig)
                pure ()

            -- | Add this migration to the table
            addMigration :: IO ()
            addMigration = do
                _ <- myExecute verbose conn
                        [sql| INSERT INTO schema_migrations
                                (name, fingerprint) VALUES (?, ?); |]
                        (CI.foldedCase (name mig), fingerprint)
                pure ()

            -- | Make sure none of the replaced migrations exist
            --
            -- Except.throwError if any do exist.
            ensureNoneExist :: [ Replaces ] -> IO ()
            ensureNoneExist rs = do
                r :: [ PG.Only Text ]
                    <- myQuery verbose conn
                        [sql| SELECT name
                                FROM schema_migrations
                                WHERE name IN ?; |]
                        (PG.Only . PG.In $ CI.foldedCase . rName <$> rs)
                case r of
                    []  -> pure ()
                    res -> do
                        let formatName :: PG.Only Text -> IO ()
                            formatName (PG.Only nm) =
                                putStrLn $ "    " ++ show nm
                        putStrLn "Error: the following migrations are\
                                    \ still in the table despite being"
                        putStrLn $ "replaced by the "
                                        ++ show (CI.original (name mig))
                                        ++ " migration:"
                        mapM_ formatName res
                        throw AbortMigration

            -- | Test to see if all the required replaced migrations exist.
            --
            -- Returns True if all the required replaced migrations exist.
            -- Returns False if none of them exist.
            -- Throws AbortMigration if some exist and some do not, or
            -- if any of the replaced migrations don't match their
            -- fingerprints.
            testRequiredExist :: [ Replaces ] -> IO Bool
            testRequiredExist reps = do
                    fps :: [ (Text, Text) ]
                        <- myQuery verbose conn
                            [sql| SELECT (name, fingerprint)
                                    FROM schema_migrations
                                    WHERE name IN ?; |]
                            (PG.Only . PG.In $ CI.foldedCase . rName <$> reps)
                    case fps of
                        -- None of the replaced migrations exist.
                        [] -> pure False
                        _  -> do
                            let fpmap :: Map Text Text
                                fpmap = Map.fromList fps
                            mapM_ (doCheck fpmap) reps
                            pure True

            doCheck :: Map Text Text -> Replaces -> IO ()
            doCheck fpmap repl = do
                let repname :: Text
                    repname = CI.foldedCase (rName repl)
                case Map.lookup repname fpmap of
                    Nothing ->
                        case (rOptional repl) of
                            Optional -> pure ()
                            Required -> do
                                putStrLn $
                                    "Error: Migration "
                                    ++ show (CI.original (name mig))
                                    ++ " is replacing migration "
                                    ++ show (CI.original (rName repl))
                                    ++ " which does not exist"
                                    ++ " (despite other replaced migrations"
                                    ++ " existing)"
                                throw AbortMigration
                    Just fprint
                        | fprint == (rFingerprint repl) -> pure ()
                        | otherwise                     -> do
                            putStrLn $
                                "Error: Migration "
                                ++ show (CI.original (name mig))
                                ++ " is replacing migration "
                                ++ show (CI.original (rName repl))
                                ++ " but fingerprints do not match,"
                            throw AbortMigration

            -- | Delete all the migrations being replaced from the table.
            deleteReplaced :: [ Replaces ] -> IO ()
            deleteReplaced repls =
                myExecute verbose conn [sql| DELETE FROM schema_migrations
                                        WHERE name IN ? ; |]
                    (PG.Only . PG.In $ CI.foldedCase . rName <$> repls)


    logQuery :: (PG.ToRow q)
                    => Verbose
                    -> PG.Connection
                    -> PG.Query
                    -> q
                    -> IO ()
    logQuery verbose conn qry q =
        when (verbose >= High) $ do
            bs <- PG.formatQuery conn qry q
            putStrLn $ "Executing: " ++ show bs

    myQuery :: (PG.ToRow q, PG.FromRow r) =>
                Verbose -> PG.Connection -> PG.Query -> q -> IO [ r ]
    myQuery verbose conn qry q = do
        logQuery verbose conn qry q
        PG.query conn qry q

    myQuery_ :: (PG.FromRow r) =>
                Verbose -> PG.Connection -> PG.Query -> IO [ r ]
    myQuery_ verbose conn qry = do
        logQuery verbose conn qry ()
        PG.query_ conn qry

    myExecute :: (PG.ToRow q) =>
                Verbose -> PG.Connection -> PG.Query -> q -> IO ()
    myExecute verbose conn qry q = do
        logQuery verbose conn qry q
        _ <- PG.execute conn qry q
        pure ()

    myExecute_ :: Verbose -> PG.Connection -> PG.Query -> IO ()
    myExecute_ verbose conn qry = do
        logQuery verbose conn qry ()
        _ <- PG.execute_ conn qry
        pure ()

