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
-- This is an internal module of "Database.PostgreSQL.Simple.Migrate",
-- you probably want that module instead.  Anything exported by this
-- module that is not also exported by the main module is subject
-- to change without notice.
--
module Database.PostgreSQL.Simple.Migrate.Internal.Apply (
    apply,
    applyBase,
    check,
    checkBase
) where

    import           Control.DeepSeq                        (force)
    import qualified Control.Exception                      as Exception
    import           Control.Monad                          (when)
    import qualified Data.CaseInsensitive                   as CI
    import           Data.Int                               (Int64)
    import           Data.Map.Strict                        (Map)
    import qualified Data.Map.Strict                        as Map
    import           Data.Text                              (Text)
    import qualified Database.PostgreSQL.Simple             as PG
    import           Database.PostgreSQL.Simple.SqlQQ
    import qualified Database.PostgreSQL.Simple.Transaction as PG

    -- These screw up the formatting, so they get their own,
    -- unformatted, block.
    import Database.PostgreSQL.Simple.Migrate.Internal.Error
    import Database.PostgreSQL.Simple.Migrate.Internal.Monad
    import Database.PostgreSQL.Simple.Migrate.Internal.Order
    import Database.PostgreSQL.Simple.Migrate.Internal.Types
    import Database.PostgreSQL.Simple.Migrate.Internal.Wrap

    -- | Apply a set of migrations to a database.
    --
    -- If the "Database.PostgreSQL.Simple" library throws an exception,
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
    apply verbose migs makeConn = do
        r <- applyBase verbose migs makeConn
        case r of
            Nothing  -> do
                putStrLn "Success."
                pure True
            Just err -> do
                wrap ("Error: " ++ formatMigrationsError err) >>= putStrLn
                putStrLn "Failure."
                pure False

    -- | Maybe print a line of output.
    mprint :: Verbose -> String -> M ()
    mprint lvl str = do
        v <- askM
        when (v >= lvl) $ liftM $ putStrLn str

    -- | Internal code for `apply`.
    --
    -- Returns a `MigrationsError` instead of printing out the error.  Useful
    -- for internal testing.
    applyBase :: Verbose
                    -> [ Migration ]
                    -> IO PG.Connection
                    -> IO (Maybe MigrationsError)
    applyBase _       []    _        = pure $ Just EmptyMigrationList
    applyBase verbose migs1 makeConn = do
            r :: Either MigrationsError () <- runM go verbose
            case r of
                Left err -> pure $ Just err
                Right () -> pure Nothing
        where
            go :: M ()
            go = do
                mprint Low "Ordering migrations."
                case checkMigrations migs1 of
                    Left err       -> throwM err
                    Right miggraph -> do
                        let migs2 :: [ Migration ]
                            migs2 = orderMigrations miggraph

                        -- Do *all* the ordering work before opening
                        -- the connection!
                        migs <- liftM . Exception.evaluate $ force migs2

                        -- Make the connection and apply the migrations
                        withConn makeConn (go2 migs)

            go2 :: [ Migration ] -> PG.Connection -> M ()
            go2 migs conn = do
                mprint Medium "Initializing database if necessary."
                withTransactionLevelM PG.Serializable conn $ do
                    b <- isInitialized conn
                    when (not b) $ initialize conn

                xmigs <- getExistingMigrations conn
                case checkExistingMigrations xmigs migs of
                    Left err     -> throwM err
                    Right needed ->
                        mapM_ (applyMigration conn) needed

    -- | Check that a set of migrations has been applied to a database.
    --
    -- This function is similar to `apply`, except that no migrations
    -- are applied.  Instead, the database is checked for correctness.
    -- Note that optional migrations need not have been applied.
    --
    -- If the "Database.PostgreSQL.Simple" library throws an exception,
    -- it is rethrown by this function.  Otherwise, if there is an
    -- error, the error printed and the application of migrations
    -- is halted.
    --
    check :: Verbose
                -- ^ What messages to print out

                -> [ Migration ]
                -- ^ The list of migrations to check.

                -> IO PG.Connection
                -- ^ How to create a database connection.
                --
                -- The connection is not made unless and until it is
                -- needed.  The create connection will be closed when
                -- the function exits (even if it exits by throwing an
                -- exception).

                -> IO Bool
                -- ^ True means the database is consistant with the
                -- set of migrations.
    check verbose migs makeConn = do
        r <- checkBase verbose migs makeConn
        case r of
            Nothing  -> pure True
            Just err -> do
                wrap ("Error: " ++ formatMigrationsError err) >>= putStrLn
                pure False

    -- | Internal code for `check`.
    --
    -- Returns a `MigrationsError` instead of printing out the error.  Useful
    -- for internal testing.
    checkBase :: Verbose
                    -> [ Migration ]
                    -> IO PG.Connection
                    -> IO (Maybe MigrationsError)
    checkBase _       []    _        = pure $ Just EmptyMigrationList
    checkBase verbose migs makeConn = do
            r <- runM go verbose
            case r of
                Left err -> pure $ Just err
                Right () -> pure Nothing
        where
            go :: M ()
            go = do
                mprint Low "Ordering migrations."
                case checkMigrations migs of
                    Left err -> throwM err
                    Right _  -> do
                        -- Make the connection and check the migrations
                        withConn makeConn go2

            go2 :: PG.Connection -> M ()
            go2 conn = do
                mprint Medium "Checking if database is initialized."
                b <- isInitialized conn
                if (not b)
                then throwM Uninitialized
                else do
                    xmigs <- getExistingMigrations conn
                    case checkExistingMigrations xmigs migs of
                        Left err     -> throwM err
                        Right needed ->
                            case requireds needed of
                                [] -> pure ()
                                xs -> throwM (MissingRequireds xs)

    withConn :: IO PG.Connection -> (PG.Connection -> M a) -> M a
    withConn makeConn act = do
            mprint High "Opening database connection."
            bracketM (liftM makeConn) doClose act
        where
            doClose :: PG.Connection -> M ()
            doClose conn = do
                mprint High "Closing database connection."
                liftM $ PG.close conn

    isInitialized :: PG.Connection -> M Bool
    isInitialized conn = do
        r1 :: [ PG.Only Bool ]
            <- myQuery_ conn
                [sql|   SELECT EXISTS (
                            SELECT 1
                            FROM pg_tables
                            WHERE tablename = 'schema_migrations'
                            LIMIT 1
                        ); |]
        pure $
            case r1 of
                (PG.Only True) : _ -> True
                _                  -> False

    initialize :: PG.Connection -> M ()
    initialize conn = do
        myExecute_ conn
            [sql|
                CREATE TABLE IF NOT EXISTS schema_migrations(
                    name TEXT PRIMARY KEY,
                    fingerprint CHAR(44) UNIQUE NOT NULL,
                    executed_at TIMESTAMP WITH TIME ZONE
                        NOT NULL DEFAULT NOW());
            |]
        myExecute_ conn
            [sql|
                CREATE INDEX ON schema_migrations(executed_at);
            |]

    getExistingMigrations :: PG.Connection -> M (Map Text Text)
    getExistingMigrations conn = do
        r :: [ (Text, Text) ]
            <- myQuery_ conn
                [sql| SELECT naem, fingerprint FROM schema_migrations; |]
        pure $ Map.fromList r

    requireds :: [ Migration ] -> [ Text ]
    requireds = fmap getName . filter isRequired
        where
                isRequired :: Migration -> Bool
                isRequired mig = optional mig == Required

                getName :: Migration -> Text
                getName = CI.original . name

    data CheckState =
        Unknown
        | OptionExists
        | RequiredMissing Text
        | RequiredExists

    checkExistingMigrations :: Map Text Text
                                -> [ Migration ]
                                -> Either MigrationsError [ Migration ]
    checkExistingMigrations xmigs migs1 = snd <$> runN (go [] migs1) xmigs
        where
            go :: [ Migration ] -> [ Migration ] -> N [ Migration ]
            go acc [] = do
                checkForUnknownMigrations
                pure $ reverse acc
            go acc (m:ms) = do
                b :: Bool <- checkMigration m
                if b
                then go (m : acc) ms
                else go acc ms

    checkForUnknownMigrations :: N ()
    checkForUnknownMigrations = do
        remain :: Map Text Text <- getN
        if (Map.null remain)
        then pure ()
        else throwN . UnknownMigrations $ Map.keys remain

    -- Returns True if the migration needs to be applied.
    checkMigration :: Migration -> N Bool
    checkMigration mig = do
        migApplied <- checkMigrationFingerprint mig
        case replaces mig of
            []               -> pure ()
            _
                | migApplied -> checkReplacesGone mig
                | otherwise  -> checkReplacedMigrations mig
        pure (not migApplied)

    -- Throws a FingerprintMigration error
    --
    -- Returns True if the migration has been applied.
    checkMigrationFingerprint :: Migration -> N Bool
    checkMigrationFingerprint mig = do
        migs :: Map Text Text <- getN
        let nm :: Text
            nm = CI.foldedCase (name mig)
        case Map.lookup nm migs of
            Nothing -> pure False
            Just fp
                | fp == fingerprint mig -> do
                    putN $ Map.delete nm migs
                    pure True
                | otherwise             ->
                    throwN $ FingerprintMismatch
                                (CI.original (name mig))

    -- Throws a ReplacedMigrationsExist error
    checkReplacesGone :: Migration -> N ()
    checkReplacesGone mig = mapM_ go (replaces mig)
        where
            go :: Replaces -> N ()
            go rep = do
                migs :: Map Text Text <- getN
                case Map.lookup (CI.foldedCase (rName rep)) migs of
                    Nothing -> pure ()
                    Just _  -> throwN $ ReplacedMigrationsExist
                                            (CI.original (name mig))
                                            (CI.original (rName rep))

    -- Throws a ReplacedMigrations error
    checkReplacedMigrations :: Migration -> N ()
    checkReplacedMigrations mig = loop Unknown (replaces mig)
        where
            loop :: CheckState -> [ Replaces ] -> N ()
            loop _ []       = pure ()
            loop s (r : rs) = do
                migs <- getN
                let nm :: Text
                    nm = CI.foldedCase (rName r)
                case Map.lookup nm migs of
                    Nothing -> missing r s rs
                    Just fp
                        | fp == rFingerprint r -> do
                            putN $ Map.delete nm migs
                            exists r s rs
                        | otherwise            ->
                            throwN $ ReplacedFingerprint
                                        (CI.original (name mig))
                                        (CI.original (rName r))

            -- The replacement is missing.
            missing :: Replaces -> CheckState -> [ Replaces ] -> N ()
            missing r
                | rOptional r == Optional = loop
                | otherwise               = missingRequired r

            -- A required replacement is missing
            missingRequired r Unknown               rs =
                loop (RequiredMissing (CI.original (rName r))) rs
            missingRequired r OptionExists          _  =
                throwN $ RequiredReplacementMissing
                                (CI.original (name mig))
                                (CI.original (rName r))
            missingRequired _ (RequiredMissing rnm) rs =
                loop (RequiredMissing rnm) rs
            missingRequired r RequiredExists        _  =
                throwN $ RequiredReplacementMissing
                                (CI.original (name mig))
                                (CI.original (rName r))

            -- The replacement exists.
            exists :: Replaces -> CheckState -> [ Replaces ] -> N ()
            exists r
                | rOptional r  == Optional = existsOptional
                | otherwise                = existsRequired

            -- An optional replaced migration exists.
            existsOptional :: CheckState
                                -> [ Replaces ]
                                -> N ()
            existsOptional Unknown               rs =
                loop OptionExists rs
            existsOptional OptionExists          rs =
                loop OptionExists rs
            existsOptional (RequiredMissing rnm) _  =
                throwN $ RequiredReplacementMissing
                            (CI.original (name mig))
                            rnm
            existsOptional RequiredExists        rs =
                loop RequiredExists rs

            -- A required replaced migration exists.
            existsRequired :: CheckState
                                -> [ Replaces ]
                                -> N ()
            existsRequired Unknown               rs =
                loop RequiredExists rs
            existsRequired OptionExists          rs =
                loop RequiredExists rs
            existsRequired (RequiredMissing rnm) _  =
                throwN $ RequiredReplacementMissing
                            (CI.original (name mig))
                            rnm
            existsRequired RequiredExists        rs =
                loop RequiredExists rs

    applyMigration :: PG.Connection
                        -> Migration
                        -> M ()
    applyMigration conn mig = 
        -- Note that we don't need to check for any errors, we can just
        -- plow ahead.
        withTransactionLevelM PG.Serializable conn $ do
            case replaces mig of
                [] -> myExecute_ conn (command mig)
                repls -> do
                    r <- myExecute conn
                            [sql| DELETE FROM schema_migrations
                                    WHERE name IN ?; |]
                            (PG.Only
                                (PG.In
                                    (fmap
                                        (CI.foldedCase . rName)
                                        repls)))
                    when (r == 0) $
                        myExecute_ conn (command mig)
            _ <- myExecute conn
                [sql| INSERT INTO schema_migrations
                        (name, fingerprint) VALUES (?, ?); |]
                (CI.foldedCase (name mig), fingerprint mig)
            pure ()


    logQuery :: (PG.ToRow q)
                    => PG.Connection
                    -> PG.Query
                    -> q
                    -> M ()
    logQuery conn qry q = do
        verbose <- askM
        liftM $
            when (verbose >= High) $ do
                bs <- PG.formatQuery conn qry q
                putStrLn $ "Executing: " ++ show bs

    myQuery_ :: (PG.FromRow r)
                => PG.Connection
                -> PG.Query
                -> M [ r ]
    myQuery_ conn qry = do
        logQuery conn qry ()
        liftM $ PG.query_ conn qry

    myExecute :: (PG.ToRow q)
                    => PG.Connection
                    -> PG.Query
                    -> q
                    -> M Int64
    myExecute conn qry q = do
        logQuery conn qry q
        liftM $ PG.execute conn qry q

    myExecute_ :: PG.Connection -> PG.Query -> M ()
    myExecute_ conn qry = do
        logQuery conn qry ()
        _ <- liftM $ PG.execute_ conn qry
        pure ()

