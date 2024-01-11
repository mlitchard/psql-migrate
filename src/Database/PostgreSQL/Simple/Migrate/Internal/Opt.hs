{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Opt
-- Description : Functions for handing optional migrations
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
module Database.PostgreSQL.Simple.Migrate.Internal.Opt (
    migrationIsApplied,
    appliedOptionalMigrations
) where

    import qualified Data.CaseInsensitive             as CI
    import qualified Data.Set                         as Set
    import           Data.Text                        (Text)
    import qualified Database.PostgreSQL.Simple       as PG
    import           Database.PostgreSQL.Simple.SqlQQ
    import qualified Database.PostgreSQL.Simple.Types as PG
    
    -- These screw up the formatting, so they get their own,
    -- unformatted, block.
    import Database.PostgreSQL.Simple.Migrate.Internal.Types

    -- | Check if a specific migration has been applied.
    --
    -- The main use for this function is to help deal with optional
    -- migrations.  The idea is that the program can behave differently
    -- depending upon whether the transaction exists or not.
    --
    -- If the migration is marked as Required, then this function does
    -- nothing and returns True (it is assumed that all required migrations
    -- have been applied).  Use
    -- `Database.PostgreSQL.Simple.Migrate.Internal.Apply.check` to ensure
    -- all required migrations have been applied.
    --
    -- Both the name and the fingerprint (hash of the command) must
    -- match for the migration to be considered applied.
    --
    -- To test if any of several migrations have been applied, consider
    -- using `appliedOptionalMigrations`, which is significantly more
    -- efficient than repeatedly calling this function.
    --
    -- Note that the result of this function is only valid during
    -- the transaction it is called in.  Calling it when not in a
    -- transaction will \"work\"- PostgreSQL will wrap the function
    -- in a transaction.  But the schema could update a single
    -- nanosecond later.
    --
    -- The correct way to use this function is to start a transaction,
    -- call this function, and then do all actions that depend upon
    -- the result in the same transaction.
    migrationIsApplied :: PG.Connection
                            -> Migration
                            -> IO Bool
    migrationIsApplied conn mig 
        | optional mig == Required = pure True
        | otherwise                = do
            res :: [ PG.Only Bool ] <- 
                PG.query conn
                    [sql|
                        SELECT EXISTS (
                            SELECT 1 FROM schema_migrations
                                WHERE name = ?
                                AND fingerprint = ?
                                LIMIT 1);
                    |]
                    (CI.foldedCase (name mig), fingerprint mig)
            pure $
                case res of
                    []              -> False
                    (PG.Only x : _) -> x

    -- | Return the list of migrations that optional, but have been applied.
    --
    -- It behaves as if `migrationIsApplied` is called for each migration,
    -- and the migration is returned if that function returns true.  But
    -- it is significantly more efficient in that it only performs one
    -- query to the database.  All required migrations will thus also be
    -- returned.
    --
    -- Like `migrationIsApplied`, this function should only be called in
    -- a transaction.
    appliedOptionalMigrations :: PG.Connection
                                    -> [ Migration ]
                                    -> IO [ Migration ]
    appliedOptionalMigrations conn migs =
        case filter (\m -> optional m == Optional) migs of
            [] -> pure migs
            ms -> do
                res :: [ PG.Only Text ] <-
                    PG.query conn
                        [sql| SELECT DISTINCT src.name
                                FROM (?) AS src(name, fingerprint)
                                WHERE (src.name, src.fingerprint) IN
                                    (SELECT name, fingerprint
                                        FROM schema_migrations); |]
                        (PG.Only
                            (PG.Values [ "TEXT", "TEXT" ]
                                (fmap
                                    (\m -> (CI.foldedCase (name m),
                                                fingerprint m))
                                    ms)))
                case res of
                    [] -> pure []
                    rs ->
                        let s :: Set.Set Text
                            s = Set.fromList (fmap PG.fromOnly rs)

                            isMem :: Migration -> Bool
                            isMem m = (optional m == Required)
                                        || (Set.member
                                                (CI.foldedCase (name m)) 
                                                s)
                        in
                        pure $ filter isMem migs


