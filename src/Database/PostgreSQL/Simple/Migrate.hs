
module Database.PostgreSQL.Simple.Migrate (
    -- * Migration type
    Migration(..),
    makeMigration,
    Replaces,
    makeReplaces
) where

    import Database.PostgreSQL.Simple.Migrate.Internal.Types
