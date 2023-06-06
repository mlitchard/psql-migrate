
module Database.PostgreSQL.Simple.Migrate (
    -- * Migration type
    Migration(..),
    Optional(..),
    makeMigration,
    Replaces(..),
    makeReplaces,
    apply,
    makeFingerprint
) where

    import Database.PostgreSQL.Simple.Migrate.Internal.Apply
    import Database.PostgreSQL.Simple.Migrate.Internal.Finger
    import Database.PostgreSQL.Simple.Migrate.Internal.Types
