-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Error
-- Description : The combined error type.
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
module Database.PostgreSQL.Simple.Migrate.Internal.Error(
    MigrationsError(..),
    formatMigrationsError
) where

    import           Control.DeepSeq
    import           Data.List.NonEmpty (NonEmpty)
    import           Data.Text          (Text)

    -- | The possible errors
    -- `Database.PostgreSQL.Simple.Migrate.Internal.Order.checkMigrations`,
    -- `Database.PostgreSQL.Simple.Migrate.Internal.Apply.apply`, or 
    -- `Database.PostgreSQL.Simple.Migrate.Internal.Apply.check`
    --   can return.
    --
    -- This makes unit testing a heck of a lot easier and less brittle.
    -- You can use `formatMigrationsError` to convert this type
    -- into a human-readable string.
    data MigrationsError =
    
        -- | Two (or more) migrations have the same name.
        DuplicateMigrationName Text

        -- | A migration lists the same dependency more than once.
        | DuplicateDependency Text Text

        -- | A migration has a dependency that isn't in the list.
        | UnknownDependency Text Text

        -- | A required migration depends upon an optional migration.
        | RequiredDependsOnOptional Text Text

        -- | A set of dependencies forms a cycle.
        | CircularDependency (NonEmpty Text)

        -- | Migration depends upon a migration in a later phase.
        | LaterPhaseDependency Text Int Text Int

        -- | All replacement migrations are optional
        | NoRequiredReplacement Text

        -- | The same migration is listed multiple times in replaces
        | DuplicateReplaces Text Text

        -- | Replaced migration still exists
        | ReplacedStillExists Text Text

        -- | No migrations in the list.
        | EmptyMigrationList

        -- | There were migrations in the database not in the list.
        | UnknownMigrations [ Text ]

        -- | The fingerprint of the given migration didn't match what
        -- was in the database.
        | FingerprintMismatch Text

        -- | The given migration has been applied to the database, but
        -- one of the migrations it replaces still exist.
        | ReplacedMigrationsExist Text Text

        -- | The fingerprint of a replaced migration doesn't match what
        -- is in the database.
        | ReplacedFingerprint Text Text

        -- | A required replacement is missing, when one or more other
        -- replacements exist.
        | RequiredReplacementMissing Text Text

        -- | The database is not initialized,
        -- `Database.PostgreSQL.Simple.Migrate.Internal.Apply.apply` has
        -- never been called.
        --
        -- Only used by
        -- `Database.PostgreSQL.Simple.Migrate.Internal.Apply.check`.
        --
        | Uninitialized

        -- | There are unapplied but required migrations. 
        --
        -- Only used by
        -- `Database.PostgreSQL.Simple.Migrate.Internal.Apply.check`.
        --
        | MissingRequireds [ Text ]
        
        deriving (Show, Read, Ord, Eq)

    instance NFData MigrationsError where
        rnf (DuplicateMigrationName x)      = rnf x `seq` ()
        rnf (DuplicateDependency x y)       = rnf x `seq` rnf y `seq` ()
        rnf (UnknownDependency x y)         = rnf x `seq` rnf y `seq` ()
        rnf (RequiredDependsOnOptional x y) = rnf x `seq` rnf y `seq` ()
        rnf (CircularDependency xs)         = rnf xs `seq` ()
        rnf (LaterPhaseDependency x i y j)  = rnf x `seq` rnf i
                                                `seq` rnf y `seq` rnf j
                                                `seq` ()
        rnf (NoRequiredReplacement t)       = rnf t
        rnf (DuplicateReplaces x y)         = rnf x `seq` rnf y `seq` ()
        rnf (ReplacedStillExists t u)       = rnf t `seq` rnf u `seq` ()
        rnf EmptyMigrationList              = ()
        rnf (UnknownMigrations xs)          = rnf xs `seq` ()
        rnf (FingerprintMismatch x)         = rnf x `seq` ()
        rnf (ReplacedMigrationsExist x y)   = rnf x `seq` rnf y `seq` ()
        rnf (ReplacedFingerprint x y)       = rnf x `seq` rnf y `seq` ()
        rnf (RequiredReplacementMissing x y) = rnf x `seq` rnf y `seq` ()
        rnf Uninitialized                   = ()
        rnf (MissingRequireds xs)           = rnf xs `seq` ()

    -- | Convert an `MigrationsError` into a human-readable string.
    formatMigrationsError :: MigrationsError -> String
    formatMigrationsError (DuplicateMigrationName nm) =
        "Two or more migrations share the name " ++ show nm
    formatMigrationsError (DuplicateDependency migName depName) =
        "The migration " ++ show migName
            ++ " has duplicate dependencies " ++ show depName
    formatMigrationsError (UnknownDependency migName depName) =
        "The migration " ++ show migName
            ++ " has a dependency " ++ show depName ++ " not in the list."
    formatMigrationsError (RequiredDependsOnOptional
                                    reqMigName optMigName) =
        "The required migration " ++ show reqMigName
            ++ " depends upon the optional migration " ++ show optMigName
    formatMigrationsError (CircularDependency migs) =
        "The following set of migrations form a dependency cycle: "
        ++ show migs
    formatMigrationsError (LaterPhaseDependency
                                    earlierMig earlierPhase
                                    laterMig laterPhase) =
        "The migration " ++ show earlierMig
            ++ " in phase " ++ show earlierPhase
            ++ " dependends upon the migration " ++ show laterMig
            ++ " in phase " ++ show laterPhase
    formatMigrationsError (NoRequiredReplacement migName) =
        "The migration " ++ show migName ++ " replaces other migrations,"
            ++ " but has no required replacements"
    formatMigrationsError (DuplicateReplaces migName replName) =
        "The migration " ++ show migName ++ " lists the migration "
            ++ show replName ++ " in it's replaces list multiple times."
    formatMigrationsError (ReplacedStillExists migName replacedName) =
        "The migration " ++ show migName
        ++ " says that it replaces the migration " ++ show replacedName
        ++ ", but that migration still exists in the list."
    formatMigrationsError  EmptyMigrationList =
        "Empty migrations list"
    formatMigrationsError (UnknownMigrations nms) =
        "The following migrations are listed in the database as having\
        \ been applied, but are not in the list of migrations given to us:"
        ++ (mconcat ((\s -> "\n    " ++ show s) <$> nms))
        ++  "\n(Are you applying the wrong list of migrations to the\
                \ wrong database?)"
    formatMigrationsError (FingerprintMismatch nm) =
        "The migration "
        ++ show nm
        ++ " does not match the fingerprint in the database."
    formatMigrationsError (ReplacedMigrationsExist par mig) =
        "The migration " ++ show mig
        ++ " is replaced by the migration " ++ show par
        ++ " but still exists in the main migration list."
    formatMigrationsError (ReplacedFingerprint nm repl) =
        "Migration " ++ show repl ++ " which is being replaced by the\
        \ migration " ++ show nm ++ " does not match the fingerprint\
        \ in the database."
    formatMigrationsError (RequiredReplacementMissing nm repl) =
        "Migration " ++ show nm ++ " is missing required replacement "
        ++ show repl ++ " while other replaced migrations exist."
    formatMigrationsError Uninitialized =
        "Database is uninitialized (no migrations have ever been applied)."
    formatMigrationsError (MissingRequireds reqs) =
        "Required migrations not applied: "
        ++ mconcat ((\s -> "\n    " ++ show s) <$> reqs)



