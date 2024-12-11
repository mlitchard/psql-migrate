{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    import qualified Data.Foldable      as Foldable
    import qualified Data.List          as List
    import           Data.List.NonEmpty (NonEmpty(..))
    import           Data.String
    import           Data.Text          (Text)

    import Database.PostgreSQL.Simple.Migrate.Internal.Types

    -- | The possible errors that
    -- `Database.PostgreSQL.Simple.Migrate.apply` or 
    -- `Database.PostgreSQL.Simple.Migrate.check`
    --   can return.
    --
    -- You can use `formatMigrationsError` to convert this type
    -- into a human-readable string.
    data MigrationsError =
    
        -- | Two (or more) migrations have the same name.
        DuplicateMigrationName Migration Migration

        -- | A migration lists the same dependency more than once.
        | DuplicateDependency Migration Text

        -- | A migration has a dependency that isn't in the list.
        | UnknownDependency Migration Text

        -- | A required migration depends upon an optional migration.
        | RequiredDependsOnOptional Migration Migration

        -- | A set of dependencies forms a cycle.
        | CircularDependency (NonEmpty Migration)

        -- | Migration depends upon a migration in a later phase.
        | LaterPhaseDependency Migration Migration

        -- | All replacement migrations are optional
        | NoRequiredReplacement Migration

        -- | The same migration is listed multiple times in replaces
        | DuplicateReplaces Migration Text

        -- | Replaced migration still exists
        | ReplacedStillExists Migration Migration

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
        rnf (DuplicateMigrationName x y)    = rnf x `seq` rnf y `seq` ()
        rnf (DuplicateDependency x y)       = rnf x `seq` rnf y `seq` ()
        rnf (UnknownDependency x y)         = rnf x `seq` rnf y `seq` ()
        rnf (RequiredDependsOnOptional x y) = rnf x `seq` rnf y `seq` ()
        rnf (CircularDependency xs)         = rnf xs `seq` ()
        rnf (LaterPhaseDependency x y)      = rnf x `seq` rnf y `seq` ()
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
    formatMigrationsError (DuplicateMigrationName mig dep) =
        "Duplicate migration names between "
            ++ showMigration mig
            ++ " and "
            ++ showMigration dep
    formatMigrationsError (DuplicateDependency mig depName) =
        "Duplicate dependency "
        ++ show depName
        ++ " in migration "
        ++ showMigration mig
    formatMigrationsError (UnknownDependency mig depName) =
        "Unknown dependency "
        ++ show depName
        ++ " in migration "
        ++ showMigration mig
    formatMigrationsError (RequiredDependsOnOptional req opt) =
        "Required migration "
        ++ showMigration req
        ++ " depends on optional migration "
        ++ showMigration opt
    formatMigrationsError (CircularDependency (mig :| [])) =
        "The migration "
        ++ showMigration mig
        ++ " depends upon itself."
    formatMigrationsError (CircularDependency (mig :| migs)) =
        "Circular dependency: "
        ++ (List.intercalate ", " (showMigration <$> (mig : migs)))
    formatMigrationsError (LaterPhaseDependency mig dep) =
        "Phase violation: migration "
        ++ showMigration mig 
        ++ " (phase " ++ show (phase mig) ++ ")"
        ++ " can not depend upon migration "
        ++ showMigration dep
        ++ " (phase " ++ show (phase dep) ++ ")"
        ++ ", which is in a later phase."
    formatMigrationsError (NoRequiredReplacement mig) =
        "The migration "
        ++ showMigration mig
        ++ " has no required replacements (at least one is needed)."
    formatMigrationsError (DuplicateReplaces mig replName) =
        "Duplicate replaced name " 
        ++ show replName
        ++ " in migration "
        ++ showMigration mig
    formatMigrationsError (ReplacedStillExists mig repl) =
        "Migration "
        ++ showMigration mig
        ++ " replaces migration "
        ++ showMigration repl
        ++ " but the latter still exists."
    formatMigrationsError  EmptyMigrationList =
        "Empty migrations list"
    formatMigrationsError (UnknownMigrations nms) =
        "The following migrations are listed in the database as having\
        \ been applied, but are not in the list of migrations given to us:"
        <> (mconcat ((\s -> "\n    " <> show s) <$> nms))
        <>  "\n(Are you applying the wrong list of migrations to the\
                \ wrong database?)"
    formatMigrationsError (FingerprintMismatch nm) =
        "The migration "
        <> show nm
        <> " does not match the fingerprint in the database."
    formatMigrationsError (ReplacedMigrationsExist par mig) =
        "The migration " <> show mig
        <> " is replaced by the migration " <> show par
        <> " but still exists in the main migration list."
    formatMigrationsError (ReplacedFingerprint nm repl) =
        "Migration " <> show repl <> " which is being replaced by the\
        \ migration " <> show nm <> " does not match the fingerprint\
        \ in the database."
    formatMigrationsError (RequiredReplacementMissing nm repl) =
        "Migration " <> show nm <> " is missing required replacement "
        <> show repl <> " while other replaced migrations exist."
    formatMigrationsError Uninitialized =
        "Database is uninitialized (no migrations have ever been applied)."
    formatMigrationsError (MissingRequireds reqs) =
        "Required migrations not applied: "
        <> mconcat ((\s -> "\n    " <> show s) <$> reqs)



