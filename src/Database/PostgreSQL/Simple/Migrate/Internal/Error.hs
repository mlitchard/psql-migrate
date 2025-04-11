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
    import           Control.Exception  (Exception)
    import          Data.Kind          (Type)
    import qualified Data.List          as List
    import           Data.List.NonEmpty (NonEmpty (..))
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
    type MigrationsError :: Type
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
        | ReplacedStillInList Migration Migration

        -- | No migrations in the list.
        | EmptyMigrationList

        -- | The given name exists multiple times as applied in the
        -- database.
        | DuplicateExisting Text

        -- | There were migrations in the database not in the list.
        | UnknownMigrations [ Text ]

        -- | The fingerprint of the given migration didn't match what
        -- was in the database.
        | FingerprintMismatch Migration Text

        -- | The given migration has been applied to the database, but
        -- one of the migrations it replaces still exist.
        | ReplacedStillInDB Migration Text

        -- | The fingerprint of a replaced migration doesn't match what
        -- is in the database.
        | ReplacedFingerprint Migration Text

        -- | A required replacement is missing, when one or more other
        -- replacements exist.
        | RequiredReplacementMissing Migration Text

        -- | The advisory lock is locked.
        | Locked

        -- | The database is not initialized,
        -- `Database.PostgreSQL.Simple.Migrate.Internal.Apply.apply` has
        -- never been called.
        --
        -- Only used by
        -- `Database.PostgreSQL.Simple.Migrate.check`.
        --
        | Uninitialized

        -- | A required migration has not been applied to the database.
        --
        -- Only used by
        -- `Database.PostgreSQL.Simple.Migrate.check`.
        --
        | RequiredUnapplied Migration
        deriving stock (Show, Read, Ord, Eq)

    instance Exception MigrationsError where

    instance NFData MigrationsError where
        rnf (DuplicateMigrationName x y)     = rnf x `seq` rnf y
        rnf (DuplicateDependency x y)        = rnf x `seq` rnf y
        rnf (UnknownDependency x y)          = rnf x `seq` rnf y
        rnf (RequiredDependsOnOptional x y)  = rnf x `seq` rnf y
        rnf (CircularDependency xs)          = rnf xs
        rnf (LaterPhaseDependency x y)       = rnf x `seq` rnf y
        rnf (NoRequiredReplacement t)        = rnf t
        rnf (DuplicateReplaces x y)          = rnf x `seq` rnf y
        rnf (ReplacedStillInList t u)        = rnf t `seq` rnf u
        rnf EmptyMigrationList               = ()
        rnf (DuplicateExisting x)            = rnf x
        rnf (UnknownMigrations xs)           = rnf xs
        rnf (FingerprintMismatch x y)        = rnf x `seq` rnf y
        rnf (ReplacedStillInDB x y)          = rnf x `seq` rnf y
        rnf (ReplacedFingerprint x y)        = rnf x `seq` rnf y
        rnf (RequiredReplacementMissing x y) = rnf x `seq` rnf y
        rnf Locked                           = ()
        rnf Uninitialized                    = ()
        rnf (RequiredUnapplied x)            = rnf x

    -- | Convert an `MigrationsError` into a human-readable string.
    formatMigrationsError :: IsString s => MigrationsError -> s
    formatMigrationsError (DuplicateMigrationName mig dep) = fromString $
        "Duplicate migration names between "
            ++ showMigration mig
            ++ " and "
            ++ showMigration dep
    formatMigrationsError (DuplicateDependency mig depName) = fromString $
        "Duplicate dependency "
        ++ show depName
        ++ " in migration "
        ++ showMigration mig
    formatMigrationsError (UnknownDependency mig depName) = fromString $
        "Unknown dependency "
        ++ show depName
        ++ " in migration "
        ++ showMigration mig
    formatMigrationsError (RequiredDependsOnOptional req opt) = fromString $
        "Required migration "
        ++ showMigration req
        ++ " depends on optional migration "
        ++ showMigration opt
    formatMigrationsError (CircularDependency (mig :| [])) = fromString $
        "The migration "
        ++ showMigration mig
        ++ " depends upon itself."
    formatMigrationsError (CircularDependency (mig :| migs)) = fromString $
        "Circular dependency: "
        ++ List.intercalate ", " (showMigration <$> (mig : migs))
    formatMigrationsError (LaterPhaseDependency mig dep) = fromString $
        "Phase violation: migration "
        ++ showMigration mig 
        ++ " (phase " ++ show (phase mig) ++ ")"
        ++ " can not depend upon migration "
        ++ showMigration dep
        ++ " (phase " ++ show (phase dep) ++ ")"
        ++ ", which is in a later phase."
    formatMigrationsError (NoRequiredReplacement mig) = fromString $
        "The migration "
        ++ showMigration mig
        ++ " has no required replacements (at least one is needed)."
    formatMigrationsError (DuplicateReplaces mig dupName) = fromString $
        "The migration " ++ showMigration mig
        ++ " has multiple replaces with the name " ++ show dupName
    formatMigrationsError (ReplacedStillInList mig repl) = fromString $
        "Migration "
        ++ showMigration mig
        ++ " replaces migration "
        ++ showMigration repl
        ++ " but the latter still exists."
    formatMigrationsError  EmptyMigrationList =
        fromString "Empty migrations list"
    formatMigrationsError (DuplicateExisting migname) = fromString $
        "Multiple instances of migration name " ++ show migname
        ++ " in database table."
    formatMigrationsError (UnknownMigrations nms) = fromString $
        "The following migrations are listed in the database as having"
        ++ " been applied, but are not in the list of migrations given to us:"
        ++ mconcat ((\s -> "\n    " <> show s) <$> nms)
        <>  "\n(Are you applying the wrong list of migrations to the wrong database?)"

    formatMigrationsError (FingerprintMismatch mig dbfp) = fromString $
        "The migration "
        ++ showMigration mig
        ++ " does not match the fingerprint in the database"
        ++ "(database=" ++ show dbfp
        ++ ", list=" ++ show (fingerprint mig)
        ++ ")"
    formatMigrationsError (ReplacedStillInDB mig rep) = fromString $
        "The migration " ++ show rep
        ++ " is replaced by the migration " ++ showMigration mig
        ++ " but still exists in the database."
    formatMigrationsError (ReplacedFingerprint mig repl) = fromString $
        "Migration "
        ++ show repl
        ++ " which is being replaced by the migration "
        ++ showMigration mig
        ++ " does not match the fingerprint in the database."
    formatMigrationsError (RequiredReplacementMissing nm repl) = fromString $
        "Migration " <> show nm <> " is missing required replacement "
        <> show repl <> " while other replaced migrations exist."
    formatMigrationsError Locked = fromString $
        "The database advisory locked is already locked (another migration"
        ++  " is ongoing?)."
    formatMigrationsError Uninitialized =
        fromString "The database is uninitialized."
    formatMigrationsError (RequiredUnapplied mig) = fromString $
        "The migration " ++ showMigration mig
        ++ " is required but not applied to the database."
