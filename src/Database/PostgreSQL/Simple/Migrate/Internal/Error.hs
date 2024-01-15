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
    import           Data.List.NonEmpty (NonEmpty)
    import           Data.String
    import           Data.Text          (Text)

    -- | The possible errors that
    -- `Database.PostgreSQL.Simple.Migrate.apply` or 
    -- `Database.PostgreSQL.Simple.Migrate.check`
    --   can return.
    --
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

    tShow :: forall s a . (IsString s, Show a) => a -> s
    tShow = fromString . show

    -- | Convert an `MigrationsError` into a human-readable string.
    formatMigrationsError :: forall s .
                                (IsString s
                                , Monoid s)
                                => MigrationsError
                                -> s
    formatMigrationsError (DuplicateMigrationName nm) =
        "Two or more migrations share the name " <> tShow nm
    formatMigrationsError (DuplicateDependency migName depName) =
        "The migration " <> tShow migName
            <> " has duplicate dependencies " <> tShow depName
    formatMigrationsError (UnknownDependency migName depName) =
        "The migration " <> tShow migName
            <> " has a dependency " <> tShow depName <> " not in the list."
    formatMigrationsError (RequiredDependsOnOptional
                                    reqMigName optMigName) =
        "The required migration " <> tShow reqMigName
            <> " depends upon the optional migration " <> tShow optMigName
    formatMigrationsError (CircularDependency migs) =
        "The following set of migrations form a dependency cycle: "
        <> tShow migs
    formatMigrationsError (LaterPhaseDependency
                                    earlierMig earlierPhase
                                    laterMig laterPhase) =
        "The migration " <> tShow earlierMig
            <> " in phase " <> tShow earlierPhase
            <> " dependends upon the migration " <> tShow laterMig
            <> " in phase " <> tShow laterPhase
    formatMigrationsError (NoRequiredReplacement migName) =
        "The migration " <> tShow migName <> " replaces other migrations,"
            <> " but has no required replacements"
    formatMigrationsError (DuplicateReplaces migName replName) =
        "The migration " <> tShow migName <> " lists the migration "
            <> tShow replName <> " in it's replaces list multiple times."
    formatMigrationsError (ReplacedStillExists migName replacedName) =
        "The migration " <> tShow migName
        <> " says that it replaces the migration " <> tShow replacedName
        <> ", but that migration still exists in the list."
    formatMigrationsError  EmptyMigrationList =
        "Empty migrations list"
    formatMigrationsError (UnknownMigrations nms) =
        "The following migrations are listed in the database as having\
        \ been applied, but are not in the list of migrations given to us:"
        <> (mconcat ((\s -> "\n    " <> tShow s) <$> nms))
        <>  "\n(Are you applying the wrong list of migrations to the\
                \ wrong database?)"
    formatMigrationsError (FingerprintMismatch nm) =
        "The migration "
        <> tShow nm
        <> " does not match the fingerprint in the database."
    formatMigrationsError (ReplacedMigrationsExist par mig) =
        "The migration " <> tShow mig
        <> " is replaced by the migration " <> tShow par
        <> " but still exists in the main migration list."
    formatMigrationsError (ReplacedFingerprint nm repl) =
        "Migration " <> tShow repl <> " which is being replaced by the\
        \ migration " <> tShow nm <> " does not match the fingerprint\
        \ in the database."
    formatMigrationsError (RequiredReplacementMissing nm repl) =
        "Migration " <> tShow nm <> " is missing required replacement "
        <> tShow repl <> " while other replaced migrations exist."
    formatMigrationsError Uninitialized =
        "Database is uninitialized (no migrations have ever been applied)."
    formatMigrationsError (MissingRequireds reqs) =
        "Required migrations not applied: "
        <> mconcat ((\s -> "\n    " <> tShow s) <$> reqs)



