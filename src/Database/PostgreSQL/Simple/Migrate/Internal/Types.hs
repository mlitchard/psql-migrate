{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Types
-- Description : Types for the migration code.
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
module Database.PostgreSQL.Simple.Migrate.Internal.Types (
    Migration(..),
    makeMigration,
    Optional(..),
    Replaces(..),
    makeReplaces,
    addDependency,
    addDependencies,
    setOptional,
    setPhase,
    addReplaces,
    setReplacesOptional,
    Verbose(..)
) where

    import           Control.DeepSeq
    import           Data.CaseInsensitive             (CI)
    import qualified Data.CaseInsensitive             as CI
    import           Data.Text                        (Text)
    import           Database.PostgreSQL.Simple.Types (Query (..))
    import           GHC.Generics

    import Database.PostgreSQL.Simple.Migrate.Internal.Finger;

    -- | Boolean-analog for whether a migration is optional or required.
    --
    data Optional = Optional  
                        -- ^ The migration may not have been applied yet.
                    | Required
                        -- ^ The migration must have been applied.
        deriving (Show, Read, Ord, Eq, Generic, Bounded, Enum)

    instance NFData Optional where
        rnf = rwhnf



    -- | A single database schema migration.
    -- 
    -- It represents a single, atomic, change to the database schema
    -- to be applied transactionally.  The core is the command, which
    -- is a SQL script, generally one or more schema modification commands
    -- like CREATE TABLE.
    --
    -- You create a Migration with the
    -- `Database.PostgreSQL.Simple.Migrate.makeMigration` command, which
    -- fills in sensible default values for most of the fields, and
    -- then modify it with one or more of the modifier functions
    -- `Database.PostgreSQL.Simple.Migrate.addDependency`,
    -- `Database.PostgreSQL.Simple.Migrate.addDependencies`,
    -- `Database.PostgreSQL.Simple.Migrate.setOptional`,
    -- `Database.PostgreSQL.Simple.Migrate.setPhase`, and
    -- `Database.PostgreSQL.Simple.Migrate.addReplaces`.  For example:
    --
    -- @
    -- {-# LANGUAGE QuasiQuotes #-}
    --
    -- import Database.PostgreSQL.Simple.SqlQQ
    --
    -- migrations :: [ Migration ]
    -- migrations = [
    --      makeMigration "example-1"
    --          [sql| ... |]
    --          \`addDependency\` "some-other-migration"
    --          \`setPhase\` 2,
    --          \`setOptional\` Optional,
    --      ...
    -- ]
    -- @
    --
    data Migration = Migration {
        name :: CI Text,
        -- ^ The name of the migration.
        --
        -- Set by the `Database.PostgreSQL.Simple.Migrate.makeMigration`
        -- function.

        command :: Query,
        -- ^ The command to execute to perform the migration.  
        --
        -- Set by the `Database.PostgreSQL.Simple.Migrate.makeMigration`
        -- function.

        fingerprint :: Text,
        -- ^ The fingerprint (hash) of the command.
        --
        -- Set by the `Database.PostgreSQL.Simple.Migrate.makeMigration`
        -- function.

        dependencies :: [ CI Text ],
        -- ^ The list of the names of other migrations this migration
        -- depends upon.
        --
        -- Defaults to the empty list by the
        -- `Database.PostgreSQL.Simple.Migrate.makeMigration`
        -- function.  Can be changed with the
        -- `Database.PostgreSQL.Simple.Migrate.addDependency` or
        -- `Database.PostgreSQL.Simple.Migrate.addDependencies`
        -- functions.

        optional :: Optional,
        -- ^ Whether the migration is optional.
        --
        -- Defaults to `Required` by the
        -- `Database.PostgreSQL.Simple.Migrate.makeMigration`
        -- function.  Can be changed with the
        -- `Database.PostgreSQL.Simple.Migrate.setOptional`
        -- function.


        phase :: Int,
        -- ^ The phase to apply the migration in.
        --
        -- Defaults to 1 by the
        -- `Database.PostgreSQL.Simple.Migrate.makeMigration`
        -- function.  Can be changed with the
        -- `Database.PostgreSQL.Simple.Migrate.setPhase`
        -- function.
        
        replaces :: [ Replaces ]
        -- ^ The list of migrations this migration replaces.
        --
        -- Defaults to the empty list by the
        -- `Database.PostgreSQL.Simple.Migrate.makeMigration`
        -- function.  Can be changed with the
        -- `Database.PostgreSQL.Simple.Migrate.addReplaces`
        -- function.
        
    } deriving (Show, Read, Generic)

    -- | Eq on Migrations is defined by name only.
    instance Eq Migration where
        m1 == m2 = name m1 == name m2
        m1 /= m2 = name m1 /= name m2

    -- | Ord on Migrations is defined by name only.
    instance Ord Migration where
        compare m1 m2 = compare (name m1) (name m2)
        m1 < m2 = name m1 < name m2
        m1 > m2 = name m1 > name m2
        m1 <= m2 = name m1 <= name m2
        m1 >= m2 = name m1 >= name m2

    instance NFData Migration where
        rnf mig = rnf (name mig)
                    `seq` rnf (fromQuery (command mig))
                    `seq` rnf (optional mig)
                    `seq` rnf (dependencies mig)
                    `seq` rnf (phase mig)
                    `seq` rnf (replaces mig)
                    `seq` ()

    -- | Create a migration.
    --
    -- The newly created migration can then be modified with the
    -- `Database.PostgreSQL.Simple.Migrate.addDependency`,
    -- `Database.PostgreSQL.Simple.Migrate.addDependencies`,
    -- `Database.PostgreSQL.Simple.Migrate.setOptional`,
    -- `Database.PostgreSQL.Simple.Migrate.setPhase`, and
    -- `Database.PostgreSQL.Simple.Migrate.addReplaces` modifier
    -- functions.
    --
    -- These fields can be overridden by the `addDependency`,
    -- `addDependencies`, `setPhase`, `setOptional`, and
    -- `addReplaces` functions.
    --
    -- All migration names need to be unique.  It customary to add a
    -- version number on to the end of the name, for example
    -- \"example-mig-1\", \"example-mig-2\", etc.  This tells
    -- the human reader that these migrations are related.  It
    -- is an error for two migrations to have the same name.
    --
    -- Note that names are case INSENSITIVE, so "foo", "Foo", and
    -- "FOO" are all considered the same name.
    --
    -- The migration command is a `Database.PostgreSQL.Simple.Query`,
    -- and is passed to `Database.PostgreSQL.Simple.execute_` within
    -- a transaction (so multiple statements are allowed within the
    -- command)..
    --
    -- Note that no arguments are allowed (execute_ is called,
    -- not execute).
    --
    makeMigration :: Text
                        -- ^ Migration name
                        -> Query
                        -- ^ Migration command
                        -> Migration
    makeMigration nm cmd = Migration {
                                name         = CI.mk nm,
                                command      = cmd,
                                fingerprint  = makeFingerprint cmd,
                                optional     = Required,
                                phase        = 1,
                                dependencies = [],
                                replaces     = [] }


    -- | Data about a replaced migration.
    --
    -- This structure is created by the
    -- `Database.PostgreSQL.Simple.Migrate.makeReplaces` function,
    -- and can be modified by the 
    -- `Database.PostgreSQL.Simple.Migrate.setReplacesOptional` function.
    --
    data Replaces = Replaces {
                            rName        :: CI Text,
                            -- ^ Name of the migration being replaced.

                            rFingerprint :: Text,
                            -- ^ Fingerprint of the migration being replaced.

                            rOptional    :: Optional
                            -- ^ If the replaced migration is optional.
                            }
                            deriving (Show, Read, Ord, Eq, Generic)

    instance NFData Replaces where
        rnf rep = rnf (rName rep)
                    `seq` rnf (rFingerprint rep)
                    `seq` rnf (rOptional rep)
                    `seq` ()


    -- | Make a Replaces data structure.
    --
    -- The replaces structure can be made optional using
    -- `Database.PostgreSQL.Simple.Migrate.setReplacesOptional` function.
    --
    -- An example:
    --
    -- @
    --      makeMigration "example3" [sql| ... |]
    --          `addReplaces` [
    --              makeReplaces "example1"
    --                  \"9wbC-yXz6ISXeA-eFOn-l4DVoJZ4P8I79HJxJKHIBIg=\",
    --              makeReplaces "example2"
    --                  \"t_isz1eBAu8XM_e2idYdnCPvI-UFEpFC3s2RTyjArDA=\"
    --                  \`setReplacesOptional\` Optional
    --          ]
    -- @
    --
    --
    -- __WARNING__: The assumption is that the command, when executed,
    -- will result the same schema as the aggregate result of applying
    -- all the replaced migrations.  For obvious reasons this condition
    -- is not checked for.  Violating it, however, will result in tears
    -- and recriminations.  Use this feature at your own risk.
    --
    -- The following conditions are all errors:
    --
    -- * If a replaced migration exists but has a different fingerprint.
    --
    -- * If some required migrations exist, and others do not.
    --
    -- * If there are only optional replaced migrations.  Not replacing
    -- any migrations is fine, as is having just one required replaced
    -- migration.  But if you are replacing migrations, you have to
    -- have at least one required replaced migration (this is so we can
    -- affirmitively tell if the replacement is happening).
    --
    -- * If any of the replaced migrations are still in the list of
    -- migrations to be applied.
    --
    -- * If the same migration is listed multiple times.
    --
    -- * If the command does not create the same schema as the
    -- aggregate of all the replaced migrations.  Note that this
    -- error condition is not checked for!
    --
    makeReplaces :: Text
                        -- ^ The name of the migration being replaced.
                        -> Text
                        -- ^ The fingerprint of the migration being replaced.
                        -> Replaces
    makeReplaces nm fprint = Replaces {
                                rName        = CI.mk nm,
                                rFingerprint = fprint,
                                rOptional    = Required }

    -- | Add a dependency to a migration.
    --
    -- If you're adding multiple dependencies, consider using 
    -- `Database.PostgreSQL.Simple.Migrate.addDependencies` instead.
    --
    -- The following conditions are all errors:
    --
    -- * For a migration to depend upon a migration in a later phase
    -- (the same phase is OK, just not later).
    --
    -- * For any sequence of migrations to form a cycle (i.e. 
    -- A depends upon B, which depends upon C, which depends upon A).
    --
    -- * For a migration to depend upon an unknown dependency.
    --
    -- * For the same migration to appear multiple times in the list.
    -- Note that names are compared case INSENSITIVE, so having
    -- \"foo\" and \"Foo\" both is an error.  
    --
    -- * For a migration marked as Required to depend upon a
    -- migration marked as Optional.
    --
    addDependency :: Migration -> Text -> Migration
    addDependency mig dep =
        mig { dependencies = CI.mk dep : dependencies mig }

    -- | Add a list of dependencies to a migration.
    --
    -- See `Database.PostgreSQL.Simple.Migrate.addDependency` for
    -- more details.
    addDependencies :: Migration -> [ Text ] -> Migration
    addDependencies mig deps =
        mig { dependencies = (CI.mk <$> deps) ++ dependencies mig }

    -- | Set the optional field of a migration.
    --
    setOptional :: Migration -> Optional -> Migration
    setOptional mig opt = mig { optional = opt }

    -- | Set the phase field of a migration.
    --
    -- All migrations of phase N will be applied before any
    -- migrations of phase N+1 are applied.
    --
    -- Note that this only covers the migrations being applied
    -- currently- it is possible that a previous execution
    -- applied a migration of a later phase, than the migration
    -- currently being applied.
    --
    -- 0, -1, etc., are valid phases.  Not all phases need to
    -- have migrations.  So, for example,  you can have migrations
    -- in phases -1, 0, 1, and 100, and that's OK- phases 2 through
    -- 99 just don't have any migrations in them.
    --
    setPhase :: Migration -> Int -> Migration
    setPhase mig phz = mig { phase = phz }

    -- | Add a list of replaced migrations to a migration.
    --
    addReplaces :: Migration -> [ Replaces ] -> Migration
    addReplaces mig repls = mig { replaces = repls ++ replaces mig }

    -- | Set the optional field of a `Replaces` structure.
    --
    setReplacesOptional :: Replaces -> Optional -> Replaces
    setReplacesOptional repl opt = repl { rOptional = opt }

    -- | The verbosity level.
    --
    -- Passed to the logging function given to 
    -- `Database.PostgreSQL.Simple.Migrate.Internal.Apply.apply` and
    -- `Database.PostgreSQL.Simple.Migrate.Internal.Apply.check`.
    -- Can be used to control much detail to print out or log.
    --
    data Verbose =
            -- | Error messages and top level messages.
            Low

            -- | Phase change messages.
            | Medium

            -- | Which migrations we are applying
            | High

            -- | What database queries we are performing.
            --
            -- Also opening and closing database connections.
            | Detail
        deriving (Show, Read, Ord, Eq, Enum, Bounded)


