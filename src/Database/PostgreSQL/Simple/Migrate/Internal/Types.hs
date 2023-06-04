{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Types
-- Description : Types for the migration code.
-- Copyright   : (c) Brian Hurt, 2023
-- License     : None
-- Maintainer  : Brian Hurt <bhurt42@gmail.com>
-- Stability   : experimental
--
-- This is an internal module of
-- "Database.PostgreSQL.Simple.Migrate.Internal", you probably want
-- that module instead.
--
module Database.PostgreSQL.Simple.Migrate.Internal.Types (
    Migration(..),
    makeMigration,
    Optional(..),
    Replaces(..),
    makeReplaces
) where

    import           Control.DeepSeq
    import           Data.CaseInsensitive             (CI)
    import qualified Data.CaseInsensitive             as CI
    import           Data.Text                        (Text)
    import           Data.Typeable
    import           Database.PostgreSQL.Simple.Types (Query (..))
    import           GHC.Generics

    -- | Boolean-analog for whether a migration is optional or required.
    --
    data Optional = Optional  
                        -- ^ The migration may not have been applied yet.
                    | Required
                        -- ^ The migration must have been applied.
        deriving (Show, Read, Ord, Eq, Generic, Typeable, Bounded, Enum)

    instance NFData Optional where
        rnf = rwhnf



    -- | A single database schema migration.
    -- 
    -- It represents a single, atomic, change to the database schema
    -- to be applied transactionally.  The core is the command, which
    -- is a SQL script, generally one or more schema modification commands
    -- like CREATE TABLE.
    --
    -- You create a Migration with the makeMigration command, which
    -- fills in sensible default values for most of the fields:
    --
    -- @
    -- {-# LANGUAGE QuasiQuotes #-}
    --
    -- import Database.PostgreSQL.Simple.SqlQQ
    --
    -- myMigration :: Migration
    -- myMigration = makeMigration "example-mig-1" [sql| ... |] []
    -- @
    --
    -- You can then override parameters with the structure update
    -- syntax:
    --
    -- @
    -- myMigration2 :: Migration
    -- myMiragtion2 = makeMigration "example-mig-2" [sql| ... |]
    --                  [ \"example-mig-1\" ]
    --                  { optional = Optional,
    --                      phase = 2 }
    -- @
    --
    -- == Field Details
    --
    -- The details of the specific fields are as follows:
    --
    -- ==== __name__
    --
    -- The name of the migration.
    --
    -- All migrations need to be unique.  It customary to add a
    -- version number on to the end of the name, for example
    -- \"example-mig-1\", \"example-mig-2\", etc.  This tells
    -- the human reader that these migrations are related.  It
    -- is an error for two migrations to have the same name.
    --
    -- Note that names are case INSENSITIVE, so "foo", "Foo", and
    -- "FOO" are all considered the same name.
    --
    -- This value is passed to `makeMigration` as it's first argument.
    --
    -- ==== __command__
    --
    -- The command to execute to perform the migration.  
    --
    -- This query is passed to `Database.PostgreSQL.Simple.execute_`.
    -- Note that no arguments are allowed (execute_ is called,
    -- not execute).
    --
    -- This value is passed to `makeMigration` as it's second argument.
    --
    -- ==== __dependencies__
    --
    -- The list of the names of other migrations this migration
    -- depends upon.
    --
    -- We want to spread the list of migrations out to many
    -- different modules.  This way, the migrations that control
    -- the database schema live near the code that accesses
    -- them.  The question then becomes how to order the migrations?
    --
    -- The solution is to just list the other migrations this
    -- migration depends upon.  We can then do a topological
    -- sort, which will give us an order we can apply the
    -- migrations in- guaranteeing that no migration will be
    -- applied before it's dependencies.
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
    -- This value is passed to `makeMigration` as it's second argument.
    -- A value of @[]@ (the empty list) is valid, and is the
    -- recommended default.
    --
    -- ==== __optional__
    --
    -- Whether the migration is optional.
    --
    -- The idea here is that the server can be aware of migrations that
    -- are coming, but may not have been applied yet.  The server can
    -- then test the database to see if the migration has been applied
    -- or not, and response appropriately.  The server can also check
    -- on start up if all the required migrations have been applied.
    --
    -- This solves the chicken and the egg problem for applying new
    -- schema changes.  You first deploy a new server that has the
    -- new migration as optional.  Once all the servers have been
    -- redeployed, you then apply the new migration.  At some later
    -- date, the optional migration can then be marked as required.
    -- 
    -- `makeMigration` sets this field to `Required`.
    --
    -- ==== __phase__
    --
    -- The phase to apply the migration in.
    --
    -- All migrations of phase N will be applied before any
    -- migrations of phase N+1 are applied.  Basically,
    -- all migrations of phase N+1 have a dependency on all
    -- migrations of phase N and all previous phases.  This
    -- gives us a crude way to say a given migration should
    -- be applied earlier or later than broad categories of
    -- other migrations, without having to directly depend
    -- upon them.
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
    -- `makeMigration` sets this field to 1.
    --
    -- ==== __replaces__
    --
    -- The list of migrations this migration replaces.
    --
    -- As a database schema evolves, an increasing number of
    -- migrations are fixes or changes to previous migrations
    -- instead of de novo creations (\"ALTER TABLE\" migrations
    -- instead of \"CREATE TABLE\" migrations).  This slows down
    -- applying migrations, and makes it hard to determine what
    -- the current schema actually is, given the list of migrations.
    --
    -- `replaces` is the solution to this.  It marks the migration
    -- as the equivalent of having applied some list of previous
    -- migrations.  When a migration has a non-empty @replaces@ field,
    -- applying it now works like:
    --
    -- (1) Check to see if the migration has already been applied,
    -- i.e. if it is in the table of applied migrations.  If it has
    -- been applied, do nothing.
    --
    -- (2) Check to see if all of the replaced migrations
    -- exist.  If they do, remove all of them from the table of 
    -- applied migrations, and add this migration- without executing
    -- the command.
    --
    -- (3) Otherwise, run the command and add this migration to the
    -- table of applied migrations.
    --
    -- __WARNING__: The assumption is that the command, when executed,
    -- will result the same schema as the aggregate result of applying
    -- all the replaced migrations.  For obvious reasons this condition
    -- is not checked for.  Violating it, however, will result in tears
    -- and recriminations.  Use this feature at your own risk.
    --
    -- Replaced migrations have their fingerprints checked, to make
    -- sure that we are replacing the migrations we think we are
    -- replacing.  You can get the fingerprint for a given migration
    -- by calling the `makeFingerprint` function, or by running
    -- the following query on a database that has the migration
    -- applied:
    --
    -- @
    -- SELECT
    --      fingerprint
    -- FROM
    --      schema_migrations
    -- WHERE
    --      name = 'example-mig-1'
    -- ;
    -- @
    --
    -- Replaced migrations can be marked as optional.  This means that
    -- the replaced migration does not have to have been applied (yet)
    -- for the replacement to take place.  If the optional replaced
    -- migration has been applied, it's fingerprint still has to be
    -- the same, and it will be removed with the other replaced migrations.
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
    -- An example:
    --
    -- Say we have the following schema migrations that have already been
    -- applied:
    --
    -- @
    -- migrations = [
    --    makeMigration "foo-1"
    --      [sql|CREATE TABLE foo (foo INT PRIMARY KEY); |] [],
    --    makeMigration "foo-2"
    --      [sql|ALTER TABLE foo ADD COLUMN bar TEXT; |]
    --      [ "foo-1" ],
    --    makeMigration "foo-3"
    --      [sql|ALTER TABLE foo ADD COLUMN baz BOOL; |] ]
    --      [ "foo-1", "foo-2" ]
    -- @
    --
    -- We could replace this with:
    --
    -- @
    -- migrations = [
    --    makeMigration "foo-4"
    --        [sql| CREATE TABLE foo (
    --                 foo INT PRIMARY KEY,
    --                 bar TEXT,
    --                 baz BOOL); |]
    --        { replaces = [
    --              makeReplaces "foo1" "<fingerprint>",
    --              makeReplaces "foo2" "<fingerprint>",
    --              makeReplaces "foo3" "<fingerprint>" ] }
    --    ]
    -- @
    --
    -- Note that the @replaces@ field is not part of the fingerprint.
    -- So, after you are sure all databases have been upgraded, you
    -- can quietly drop the replaces field, and move forward with
    -- your cleaner migration list.
    --
    -- `makeMigration` sets this field to @[]@ (the empty list).
    --
    data Migration = Migration {
        name :: CI Text,
        -- ^ The name of the migration.

        command :: Query,
        -- ^ The command to execute to perform the migration.  

        dependencies :: [ CI Text ],
        -- ^ The list of the names of other migrations this migration
        -- depends upon.

        optional :: Optional,
        -- ^ Whether the migration is optional.

        phase :: Int,
        -- ^ The phase to apply the migration in.
        
        replaces :: [ Replaces ]
        -- ^ The list of migrations this migration replaces.
    } deriving (Show, Read, Ord, Eq, Generic, Typeable)

    instance NFData Migration where
        rnf mig = rnf (name mig)
                    `seq` rnf (fromQuery (command mig))
                    `seq` rnf (optional mig)
                    `seq` rnf (dependencies mig)
                    `seq` rnf (phase mig)
                    `seq` rnf (replaces mig)
                    `seq` ()

    -- | Create a migration with most fields filled in with sensible
    -- defaults.
    --
    -- The migration name and command must be given, but everything
    -- else gets a default value.  Specifically:
    --
    -- * The `optional` field is set to `Required`.
    --
    -- * The `phase` field is set to 1.
    --
    -- * The `dependencies` field is set to @[]@ (the empty list).
    --
    -- * The `replaces` field is set to @[]@ (the empty list).
    --
    makeMigration :: Text
                        -- ^ Migration name
                        -> Query
                        -- ^ Migration command
                        -> [ Text ]
                        -- ^ Dependencies
                        -> Migration
    makeMigration nm cmd deps = Migration {
                                name         = CI.mk nm,
                                command      = cmd,
                                optional     = Required,
                                phase        = 1,
                                dependencies = (CI.mk <$> deps),
                                replaces     = [] }


    -- | Data about a replaced migration.
    --
    -- See the documentation under the replaces field in `Migration`.
    --
    data Replaces = Replaces {
                            rName        :: CI Text,
                            -- ^ Name of the migration being replaced.

                            rFingerprint :: Text,
                            -- ^ Fingerprint of the migration being replaced.

                            rOptional    :: Optional
                            -- ^ If the replaced migration is optional.
                            }
                            deriving (Show, Read, Ord, Eq, Generic, Typeable)

    instance NFData Replaces where
        rnf rep = rnf (rName rep)
                    `seq` rnf (rFingerprint rep)
                    `seq` rnf (rOptional rep)
                    `seq` ()


    -- | Make a Replaces data structure.
    --
    -- The rOptional field can be set using the structure update syntax,
    -- like:
    --
    -- @
    --    makeReplaces "example-1" "..." { rOptional = Optional }
    -- @
    --
    -- See the replaces field documentation in the `Migration` data
    -- structure for more.
    makeReplaces :: Text
                        -- ^ The name of the migration being replaced.
                        -> Text
                        -- ^ The fingerprint of the migration being replaced.
                        -> Replaces
    makeReplaces nm fprint = Replaces {
                                rName        = CI.mk nm,
                                rFingerprint = fprint,
                                rOptional    = Required }

