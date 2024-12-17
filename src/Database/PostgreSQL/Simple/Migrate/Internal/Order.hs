{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Order
-- Description : Order migrations
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
-- This module contains the things we can do without querying the
-- database- including basic sanity checks and ordering of the
-- migrations.
--
module Database.PostgreSQL.Simple.Migrate.Internal.Order (
    Apply(..),
    orderMigrations
) where

    import           Control.Monad        (unless, when)
    import           Data.CaseInsensitive (CI)
    import qualified Data.CaseInsensitive as CI
    import qualified Data.Foldable        as Foldable
    import qualified Data.Graph           as Graph
    import           Data.IntMap.Strict   (IntMap)
    import qualified Data.IntMap.Strict   as IntMap
    import qualified Data.List            as List
    import           Data.List.NonEmpty   (NonEmpty (..))
    import           Data.Map.Strict      (Map)
    import qualified Data.Map.Strict      as Map
    import           Data.Maybe           (mapMaybe)
    import           Data.Set             (Set)
    import qualified Data.Set             as Set
    import           Data.Text            (Text)

    -- These screw up the formatting, so they get their own,
    -- unformatted, block.
    import Database.PostgreSQL.Simple.Migrate.Internal.Error
    import Database.PostgreSQL.Simple.Migrate.Internal.Types

    -- | They type of a node in the graph.
    --
    -- Using a structure instead of a tuple for correctness reasons.
    data Node = Node {
                    getMigration :: Migration,
                    getKey :: CI Text,
                    getEdges :: [ CI Text ] }

    -- | A graph and associated operations.
    --
    -- Using a structure instead of a tuple for correctness reasons.
    --
    -- We'd like to call this a Graph, but that name is taken.  If I
    -- think of a better name, I'll rename it.
    --
    data Grph = Grph {
                -- | The graph object
                getGraph :: Graph.Graph,

                -- | Look up a node given it's vertex.
                getNode :: Graph.Vertex -> Node,

                -- | Look up a vertext, given it's key.
                --
                -- Returns Nothing if the key is not in the graph.
                getVertex :: CI Text -> Maybe Graph.Vertex }


    data Apply = Apply | Replace

    -- | Order the migrations
    orderMigrations ::
        [ Migration ]
        -- ^ The list of migrations
        -> [ (Text, Text) ]
        -- ^ The pre-existing migrations (name, fingerprint)
        -> Either MigrationsError (Maybe Migration, [ (Apply, Migration) ])
    orderMigrations []   _       = Left $ EmptyMigrationList
    orderMigrations migs applied = do -- Either monad

        -- Make the name -> migration map.
        migMap :: Map (CI Text) Migration
            <- Foldable.foldlM makeMigMap Map.empty migs

        -- Make the name -> fingerprint map from the migrations
        -- already applied to the database.
        fpMap :: Map (CI Text) Text
            <- Foldable.foldlM makeFingerprintMap Map.empty applied

        -- Check all the migrations for validity.
        --
        -- remMap: Remaining map, those migrations that were in
        -- the fpMap but not accounted for in the migrations list.
        -- If this map isn't empty, that's an error.
        --
        -- apMap: The map of migrations that have not been applied
        -- to the database, and whether they are an Apply (a migration
        -- that needs to be applied) or Replace (a migration that
        -- is simply replacing an existing set of migrations).
        --
        ((remMap, apMap), ()) 
            :: ((Map (CI Text) Text, Map (CI Text) Apply), ())
            <- runStateM
                    (mapM_ (checkMigrations migMap) migs)
                    (fpMap, Map.empty)

        -- Check that remMap is empty- if not, it's an error.
        unless (Map.null remMap) $
            Left $ UnknownMigrations $ CI.original <$> Map.keys remMap

        -- Create the per-phase graphs
        let zgrphs :: IntMap Grph
            zgrphs = makeGraphs migMap

        -- Check for cycles
        mapM_ cycleCheck zgrphs

        -- Note that these two function calls are explicitly left as
        -- lazy thunks- only one will be forced.
        pure (requiredUnapplied migMap apMap, sortMigs zgrphs apMap)


    -- | Add a migration to an accumulating map of names to migrations.
    --
    -- Note that we only de-dup here, we don't do deeper checks.
    --
    makeMigMap :: Map (CI Text) Migration
                    -> Migration
                    -> Either MigrationsError (Map (CI Text) Migration)
    makeMigMap migMap mig =
        case Map.lookup (name mig) migMap of
            Nothing -> pure $ Map.insert (name mig) mig migMap
            Just mig2 -> Left $ DuplicateMigrationName mig mig2

    -- | Add a migration name and fingerprint to an accumulating map.
    --
    -- Note that we only dedup here, we don't do deeper checks.
    --
    makeFingerprintMap :: Map (CI Text) Text
                            -> (Text, Text)
                            -> Either MigrationsError (Map (CI Text) Text)
    makeFingerprintMap apMap (oName, fp) =
        let nm :: CI Text
            nm = CI.mk oName
        in
        case Map.lookup nm apMap of
            Nothing -> pure $ Map.insert nm fp apMap
            Just _  -> Left $ DuplicateExisting oName

    -- | Check a migration for validity.
    --
    -- This is where most of the sanity checking happens.
    --
    checkMigrations :: Map (CI Text) Migration
                        -> Migration
                        -> StateM ()
    checkMigrations migMap mig = do
            liftEither $ baseChecks
            mfp :: Maybe Text <- getFingerprint (name mig)
            case mfp of
                Nothing -> do
                    applyType :: Apply <- getApplyType (replaces mig)
                    setApply (name mig) applyType
                Just fp
                    | fp /= fingerprint mig ->
                        failM $ FingerprintMismatch mig fp
                    | otherwise             -> do
                        mapM_ noReplaces (replaces mig)
                        pure ()
        where

            -- Do all the basic checking.
            --
            -- Checks that don't depend upon what migrations have been
            -- applied already.
            baseChecks :: Either MigrationsError ()
            baseChecks = do
                _ <- Foldable.foldlM checkDupe Set.empty (dependencies mig)
                deps :: [ Migration ] <- traverse lookupDep (dependencies mig)
                when (optional mig == Required) $ mapM_ checkNoOpt deps
                mapM_ checkPhase deps
                mapM_ checkCirc (dependencies mig)
                case replaces mig of
                    [] -> pure ()
                    xs -> baseCheckReplaces xs

            -- Check that we don't have duplicate dependencies.
            -- checks on it.
            checkDupe :: Set (CI Text)
                            -> CI Text
                            -> Either MigrationsError (Set (CI Text))
            checkDupe st nm =
                if Set.member nm st
                    then Left $ DuplicateDependency mig (CI.original nm)
                    else pure $ Set.insert nm st

            -- Convert from dependency name to migration structure.
            --
            -- Errors out if the migration doesn't exist.
            lookupDep :: CI Text -> Either MigrationsError Migration
            lookupDep nm =
                case Map.lookup nm migMap of
                    Nothing  ->
                        Left $ UnknownDependency mig (CI.original nm)
                    Just dep -> pure dep


            -- Check that we don't depend upon on optional migration.
            --
            -- This is only called when we are a required migration.
            checkNoOpt :: Migration -> Either MigrationsError ()
            checkNoOpt dep =
                if optional dep == Optional
                then Left $ RequiredDependsOnOptional mig dep
                else pure ()

            -- Check that we are not in an early phase than a dependency.
            checkPhase :: Migration -> Either MigrationsError ()
            checkPhase dep =
                if phase mig < phase dep
                then Left $ LaterPhaseDependency mig dep
                else pure ()

            -- Check that we're not a trivial circular dependency
            -- (i.e. we depend on ourselves)
            checkCirc :: CI Text -> Either MigrationsError ()
            checkCirc depName =
                if depName == name mig
                then Left . CircularDependency $ mig :| [ ]
                else pure ()

            -- Do basic checking of replaces.
            --
            -- Two checks: no replaced migration should be in the list
            -- of migrations (i.e. in the migMap).  And there should be
            -- at least one required replacement.
            baseCheckReplaces :: [ Replaces ] -> Either MigrationsError ()
            baseCheckReplaces [] = pure ()
            baseCheckReplaces repls = do
                checkForRequiredReplacement repls
                _ <- Foldable.foldlM checkDupeReplacement Set.empty repls
                mapM_ checkReplacesDoesNotExist repls

            -- Check that we have at least one required replacement.
            -- Note that we don't have to deal with the empty list,
            -- as that has already been handled.
            checkForRequiredReplacement :: [ Replaces ]
                                            -> Either MigrationsError ()
            checkForRequiredReplacement repls =
                if any (\r -> rOptional r == Required) repls
                then pure ()
                else Left $ NoRequiredReplacement mig

            -- Check that we do not have duplicate replacements
            checkDupeReplacement :: Set (CI Text)
                                    -> Replaces
                                    -> Either MigrationsError (Set (CI Text))
            checkDupeReplacement st rep =
                if Set.member (rName rep) st
                then Left $ DuplicateReplaces mig (CI.original (rName rep))
                else pure $ Set.insert (rName rep) st

            -- Check that a replacement does not exist.
            checkReplacesDoesNotExist :: Replaces
                                            -> Either MigrationsError ()
            checkReplacesDoesNotExist rep =
                case Map.lookup (rName rep) migMap of
                    Nothing   -> pure ()
                    Just rmig -> Left $ ReplacedStillInList mig rmig

            -- | We should not have any of the replaced migrations in
            -- the database.
            --
            -- This is called when the migration that is replacing them
            -- has already been applied to the database.  So the
            -- migrations it is replacing should have been removed
            -- already.
            noReplaces :: Replaces -> StateM ()
            noReplaces repl = do
                mfp :: Maybe Text <- getFingerprint (rName repl)
                case mfp of
                    Nothing -> pure ()
                    Just _  -> failM $ ReplacedStillInDB mig
                                            (CI.original (rName repl))

            -- | Make sure that either all the required replaced migrations
            -- are in the database, or no replaced migrations are in the
            -- database.
            getApplyType :: [ Replaces ] -> StateM Apply
            getApplyType repls = do
                -- Run through all the replacements, and see if they're
                -- in the database.  Check their fingerprints if they
                -- are.
                dbs :: [ (Bool, Replaces) ] <- traverse isInDB repls
                if any fst dbs
                then
                    -- At least one replaced migration is in the database.
                    -- We don't care if it's optional or required.  But
                    -- this means that every required replacement should
                    -- be in the database.  It's an error if this isn't
                    -- so.
                    let f :: (Bool, Replaces) -> Bool
                        f (inDB, repl) = (not inDB)
                                            && (rOptional repl == Required)
                    in
                    case List.find f dbs of
                        Nothing  -> pure Replace
                        Just (_, repl) ->
                            failM $ RequiredReplacementMissing mig
                                                (CI.original (rName repl))
                else pure Apply

            isInDB :: Replaces -> StateM (Bool, Replaces)
            isInDB repl = do
                mfp :: Maybe Text <- getFingerprint (rName repl)
                case mfp of
                    Nothing -> pure (False, repl)
                    Just fp ->
                        if (fp == rFingerprint repl)
                        then pure (True, repl)
                        else failM $ ReplacedFingerprint mig
                                            (CI.original (rName repl))

    -- | Turn a MigMap into a set of graphs, one per phase.
    makeGraphs :: Map (CI Text) Migration -> IntMap Grph
    makeGraphs migMap = makeG <$> fullG
        where
            makeG :: Map (CI Text) Node -> Grph
            makeG nmap = toGrph . Graph.graphFromEdges $
                            fromNode <$> Map.elems nmap

            -- The full graph
            fullG :: IntMap (Map (CI Text) Node)
            fullG = Map.foldl' go initG migMap
                where
                    go :: IntMap (Map (CI Text) Node)
                            -> Migration
                            -> IntMap (Map (CI Text) Node)
                    go zmap mig =
                        -- adjust is the correct function to use
                        -- here- every phase with a migration
                        -- should be a member of zmap.
                        IntMap.adjust (addEdges mig)
                            (phase mig)
                            zmap

                    addEdges :: Migration
                                -> Map (CI Text) Node
                                -> Map (CI Text) Node
                    addEdges mig nmap =
                        List.foldl'
                            (addEdge (name mig))
                            nmap
                            (dependencies mig)

                    addEdge :: CI Text
                                -> Map (CI Text) Node
                                -> CI Text
                                -> Map (CI Text) Node
                    addEdge target nmap source =
                        -- adjust is the correct function to use
                        -- here- if the source is in a different
                        -- phase than the target, we don't want
                        -- to add the edge (the phases take care
                        -- of enforcing the ordering).
                        Map.adjust (plusEdge target) source nmap

                    plusEdge :: CI Text -> Node -> Node
                    plusEdge target node =
                        node { getEdges = target : getEdges node }

            -- Our initial graph, where we've added all the nodes,
            -- but no edges.
            initG :: IntMap (Map (CI Text) Node)
            initG = Map.foldl' go IntMap.empty migMap
                where
                    go :: IntMap (Map (CI Text) Node) -> Migration ->
                            IntMap (Map (CI Text) Node)
                    go zmap mig =
                        let node :: Node
                            node = Node {
                                    getMigration = mig,
                                    getKey = name mig,
                                    getEdges = [] }
                        in
                        IntMap.alter (addNode node) (phase mig) zmap

                    addNode :: Node
                                -> Maybe (Map (CI Text) Node)
                                -> Maybe (Map (CI Text) Node)
                    addNode node Nothing =
                        Just $ Map.singleton (getKey node) node
                    addNode node (Just nmap) =
                        Just $ Map.insert (getKey node) node nmap

    -- | Check a Grph for cycles.
    --
    -- Note: we only check for intra-phase cycles, cycles within a single
    -- phase.  Intra-phase cycles, cycles that involve multiple different
    -- phases, will, by necessity, include dependencies from migrations
    -- in later phases to migrations in earlier phases.  Which is an error
    -- caught earlier in the process.
    --
    cycleCheck :: Grph -> Either MigrationsError ()
    cycleCheck grph =
            let trees :: Graph.Forest Graph.Vertex
                trees = Graph.scc (getGraph grph)
            in
            mapM_ checkTree trees
        where
            checkTree :: Graph.Tree Graph.Vertex
                            -> Either MigrationsError ()
            checkTree tree =
                case Foldable.toList tree of
                    []     -> pure ()
                    (x:xs) -> Left . CircularDependency $
                                    fixup <$> (x :| xs)

            fixup :: Graph.Vertex -> Migration
            fixup vtx = 
                let node :: Node
                    node = getNode grph vtx
                in
                getMigration node

    -- | Convert a tuple to a Node structure.
    toNode :: (Migration, CI Text, [ CI Text ]) -> Node
    toNode (m, k, es) = Node {  getMigration = m,
                                getKey = k,
                                getEdges = es }

    -- | Convert a Node structure back into a tuple.
    fromNode :: Node -> (Migration, CI Text, [ CI Text ])
    fromNode node = (getMigration node, getKey node, getEdges node)


    -- | Convert a tuple to a Grph structure.
    toGrph :: (Graph.Graph,
                    Graph.Vertex -> (Migration, CI Text, [ CI Text ]),
                    CI Text -> Maybe Graph.Vertex)
                -> Grph
    toGrph (graph, getn, getv) =
        Grph {
            getGraph = graph,
            getNode = toNode <$> getn,
            getVertex = getv }

    -- | Do the topological sort of the graphs, and filter for
    -- only those that need applying or replacing.
    --
    sortMigs  :: IntMap Grph -> Map (CI Text) Apply -> [ (Apply, Migration) ]
    sortMigs zgrphs apMap =
        let grs1 :: [ (Int, Grph) ]
            grs1 = IntMap.toAscList zgrphs

            grs2 :: [ Grph ]
            grs2 = snd <$> grs1

            grs3 :: [ [ Migration ] ]
            grs3 = tsort <$> grs2

            grs4 :: [ Migration ]
            grs4 = concat grs3

            f :: Migration -> Maybe (Apply, Migration)
            f mig =
                case Map.lookup (name mig) apMap of
                    Nothing -> Nothing
                    Just ap -> Just (ap, mig)
        in
        if Map.null apMap
        then []
        else mapMaybe f grs4

    -- | Do a topological sort of a graph.
    tsort :: Grph -> [ Migration ]
    tsort grph =
        let vtxs :: [ Graph.Vertex ]
            vtxs = Graph.topSort (getGraph grph)

            nodes :: [ Node ]
            nodes = getNode grph <$> vtxs

        in
        getMigration <$> nodes

    -- | Are there any migrations that are required, yet not applied?
    requiredUnapplied :: Map (CI Text) Migration
                            -> Map (CI Text) Apply
                            -> Maybe Migration
    requiredUnapplied migMap apmap =
        let f :: CI Text -> Maybe Migration -> Maybe Migration
            f nm rest =
                case Map.lookup nm migMap of
                    -- This should never happen.
                    Nothing                        -> rest
                    Just mig 
                        | optional mig == Required -> Just mig
                        | otherwise                -> rest
        in
        foldr f Nothing (Map.keys apmap)


    -- | A state monad.
    --
    -- I don't want to depend upon MTL just for one bit of code, but
    -- having a transformer stack in one place is _really useful_.
    -- So I just hand-roll my own.
    --
    newtype StateM a =
        StateM {
            runStateM ::
                (Map (CI Text) Text, Map (CI Text) Apply)
                -> Either
                    MigrationsError
                    ((Map (CI Text) Text, Map (CI Text) Apply), a) }

    instance Functor StateM where
        fmap f sm = StateM $ \s -> fmap f <$> runStateM sm s

    instance Applicative StateM where
        pure a = StateM $ \s -> Right (s, a)
        sm1 <*> sm2 =
            StateM $ \s0 -> do -- Either monad
                (s1, f) <- runStateM sm1 s0
                (s2, a) <- runStateM sm2 s1
                Right $ (s2, f a)

    instance Monad StateM where
        return = pure
        xm >>= f =
            StateM $ \s0 -> do -- Either Monad
                (s1, x) <- runStateM xm s0
                runStateM (f x) s1

    -- | Look up a fingerprint
    --
    -- If the migation name existing in the map of name/fingerprints,
    -- then remove it from that map, and return Just the fingerprint.
    -- Otherwise, return Nothing.
    getFingerprint :: CI Text -> StateM (Maybe Text)
    getFingerprint nm = StateM $
        \(fpm, apm) ->
            case Map.lookup nm fpm of
                Nothing -> Right ((fpm, apm), Nothing)
                Just fp -> Right ((Map.delete nm fpm, apm), Just fp)

    -- | Set the apply value.
    setApply :: CI Text -> Apply -> StateM ()
    setApply nm ap =
        StateM $ \(fpm, apm) -> Right ((fpm, Map.insert nm ap apm), ())

    -- | Lift an either into a StateM
    liftEither :: Either MigrationsError a -> StateM a
    liftEither e = StateM $ \s -> (\a -> (s, a)) <$> e

    -- | Fail with a migrations error.
    failM :: MigrationsError -> StateM a
    failM err = StateM $ \_ -> Left err
