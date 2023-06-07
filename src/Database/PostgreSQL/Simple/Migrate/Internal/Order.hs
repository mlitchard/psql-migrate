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
module Database.PostgreSQL.Simple.Migrate.Internal.Order (
    orderMigrations,
    OrderMigrationsError(..),
    formatOrderMigrationsError
) where

    import           Control.DeepSeq
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
    import           Data.Set             (Set)
    import qualified Data.Set             as Set
    import           Data.Text            (Text)
    import qualified Data.Tree            as Tree
    import           Data.Typeable
    import           GHC.Generics

    import Database.PostgreSQL.Simple.Migrate.Internal.Types

    -- | The possible errors `orderMigrations` can return.
    --
    -- This makes unit testing a heck of a lot easier and less brittle.
    -- You can use `formatOrderMigrationsError` to convert this type
    -- into a human-readable string.
    data OrderMigrationsError =
    
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

        deriving (Show, Read, Ord, Eq, Generic, Typeable)

    instance NFData OrderMigrationsError where
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


    -- | Convert an `OrderMigrationsError` into a human-readable string.
    formatOrderMigrationsError :: OrderMigrationsError -> String
    formatOrderMigrationsError (DuplicateMigrationName nm) =
        "Two or more migrations share the name " ++ show nm
    formatOrderMigrationsError (DuplicateDependency migName depName) =
        "The migration " ++ show migName
            ++ " has duplicate dependencies " ++ show depName
    formatOrderMigrationsError (UnknownDependency migName depName) =
        "The migration " ++ show migName
            ++ " has a dependency " ++ show depName ++ " not in the list."
    formatOrderMigrationsError (RequiredDependsOnOptional
                                    reqMigName optMigName) =
        "The required migration " ++ show reqMigName
            ++ " depends upon the optional migration " ++ show optMigName
    formatOrderMigrationsError (CircularDependency migs) =
        "The following set of migrations form a dependency cycle: "
        ++ show migs
    formatOrderMigrationsError (LaterPhaseDependency
                                    earlierMig earlierPhase
                                    laterMig laterPhase) =
        "The migration " ++ show earlierMig
            ++ " in phase " ++ show earlierPhase
            ++ " dependends upon the migration " ++ show laterMig
            ++ " in phase " ++ show laterPhase
    formatOrderMigrationsError (NoRequiredReplacement migName) =
        "The migration " ++ show migName ++ " replaces other migrations,"
            ++ " but has no required replacements"
    formatOrderMigrationsError (DuplicateReplaces migName replName) =
        "The migration " ++ show migName ++ " lists the migration "
            ++ show replName ++ " in it's replaces list multiple times."
    formatOrderMigrationsError (ReplacedStillExists migName replacedName) =
        "The migration " ++ show migName
        ++ " says that it replaces the migration " ++ show replacedName
        ++ ", but that migration still exists in the list."



    -- | Simple monad typedef.
    type M a = Either OrderMigrationsError a

    -- | The type of a key in the graph.
    type K = CI Text

    -- | They type of a node in the graph.
    --
    -- Using a structure instead of a tuple for correctness reasons.
    data Node = Node {
                    getMigration :: Migration,
                    getKey :: K,
                    getEdges :: [ K ] }

    -- | Typedef for a map of name to migration (a "migmap").
    type MigMap = Map K Migration

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
                getVertex :: K -> Maybe Graph.Vertex }

    -- | Return a list of migrations in the order they can be applied in.
    --
    -- Also check all the invariants we can.
    --
    orderMigrations :: [ Migration ]
                        -> Either OrderMigrationsError [ Migration ]
    orderMigrations migs = do -- Either monad
            -- Make the name -> migration map.
            migMap :: MigMap <- makeMigMap migs

            -- Check all the migrations for validity.
            Map.foldr (checkMig migMap) (Right ()) migMap

            -- Create the per-phase graphs
            let zgrphs :: IntMap Grph
                zgrphs = makeGraphs migMap

            -- Check for cycles
            mapM_ cycleCheck zgrphs

            -- Do the topsorts
            let grs1 :: [ (Int, Grph) ]
                grs1 = IntMap.toAscList zgrphs

                grs2 :: [ Grph ]
                grs2 = snd <$> grs1

                grs3 :: [ [ Migration ] ]
                grs3 = tsort <$> grs2

                grs4 :: [ Migration ]
                grs4 = concat grs3

            pure grs4

    -- | Do a topological sort of a graph.
    tsort :: Grph -> [ Migration ]
    tsort grph =
        let vtxs :: [ Graph.Vertex ]
            vtxs = Graph.topSort (getGraph grph)

            nodes :: [ Node ]
            nodes = getNode grph <$> vtxs

        in
        getMigration <$> nodes


    -- | Create the map of name -> migration.
    --
    -- And while we're at it, make sure we don't have any duplicate
    -- names.
    --
    makeMigMap :: [ Migration ] -> M MigMap
    makeMigMap migs = foldr go Right migs Map.empty
        where
            go :: Migration
                    -> (MigMap -> M MigMap)
                    -> MigMap
                    -> M MigMap
            go mig cont mp =
                -- Are we already in the map?
                case Map.lookup (name mig) mp of
                    Just mig2 ->
                        -- Yes: we're a duplicate name.
                        Left $ DuplicateMigrationName 
                                (CI.original (name mig2))
                    Nothing ->
                        -- No: add us to the map and continue.
                        let mp2 :: MigMap
                            mp2 = Map.insert (name mig) mig mp
                        in
                        cont mp2

    -- | Check a migration for multiple error conditions.
    checkMig :: MigMap -> Migration -> M () -> M ()
    checkMig migMap mig cont = do
            foldr   checkDep
                    (\_ -> Right ())
                    (dependencies mig)
                    Set.empty
            case replaces mig of
                [] -> cont
                rs -> do
                    mapM_ checkRepl rs
                    checkReqRepl rs
                    foldr checkDupRepl (\_ -> Right ()) rs Set.empty
                    cont
                    
        where

            -- Check a dependency for multiple error conditions.
            checkDep :: K
                        -> (Set K -> M ())
                        -> Set K
                        -> M ()
            checkDep depCIName next set =
                -- Check that the dependency exists
                case Map.lookup depCIName migMap of
                    Nothing     ->
                        Left $ UnknownDependency
                                (CI.original (name mig))
                                (CI.original depCIName)
                    Just depMig
                        -- Check that we haven't already seen this
                        -- dependency.
                        | (Set.member depCIName set)       ->
                            Left $ DuplicateDependency
                                    (CI.original (name mig))
                                    (CI.original depCIName)

                        -- Check that the depency isn't optional
                        -- or this migration isn't required.
                        | ((optional depMig == Optional)
                            && (optional mig == Required)) ->
                            Left $ RequiredDependsOnOptional
                                    (CI.original (name mig))
                                    (CI.original (name depMig))

                        -- Check that the dependency is not in a
                        -- later phase.
                        | (phase depMig > phase mig) ->
                            Left $ LaterPhaseDependency
                                    (CI.original (name mig))
                                    (phase mig)
                                    (CI.original (name depMig))
                                    (phase depMig)

                        -- Check if we're a direct cycle (i.e. we
                        -- depend upon ourselves).
                        | (depCIName == (name mig)) ->
                            Left $ CircularDependency
                                    ((CI.original (name mig))
                                        :| [ CI.original depCIName ])
                        | otherwise                        ->
                           next (Set.insert depCIName set)

            -- Check if a replaced migration still exists.
            checkRepl :: Replaces -> M ()
            checkRepl repl =
                case Map.lookup (rName repl) migMap of
                    Nothing -> Right ()
                    Just _  -> Left $
                                ReplacedStillExists
                                    (CI.original (name mig))
                                    (CI.original (rName repl))

            -- Check that we have at least one required replaces.
            --
            -- Note that we don't have to worry about the
            -- empty list case, as that was already dealt with.
            --
            checkReqRepl :: [ Replaces ] -> M ()
            checkReqRepl repls =
                if any isReq repls
                then Right ()
                else Left $ NoRequiredReplacement
                                (CI.original (name mig))

            isReq :: Replaces -> Bool
            isReq repl = rOptional repl == Required

            checkDupRepl :: Replaces -> (Set K -> M ()) -> Set K -> M ()
            checkDupRepl repl next set =
                if (Set.member (rName repl) set)
                then Left $ DuplicateReplaces (CI.original (name mig))
                                (CI.original (rName repl))
                else next (Set.insert (rName repl) set)

    -- | Turn a MigMap into a set of graphs, one per phase.
    makeGraphs :: MigMap -> IntMap Grph
    makeGraphs migmap = makeG <$> fullG
        where
            makeG :: Map K Node -> Grph
            makeG nmap = toGrph . Graph.graphFromEdges $
                            fromNode <$> Map.elems nmap

            -- The full graph
            fullG :: IntMap (Map K Node)
            fullG = Map.foldl' go initG migmap
                where
                    go :: IntMap (Map K Node)
                            -> Migration
                            -> IntMap (Map K Node)
                    go zmap mig =
                        -- adjust is the correct function to use
                        -- here- every phase with a migration
                        -- should be a member of zmap.
                        IntMap.adjust (addEdges mig)
                            (phase mig)
                            zmap

                    addEdges :: Migration -> Map K Node -> Map K Node
                    addEdges mig nmap =
                        List.foldl'
                            (addEdge (name mig))
                            nmap
                            (dependencies mig)

                    addEdge :: K -> Map K Node -> K -> Map K Node
                    addEdge target nmap source =
                        -- adjust is the correct function to use
                        -- here- if the source is in a different
                        -- phase than the target, we don't want
                        -- to add the edge (the phases take care
                        -- of enforcing the ordering).
                        Map.adjust (plusEdge target) source nmap

                    plusEdge :: K -> Node -> Node
                    plusEdge target node =
                        node { getEdges = target : getEdges node }

            -- Our initial graph, where we've added all the nodes,
            -- but no edges.
            initG :: IntMap (Map K Node)
            initG = Map.foldl' go IntMap.empty migmap
                where
                    go :: IntMap (Map K Node) -> Migration ->
                            IntMap (Map K Node)
                    go zmap mig =
                        let node :: Node
                            node = Node {
                                    getMigration = mig,
                                    getKey = name mig,
                                    getEdges = [] }
                        in
                        IntMap.alter (addNode node) (phase mig) zmap

                    addNode :: Node
                                -> Maybe (Map K Node)
                                -> Maybe (Map K Node)
                    addNode node Nothing =
                        Just $ Map.singleton (getKey node) node
                    addNode node (Just nmap) =
                        Just $ Map.insert (getKey node) node nmap

    -- | Check a `Grph` for cycles.
    --
    -- Note: we only check for intra-phase cycles, cycles within a single
    -- phase.  Intra-phase cycles, cycles that involve multiple different
    -- phases, will, by necessity, include dependencies from migrations
    -- in later phases to migrations in earlier phases.  Which is an error
    -- caught earlier in the process.
    --
    cycleCheck :: Grph -> M ()
    cycleCheck grph =
            let trees :: Graph.Forest Graph.Vertex
                trees = Graph.scc (getGraph grph)
            in
            foldr checkTree (Right ()) trees
        where
            checkTree :: Graph.Tree Graph.Vertex -> M () -> M ()
            checkTree tree next =
                case Tree.subForest tree of
                    [] -> next
                    _  ->
                        case Foldable.toList tree of
                            []     -> error "Unreachable code reached!"
                            (x:xs) ->
                                Left . CircularDependency $
                                    fixup <$> (x :| xs)

            fixup :: Graph.Vertex -> Text
            fixup vtx = 
                let node :: Node
                    node = getNode grph vtx
                in
                CI.original (getKey node)


    -- | Convert a tuple to a `Node` structure.
    toNode :: (Migration, K, [ K ]) -> Node
    toNode (m, k, es) = Node {  getMigration = m,
                                getKey = k,
                                getEdges = es }

    -- | Convert a `Node` structure back into a tuple.
    fromNode :: Node -> (Migration, K, [ K ])
    fromNode node = (getMigration node, getKey node, getEdges node)


    -- | Convert a tuple to a `Grph` structure.
    toGrph :: (Graph.Graph,
                    Graph.Vertex -> (Migration, K, [ K ]),
                    K -> Maybe Graph.Vertex)
                -> Grph
    toGrph (graph, getn, getv) =
        Grph {
            getGraph = graph,
            getNode = toNode <$> getn,
            getVertex = getv }

    

