{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Live (
    withLiveTests
) where


    import           Control.Exception
    import qualified Data.Text                        as Text
    import           Data.Time.Clock.POSIX
    import qualified Database.PostgreSQL.Simple       as PG
    import           Database.PostgreSQL.Simple.SqlQQ
    import qualified Database.PostgreSQL.Simple.Types as PG
    import           System.Exit
    import           Test.HUnit

    withLiveTests :: PG.ConnectInfo -> Test -> IO ()
    withLiveTests origInfo otherTests = do
            res :: Either SomeException Counts <- try go1
            case res of
                Left (SomeException _) -> exitFailure
                Right cnts
                    | ((errors cnts > 0) || (failures cnts > 0)) ->
                        exitFailure
                    | otherwise -> exitSuccess

        where
            go1 :: IO Counts
            go1 = bracket (openDB origInfo) (closeDB origInfo) go2

            go2 :: PG.Connection -> IO Counts
            go2 origConn = bracket
                                (createDB origConn)
                                (dropDB origConn)
                                go3

            go3 :: String -> IO Counts
            go3 dbName = do
                let testInfo = origInfo { PG.connectDatabase = dbName }
                bracket (openDB testInfo) (closeDB testInfo) go4

            go4 :: PG.Connection -> IO Counts
            go4 conn = do
                let allTests :: Test
                    allTests = TestList [
                                    otherTests,
                                    liveTests conn
                                ]
                putStrLn ""
                r <- runTestTT allTests
                putStrLn ""
                pure r


            openDB :: PG.ConnectInfo -> IO PG.Connection
            openDB cinfo = do
                putStrLn $ "Attempting to connect to database at "
                            ++ showInfo cinfo
                r :: Either SomeException PG.Connection
                    <- try $ do
                                conn <- PG.connect cinfo
                                _ :: [ PG.Only Int ]
                                    <- PG.query_ conn [sql| SELECT 1; |]
                                pure conn
                case r of
                    Left (SomeException e) -> do
                        putStrLn $ "Trying to connect threw exception: "
                                        ++ show e
                        throw e
                    Right conn -> pure conn

            closeDB :: PG.ConnectInfo -> PG.Connection -> IO ()
            closeDB cinfo conn = do
                putStrLn $ "Closing connection to database at "
                                ++ showInfo cinfo
                r :: Either SomeException () <- try (PG.close conn)
                case r of
                    Left (SomeException e) -> do
                        putStrLn $ "Closing connection threw exception: "
                                        ++ show e
                        throw e
                    Right () -> pure ()

            createDB :: PG.Connection -> IO String
            createDB conn = do
                now <- getPOSIXTime
                let dbname :: String
                    dbname = "psql-migrate-testdb-" ++ show now
                putStrLn $ "Creating test database " ++ show dbname
                r :: Either SomeException ()
                    <- try $ do
                        _ <- PG.execute conn
                                [sql| CREATE DATABASE ?; |]
                                (PG.Only (PG.Identifier (Text.pack dbname)))
                        pure ()
                case r of
                    Left (SomeException e) -> do
                        putStrLn $ "Creating test database threw exception: "
                                    ++ show e
                        throw e
                    Right () -> pure dbname


            dropDB :: PG.Connection -> String -> IO ()
            dropDB conn dbname = do
                putStrLn $ "Dropping test database " ++ show dbname
                r :: Either SomeException ()
                    <- try $ do
                        _ <- PG.execute conn
                                [sql| DROP DATABASE ?; |]
                                (PG.Only (PG.Identifier (Text.pack dbname)))
                        pure ()
                case r of
                    Left (SomeException e) -> do
                        putStrLn $ "Dropping test database threw exception: "
                                        ++ show e
                        putStrLn "(You may need to drop the database by hand)"
                        throw e
                    Right () -> pure ()


            showInfo :: PG.ConnectInfo -> String
            showInfo cinfo = PG.connectUser cinfo
                                ++ "@"
                                ++ PG.connectHost cinfo
                                ++ ":"
                                ++ show (PG.connectPort cinfo)
                                ++ "/"
                                ++ PG.connectDatabase cinfo
    

    liveTests :: PG.Connection -> Test
    liveTests _ = TestLabel "LiveTests" $
                    TestCase $ do
                        putStrLn "\nIn live tests!"
