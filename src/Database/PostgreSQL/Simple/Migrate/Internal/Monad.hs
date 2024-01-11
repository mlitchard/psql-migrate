{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Monad
-- Description : The monad control stack I need
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
-- Rather than depending upon the whole monad control cluster of
-- libraries- transformers, mtl, monad base control, etc- I just
-- supply the two stacks I really need, with all the various functions
-- hand rollled.
--
module Database.PostgreSQL.Simple.Migrate.Internal.Monad (
    M,
    runM,
    liftM,
    askM,
    throwM,
    bracketM,
    withTransactionLevelM,
    N,
    runN,
    getN,
    putN,
    throwN
) where

    import           Control.Exception
    import           Control.Monad                          (ap)
    import           Data.Map                               (Map)
    import           Data.Text                              (Text)
    import qualified Database.PostgreSQL.Simple             as PG
    import qualified Database.PostgreSQL.Simple.Transaction as PG


    -- These screw up the formatting, so they get their own,
    -- unformatted, block.
    import Database.PostgreSQL.Simple.Migrate.Internal.Types (Verbose)
    import Database.PostgreSQL.Simple.Migrate.Internal.Error (MigrationsError)

    -- | The M monad type.
    --
    -- This monad is the equivalent of the transformer stack:
    --
    -- @
    -- ReaderT Verbose (ExceptT MigrationsError IO)
    -- @
    --
    -- It is used in
    -- `Database.PostgreSQL.Simple.Migrate.Internal.Apply.apply`.
    --
    newtype M a = M { runM :: Verbose -> IO (Either MigrationsError a) }

    instance Functor M where
        fmap f m = M $ \v -> (fmap . fmap) f (runM m v)

    instance Applicative M where
        pure a = M $ \_ -> pure $ Right a
        (<*>) = ap

    instance Monad M where
        return = pure

        m >>= f = M $ \v -> do
                    r1 <- runM m v
                    case r1 of
                        Left err -> pure $ Left err
                        Right a  -> runM (f a) v

    -- | Our version of liftIO.
    liftM :: forall a . IO a -> M a
    liftM io = M $ \_ -> Right <$> io

    -- | Our version of ReaderT's ask.
    askM :: M Verbose
    askM = M $ \v -> pure $ Right v

    -- | Our version of ExceptT's throwError
    throwM :: forall a . MigrationsError -> M a
    throwM err = M $ \ _ -> pure $ Left err

    type StM a = Either MigrationsError a

    -- | Our version of a lifted `bracket`.
    bracketM :: forall a b c . M a -> (a -> M b) -> (a -> M c) -> M c
    bracketM start stop act =
            M $ \v -> bracket (start' v) (stop' v) (act' v)
        where
            start' :: Verbose -> IO (StM a)
            start' v = runM start v

            stop' :: Verbose -> StM a -> IO (StM b)
            stop' _ (Left err) = pure $ Left err
            stop' v (Right a)  = runM (stop a) v

            act' :: Verbose -> StM a -> IO (StM c)
            act' _ (Left err) = pure $ Left err
            act' v (Right a)  = runM (act a) v

    -- | Our version of a lifted
    -- `Database.PostgreSQL.Simple.Transaction.withTransactionLevel`.
    withTransactionLevelM :: PG.IsolationLevel -> PG.Connection -> M a -> M a
    withTransactionLevelM iso conn act =
        M $ \v -> PG.withTransactionLevel iso conn (runM act v)


    -- | The other monad stack I need.
    --
    -- This monad is the equivalent of:
    --
    -- @
    -- StateT (Map Text Text) (Except MigrationsError)
    -- @
    --
    -- Note that this is not an IO-based stack.
    --
    -- It is used by
    -- `Database.PostgreSQL.Simple.Migrate.Internal.Apply.check`.
    --
    newtype N a = N { runN :: Map Text Text
                            -> Either MigrationsError (Map Text Text, a) }

    instance Functor N where
        fmap f n = N $ \s -> (fmap . fmap) f (runN n s)

    instance Applicative N where
        pure a = N $ \s -> Right (s, a)
        (<*>) = ap

    instance Monad N where
        return = pure

        n >>= f = N $ \s1 ->
                    case runN n s1 of
                        Left err -> Left err
                        Right (s2, a) -> runN (f a) s2

    -- | The hand-rolled version of StateT's get function.
    getN :: N (Map Text Text)
    getN = N $ \s -> Right (s, s)

    -- | The hand-rolled version of StateT's put function.
    putN :: Map Text Text -> N ()
    putN s = N $ \_ -> Right (s, ())

    -- | The hand-rolled version of Except's throw function.
    throwN :: forall a . MigrationsError -> N a
    throwN err = N $ \_ -> Left err

