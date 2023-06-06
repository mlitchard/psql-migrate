{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Finger
-- Description : Calculate migration fingerprints.
-- Copyright   : (c) Brian Hurt, 2023
-- License     : None
-- Maintainer  : Brian Hurt <bhurt42@gmail.com>
-- Stability   : experimental
--
-- This is an internal module of
-- "Database.PostgreSQL.Simple.Migrate.Internal", you probably want
-- that module instead.
--

module Database.PostgreSQL.Simple.Migrate.Internal.Finger (
    makeFingerprint,
) where

    import qualified Crypto.Hash                      as Hash
    import qualified Data.ByteArray                   as ByteArray
    import           Data.ByteString                  (ByteString)
    import qualified Data.ByteString.Base64.URL       as Base64
    import           Data.Text                        (Text)
    import qualified Data.Text.Encoding               as Encoding
    import           Database.PostgreSQL.Simple.Types (Query (..))

    -- This screws up the formatting, so they get their own,
    -- unformatted, block.
    import Database.PostgreSQL.Simple.Migrate.Internal.Types


    -- | Calculate the fingerprint of a given migration.
    makeFingerprint :: Migration -> Text
    makeFingerprint mig =
        let q :: ByteString
            q = fromQuery (command mig)

            dig :: Hash.Digest Hash.SHA3_256
            dig = Hash.hash q

            bs :: ByteString
            bs = ByteArray.convert dig

            b64 :: ByteString
            b64 = Base64.encode bs
        in
        Encoding.decodeUtf8 b64

