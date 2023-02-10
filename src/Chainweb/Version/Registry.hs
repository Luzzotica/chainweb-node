{-# language RecordWildCards #-}

module Chainweb.Version.Registry
    ( registerVersion
    , validateVersion
    , lookupVersion
    ) where

import Control.DeepSeq
import Control.Exception
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import System.IO.Unsafe

import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.Mainnet
import Chainweb.Utils

{-# NOINLINE versionMap #-}
versionMap :: IORef (HashMap ChainwebVersionCode ChainwebVersion)
versionMap = unsafePerformIO $ do
    validateVersion mainnet
    validateVersion devnet
    newIORef $ HM.fromList
        [ (_versionCode mainnet, mainnet)
        , (_versionCode devnet, devnet)
        ]

registerVersion :: ChainwebVersion -> IO ()
registerVersion v = do
    validateVersion v
    atomicModifyIORef' versionMap $ \m ->
        case HM.lookup (_versionCode v) m of
            Just v'
                | v /= v' -> error "registerVersion: conflicting version registered already"
                | otherwise -> (m, ())
            Nothing ->
                (HM.insert (_versionCode v) v m, ())

validateVersion :: ChainwebVersion -> IO ()
validateVersion v = -- TODO
    evaluate (rnf v)

lookupVersion :: ChainwebVersionCode -> ChainwebVersion
lookupVersion code
    | code == _versionCode mainnet = mainnet
    | otherwise = lazify (unsafeDupablePerformIO $ do
        m <- readIORef versionMap
        return $ fromJuste $ HM.lookup code m
        ) { _versionCode = code }
  where
    lazify ~ChainwebVersion{..} = ChainwebVersion{..}

