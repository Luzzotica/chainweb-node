{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Version
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Properties of Chainweb Versions
--
module Chainweb.Version
-- ( DevVersionConfig(..)
-- , versionForks
-- , versionForkUpperBound
-- , versionBlockRate
-- , Fork(..)
-- , defaultDevVersionConfig
-- , ChainwebVersion(..)
-- , ChainwebVersionName(..)
-- , versionConfig
-- , ChainwebVersion
-- , encodeChainwebVersion
-- , decodeChainwebVersion
-- , chainwebVersionFromText
-- , chainwebVersionToText
-- , chainwebVersionId

-- -- * Properties of Chainweb Version
-- -- ** Chain Graph
-- , chainwebGraphs
-- , genesisGraph
-- , genesisHeight
-- , to20ChainsDevelopment
-- -- ** POW
-- , BlockRate(..)
-- , blockRate
-- , WindowWidth(..)
-- , window
-- , headerSizeBytes
-- , workSizeBytes
-- -- ** Payload Validation Parameters
-- , maxBlockGasLimit
-- -- ** Payload Validation Guards
-- , vuln797Fix
-- , coinV2Upgrade
-- , to20ChainRebalance
-- , pactBackCompat_v16
-- , skipTxTimingValidation
-- , enableModuleNameFix
-- , enableModuleNameFix2
-- , enablePactEvents
-- , enableSPVBridge
-- , pact4coin3Upgrade
-- , pact420Upgrade
-- , enforceKeysetFormats
-- , doCheckTxHash
-- , chainweb213Pact
-- , chainweb214Pact
-- , chainweb215Pact
-- , chainweb216Pact
-- , chainweb217Pact
-- , pact44NewTrans

-- -- ** BlockHeader Validation Guards
-- , slowEpochGuard
-- , oldTargetGuard
-- , skipFeatureFlagValidationGuard
-- , oldDaGuard

-- -- * Typelevel ChainwebVersion
-- , ChainwebVersionT(..)
-- , ChainwebVersionSymbol
-- , chainwebVersionSymbolVal
-- , SomeChainwebVersionT(..)
-- , KnownChainwebVersionSymbol
-- , someChainwebVersionVal

-- -- * Singletons
-- , Sing(SChainwebVersion)
-- , SChainwebVersion
-- , pattern FromSingChainwebVersion

-- -- * HasChainwebVersion
-- , HasChainwebVersion(..)
-- , mkChainId
-- , chainIds

-- -- * ChainId
-- , module Chainweb.ChainId

-- -- * Re-exports from Chainweb.ChainGraph

-- -- ** Chain Graph
-- , ChainGraph
-- , HasChainGraph(..)
-- , adjacentChainIds
-- , chainGraphAt
-- , chainGraphAt_
-- , chainwebGraphsAt

-- -- ** Graph Properties
-- , order
-- , diameter
-- , degree
-- , shortestPath

-- -- ** Undirected Edges
-- , AdjPair
-- , _getAdjPair
-- , pattern Adj
-- , adjs
-- , adjsOfVertex
-- , checkAdjacentChainIds

-- -- * Internal. Don't use. Exported only for testing
-- , headerSizes
-- , headerBaseSizeBytes
-- ) where
where

import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding ((.=), (<.>), index)
import Control.Monad.Catch

import Data.Aeson hiding (pairs)
import Data.Aeson.Types
import Data.Foldable
import Data.Function
import Data.Functor.Apply
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word

import GHC.Generics(Generic, Generic1)
import GHC.Stack
import GHC.TypeLits

import Numeric.Natural

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.MerkleUniverse
import Chainweb.Miner.Pact
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Utils.Serialization

-- import qualified Chainweb.Pact.Transactions.OtherTransactions as Other
-- import qualified Chainweb.Pact.Transactions.CoinV3Transactions as CoinV3
-- import qualified Chainweb.Pact.Transactions.CoinV4Transactions as CoinV4
-- import qualified Chainweb.Pact.Transactions.CoinV5Transactions as CoinV5

import Data.Singletons

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Bootstrap Peer Info

-- | For each chainweb version there is a hardcoded set of bootstrap nodes for
-- the P2P network.
--
-- If a bootstrap node has an public DNS name with an official TLS certificate
-- the peer-id should be omitted. For bootstrap nodes without an proper
-- certificate, the peer id is the SHA256 hash of the X509 certificate.
--
-- bootstrapPeerInfos :: ChainwebVersion -> [PeerInfo]
-- bootstrapPeerInfos Test{} = [testBootstrapPeerInfos]
-- bootstrapPeerInfos TimedConsensus{} = [testBootstrapPeerInfos]
-- bootstrapPeerInfos PowConsensus{} = [testBootstrapPeerInfos]
-- bootstrapPeerInfos TimedCPM{} = [testBootstrapPeerInfos]
-- bootstrapPeerInfos FastTimedCPM{} = [testBootstrapPeerInfos]
-- bootstrapPeerInfos Development{} = []
-- bootstrapPeerInfos Testnet04 = domainAddr2PeerInfo testnetBootstrapHosts
-- bootstrapPeerInfos Mainnet01 = domainAddr2PeerInfo mainnetBootstrapHosts

testBootstrapPeerInfos :: PeerInfo
testBootstrapPeerInfos =
    PeerInfo
#if WITH_ED25519
        { _peerId = Just $ unsafeFromText "BMe2hSdSEGCzLwvoYXPuB1BqYEH5wiV5AvacutSGWmg"
#else
        { _peerId = Just $ unsafeFromText "9LkpIG95q5cs0YJg0d-xdR2YLeW_puv1PjS2kEfmEuQ"
#endif
            -- this is the fingerprint of the certificate and key that is stored
            -- in ./scripts/test-bootstrap-node.config". For programatic use of
            -- the same certificate is also available at
            -- "Chainweb.Test.P2P.Peer.BootstrapConfig". It is intended for
            -- testing purposes only.

        , _peerAddr = HostAddress
            { _hostAddressHost = localhost
            , _hostAddressPort = 1789
            }
        }

-- | Official testnet bootstrap nodes
--
domainAddr2PeerInfo :: [HostAddress] -> [PeerInfo]
domainAddr2PeerInfo = fmap (PeerInfo Nothing)

data Fork
    = Vuln797Fix
    | SlowEpoch
    | OldTargetGuard
    | EnforceKeysetFormats
    | SkipFeatureFlagValidation
    | OldDAGuard
    | CheckTxHash
    | PactEvents
    | SkipTxTimingValidation
    | SPVBridge
    | ModuleNameFix
    | ModuleNameFix2
    | PactBackCompat_v16
    | CoinV2
    | Pact4Coin3
    | Pact420
    | Chainweb213Pact
    | Chainweb214Pact
    | Chainweb215Pact
    | Chainweb216Pact
    | Chainweb217Pact
    | Pact44NewTrans
    -- always add new forks at the end, not in the middle of the constructors.
    deriving (Bounded, Generic, NFData, Hashable, Eq, Enum, Ord, Show)

instance HasTextRepresentation Fork where
  toText Vuln797Fix = "vuln797Fix"
  toText CoinV2 = "coinV2"
  toText SlowEpoch = "slowEpoch"
  toText OldTargetGuard = "oldTargetGuard"
  toText EnforceKeysetFormats = "enforceKeysetFormats"
  toText SkipFeatureFlagValidation = "skipFeatureFlagValidation"
  toText OldDAGuard = "oldDaGuard"
  toText CheckTxHash = "checkTxHash"
  toText Pact4Coin3 = "pact4Coin3"
  toText PactEvents = "pactEvents"
  toText SkipTxTimingValidation = "skipTxTimingValidation"
  toText SPVBridge = "spvBridge"
  toText PactBackCompat_v16 = "pactBackCompat_v16"
  toText ModuleNameFix = "moduleNameFix"
  toText ModuleNameFix2 = "moduleNameFix2"
  toText Pact420 = "pact420"
  toText Chainweb213Pact = "chainweb213Pact"
  toText Chainweb214Pact = "chainweb214Pact"
  toText Chainweb215Pact = "chainweb215Pact"
  toText Chainweb216Pact = "chainweb216Pact"
  toText Chainweb217Pact = "chainweb217Pact"
  toText Pact44NewTrans = "pact44NewTrans"

  fromText "vuln797Fix" = return Vuln797Fix
  fromText "coinV2" = return CoinV2
  fromText "slowEpoch" = return SlowEpoch
  fromText "oldTargetGuard" = return OldTargetGuard
  fromText "enforceKeysetFormats" = return EnforceKeysetFormats
  fromText "skipFeatureFlagValidation" = return SkipFeatureFlagValidation
  fromText "oldDaGuard" = return OldDAGuard
  fromText "checkTxHash" = return CheckTxHash
  fromText "pact4Coin3" = return Pact4Coin3
  fromText "pactEvents" = return PactEvents
  fromText "skipTxTimingValidation" = return SkipTxTimingValidation
  fromText "spvBridge" = return SPVBridge
  fromText "pactBackCompat_v16" = return PactBackCompat_v16
  fromText "moduleNameFix" = return ModuleNameFix
  fromText "moduleNameFix2" = return ModuleNameFix2
  fromText "pact420" = return Pact420
  fromText "chainweb213Pact" = return Chainweb213Pact
  fromText "chainweb214Pact" = return Chainweb214Pact
  fromText "chainweb215Pact" = return Chainweb215Pact
  fromText "chainweb216Pact" = return Chainweb216Pact
  fromText "chainweb217Pact" = return Chainweb217Pact
  fromText "pact44NewTrans" = return Pact44NewTrans
  fromText t = throwM . TextFormatException $ "Unknown Chainweb fork: " <> t

instance ToJSON Fork where
    toJSON = toJSON . toText
instance ToJSONKey Fork where
    toJSONKey = toJSONKeyText toText
instance FromJSON Fork where
    parseJSON = parseJsonFromText "Fork"
instance FromJSONKey Fork where
    fromJSONKey = FromJSONKeyTextParser $ either fail return . eitherFromText

-- data Flags a = Flags
    -- -- -------------------------------------------------------------------------- --
    -- -- Header Validation Guards
    -- --
    -- -- The guards in this section encode when changes to validation rules for data
    -- -- on the chain become effective.
    -- --
    -- -- Only the following types are allowed as parameters for guards
    -- --
    -- -- * BlockHeader,
    -- -- * ParentHeader,
    -- -- * BlockCreationTime, and
    -- -- * ParentCreationTime
    -- --
    -- -- The result is a simple 'Bool'.
    -- --
    -- -- Guards should have meaningful names and should be used in a way that all
    -- -- places in the code base that depend on the guard should reference the
    -- -- respective guard. That way all dependent code can be easily identified using
    -- -- ide tools, like for instance @grep@.
    -- --
    -- -- Each flag should have a description that provides background for the change
    -- -- and provides all information needed for maintaining the code or code that
    -- -- depends on it.
    -- --
    -- { _slowEpochGuardFlag :: !a
    -- -- ^ Turn off slow epochs (emergency DA) for blocks from 80,000 onwward.
    -- --
    -- -- Emergency DA is considered a miss-feature.
    -- --
    -- -- It's intended purpose is to prevent chain hopping attacks, where an attacker
    -- -- temporarily adds a large amount of hash power, thus increasing the
    -- -- difficulty. When the hash power is removed, the remaining hash power may not
    -- -- be enough to reach the next block in reasonable time.
    -- --
    -- -- In practice, emergency DAs cause more problems than they solve. In
    -- -- particular, they increase the chance of deep forks. Also they make the
    -- -- behavior of the system unpredictable in states of emergency, when stability
    -- -- is usually more important than throughput.
    -- --
    -- , _oldTargetGuardFlag :: !a
    -- -- ^ Use the current block time for computing epoch start date and
    -- -- target.
    -- --
    -- -- When this guard is switched off, there will be a single epoch of just 119
    -- -- blocks. The target computation won't compensate for that, since the effects
    -- -- are marginal.
    -- --
    -- , _skipFeatureFlagValidationFlag :: !a
    -- -- ^ Skip validation of feature flags.
    -- --
    -- -- Unused feature flag bits are supposed to be set to 0. As of Chainweb 1.7, the
    -- -- Feature Flag bytes and Nonce bytes have switched places in `BlockHeader`. For
    -- -- live chains, enforcing the following condition must be ignored for the
    -- -- historical blocks for which both the Nonce and Flags could be anything.
    -- --
    -- , _oldDaGuardFlag :: !a
    -- , _vuln797FixFlag :: !a
    -- , _pactBackCompat_v16Flag :: !a
    -- -- ^ Preserve Pact bugs pre 1.6 chainweb version
    -- -- Mainnet 328000 ~ UTC Feb 20 15:36, EST Feb 20 10:56
    -- --
    -- , _skipTxTimingValidationFlag :: !a
    -- -- ^ Early versions of chainweb used the creation time of the current header
    -- -- for validation of pact tx creation time and TTL. Nowadays the times of
    -- -- the parent header a used.
    -- --
    -- -- When this guard is enabled timing validation is skipped.
    -- --
    -- , _moduleNameFixFlag :: !a
    -- -- ^ Checks height after which module name fix in effect.
    -- --
    -- , _moduleNameFix2Flag :: !a
    -- -- ^ Related, later fix (Pact #801)
    -- --
    -- , _pactEventsFlag :: !a
    -- -- ^ Turn on pact events in command output.
    -- --
    -- , _spvBridgeFlag :: !a
    -- -- ^ Bridge support: ETH and event SPV.
    -- --
    -- , _enforceKeysetFormatsFlag :: !a
    -- , _checkTxHashFlag :: !a
    -- , _pact44NewTransFlag :: !a
    -- }
    -- deriving stock (Eq, Ord, Foldable, Functor, Generic, Generic1, Show, Traversable)
    -- deriving anyclass (Hashable, NFData, ToJSON, FromJSON)

-- instance Foldable1 Flags where foldMap1 = foldMap1Default
-- instance Traversable1 Flags where
--     traverse1 f = fmap GHC.Generics.to1 . traverse1 f . GHC.Generics.from1

-- makeLenses 'Flags

-- instance Representable Flags
-- instance Distributive Flags where distribute = distributeRep

newtype ChainwebVersionName =
    ChainwebVersionName { getChainwebVersionName :: T.Text }
    deriving stock (Generic, Eq, Ord)
    deriving newtype (Show, ToJSON, FromJSON)
    deriving anyclass (Hashable, NFData)

newtype ChainwebVersionCode =
    ChainwebVersionCode { getChainwebVersionCode :: Word32 }
    deriving stock (Generic, Eq, Ord)
    deriving newtype (Show, ToJSON, FromJSON)
    deriving anyclass (Hashable, NFData)

-- -- -------------------------------------------------------------------------- --
-- -- POW Parameters

data ChainMap a = AllChains a | OnChains (HashMap ChainId a)
    deriving stock (Eq, Functor, Generic, Ord, Show)
    deriving anyclass (Hashable, NFData)

onChains :: [(ChainId, a)] -> ChainMap a
onChains = OnChains . HM.fromList

instance Applicative ChainMap where
    pure = AllChains
    OnChains l <*> OnChains r = OnChains $ HM.intersectionWith ($) l r
    OnChains l <*> AllChains r = OnChains $ fmap ($ r) l
    AllChains l <*> OnChains r = OnChains $ fmap (l $) r
    AllChains l <*> AllChains r = AllChains $ l r

instance Apply ChainMap where
    (<.>) = (<*>)

instance ToJSON a => ToJSON (ChainMap a) where
    toJSON (AllChains a) = object
        [ "allChains" .= a
        ]
    toJSON (OnChains m) = toJSON m

instance FromJSON a => FromJSON (ChainMap a) where
    parseJSON = withObject "ChainMap" $ \o ->
        (AllChains <$> o .: "allChains") <|> OnChains <$> parseJSON (Object o)

makePrisms ''ChainMap

onChain :: ChainId -> Fold (ChainMap a) a
onChain cid = folding $ \case
    OnChains m -> m ^. at cid
    AllChains a -> Just a

data Rule h a = Above (h, a) (Rule h a) | End a
    deriving stock (Eq, Ord, Show, Foldable, Functor, Generic, Generic1, Traversable)
    deriving anyclass (Hashable, NFData)

heights :: Traversal (Rule h a) (Rule h2 a) h h2
heights f (Above (h, a) t) = Above <$> ((, a) <$> f h) <*> heights f t
heights _ (End a) = pure (End a)

instance Foldable1 (Rule h) where foldMap1 = foldMap1Default
instance Traversable1 (Rule h) where
    traverse1 f (Above (h, a) t) = Above <$> ((h,) <$> f a) <.> traverse1 f t
    traverse1 f (End a) = End <$> f a

instance (ToJSON h, ToJSON a) => ToJSON (Rule h a) where
    toJSON = toJSON . go
      where
        go (Above (h, a) t) = toJSON (toJSON h, toJSON a) : go t
        go (End a) = [toJSON a]

instance (FromJSON h, FromJSON a) => FromJSON (Rule h a) where
    parseJSON = withArray "Rule" $ go . V.toList
      where
        go [] = fail "empty list"
        go [a] = End <$> parseJSON a
        go (x:xs) = Above <$> parseJSON x <*> go xs

ruleHead :: Rule h a -> (Maybe h, a)
ruleHead (Above (h, a) _) = (Just h, a)
ruleHead (End a) = (Nothing, a)

ruleDropWhile :: (h -> Bool) -> Rule h a -> Rule h a
ruleDropWhile p (Above (h, a) t)
    | p h = Above (h, a) (ruleDropWhile p t)
    | otherwise = ruleDropWhile p t
ruleDropWhile _ t = t

data Measurement h a = Bottom a | Top (h, a) | Between (h, a) (h, a)

measureRule' :: (h -> Bool) -> Rule h a -> Measurement h a
measureRule' p ((topH, topA) `Above` topTail)
    | p topH = Top (topH, topA)
    | otherwise = go topH topA topTail
  where
    go lh la (Above (h, a) t)
        | p h = Between (lh, la) (h, a)
        | otherwise = go h a t
    go _ _ (End a) = Bottom a
measureRule' _ (End a) = Bottom a

measureRule :: Ord h => h -> Rule h a -> Measurement h a
measureRule h =
    measureRule' (\hc -> h >= hc)

ruleElemHeight :: Eq a => (a -> Bool) -> Rule h a -> Maybe (Maybe h)
ruleElemHeight p (Above (h, a) t)
    | p a = Just (Just h)
    | otherwise = ruleElemHeight p t
ruleElemHeight p (End a)
    | p a = Just Nothing
    | otherwise = Nothing

ruleElems :: h -> Rule h a -> NE.NonEmpty (h, a)
ruleElems h (End a) = (h, a) NE.:| []
ruleElems he (Above (h, a) t) = (h, a) `NE.cons` ruleElems he t

data ChainwebVersion
    = ChainwebVersion
    { _versionCode :: ChainwebVersionCode
    , _versionName :: ChainwebVersionName
    , _versionGraphs :: Rule BlockHeight ChainGraph
    , _versionForks :: HashMap Fork (ChainMap BlockHeight)
    , _versionUpgrades :: ChainMap (HashMap BlockHeight [ChainwebTransaction])
    , _versionBlockRate :: BlockRate
    , _versionWindow :: Maybe WindowWidth
    , _versionHeaderBaseSizeBytes :: Natural
    , _versionMaxBlockGasLimit :: Rule BlockHeight (Maybe Natural)
    , _versionFakeFirstEpochStart :: Bool
    -- ^ The size in bytes of the constant portion of the serialized header. This is
    -- the header /without/ the adjacent hashes.
    --
    -- NOTE: This is internal. For the actual size of the serialized header
    -- use 'headerSizeBytes'.
    , _versionBootstraps :: [PeerInfo]
    , _versionGenesis :: ChainwebGenesis
    , _versionCheats :: Cheats
    }
    deriving stock (Generic, Show)
    deriving anyclass NFData

instance Eq ChainwebVersion where (==) = (==) `on` _versionCode
instance Ord ChainwebVersion where compare = compare `on` _versionCode

data Cheats = Cheats
    { _disablePow :: Bool
    , _disablePact :: Bool
    , _disablePeerValidation :: Bool
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, NFData)

data ChainwebGenesis = ChainwebGenesis
    { _genesisBlockTarget :: ChainMap HashTarget
    , _genesisBlockPayload :: ChainMap PayloadWithOutputs
    , _genesisTime :: ChainMap BlockCreationTime
    }
    deriving stock (Generic, Eq)
    deriving anyclass NFData

instance Show ChainwebGenesis where
    show _ = "<genesis>"

makeLensesWith (lensRules & generateUpdateableOptics .~ False) 'ChainwebVersion
makeLensesWith (lensRules & generateUpdateableOptics .~ False) 'ChainwebGenesis
makeLensesWith (lensRules & generateUpdateableOptics .~ False) 'Cheats

genesisBlockPayloadHash :: ChainwebVersion -> ChainId -> BlockPayloadHash
genesisBlockPayloadHash v cid = v ^?! versionGenesis . genesisBlockPayload . onChain cid . to _payloadWithOutputsPayloadHash

-- | Empty payload marking no-op transaction payloads for deprecated
-- versions.
--
emptyPayload :: PayloadWithOutputs
emptyPayload = PayloadWithOutputs mempty miner coinbase h i o
  where
    (BlockPayload h i o) = newBlockPayload miner coinbase mempty
    miner = MinerData $ encodeToByteString noMiner
    coinbase = noCoinbaseOutput

-- -- | The moment of creation of a Genesis Block. For test chains, this is the
-- -- Linux Epoch. Production chains are otherwise fixed to a specific timestamp.
-- --
-- genesisTime :: ChainwebVersion -> ChainId -> BlockCreationTime
-- genesisTime Test{} = BlockCreationTime epoch
-- genesisTime TimedConsensus{} = BlockCreationTime epoch
-- genesisTime PowConsensus{} = BlockCreationTime epoch
-- genesisTime TimedCPM{} = BlockCreationTime epoch
-- genesisTime FastTimedCPM{} = BlockCreationTime epoch
-- genesisTime Development{} = BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
-- genesisTime Testnet04 = BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
-- genesisTime Mainnet01 = BlockCreationTime [timeMicrosQQ| 2019-10-30T00:01:00.0 |]
-- --
-- -- TODO when Payload DB is finally loading genesis post-sync and post-pact,
-- -- the genesis block payload should be PayloadData, and PayloadWithOutputs
-- -- should have the TransactionTree and OutputTree to avoid recreating those
-- -- in PayloadStore.
-- genesisBlockPayload :: ChainwebVersion -> ChainId -> PayloadWithOutputs
-- -- Test Instances
-- genesisBlockPayload Test{} _ = emptyPayload
-- genesisBlockPayload TimedConsensus{} _ = emptyPayload
-- genesisBlockPayload PowConsensus{} _ = emptyPayload
-- genesisBlockPayload TimedCPM{} cid = case chainIdInt @Int cid of
--     0 -> TN0.payloadBlock
--     _ -> TNN.payloadBlock
-- genesisBlockPayload FastTimedCPM{} cid = case chainIdInt @Int cid of
--     0 -> TN0.payloadBlock
--     _ -> TNN.payloadBlock
-- -- Development Instances
-- genesisBlockPayload Development{} cid = case chainIdInt @Int cid of
--     0 -> DN0.payloadBlock
--     c | c >= 1, c <= 9 -> DNN.payloadBlock
--     c | c >= 10, c <= 19 -> DNKAD.payloadBlock
--     _ -> error "chainweb graph only supports a maximum of 20 chains - please review"
-- -- Production Instances
-- genesisBlockPayload Testnet04 cid = case chainIdInt @Int cid of
--     0 -> PN0.payloadBlock
--     _ -> PNN.payloadBlock
-- genesisBlockPayload Mainnet01 cid = case chainIdInt @Int cid of
--     0 -> MN0.payloadBlock
--     1 -> MN1.payloadBlock
--     2 -> MN2.payloadBlock
--     3 -> MN3.payloadBlock
--     4 -> MN4.payloadBlock
--     5 -> MN5.payloadBlock
--     6 -> MN6.payloadBlock
--     7 -> MN7.payloadBlock
--     8 -> MN8.payloadBlock
--     9 -> MN9.payloadBlock
--     c | c >= 10, c <= 19 -> MNKAD.payloadBlock
--     _ -> error "chainweb graph only supports a maximum of 20 chains - please review"

-- instance ToJSON ChainwebVersion where
--     toJSON c = object $
--         [ "forks" .= _versionForks c
--         , "blockRate" .= _getBlockRate (_versionBlockRate c)
--         ]
--         -- ++ maybeToList (("forkUpperBound" .=) <$> _versionForkUpperBound c)
-- instance FromJSON (ChainwebVersion -> ChainwebVersion) where
--     parseJSON = withObject "DevVersionConfig" $ \o -> do
--         jsonVersionForks <- o .:? "versionForks"
--         jsonBlockRate <- fmap BlockRate <$> o .:? "blockRate"
--         -- jsonForkUpperBound <- o .:? "forkUpperBound"
--         return $
--           \c -> c
--             { _versionForks = fromMaybe (_versionForks c) jsonVersionForks
--             , _versionBlockRate = fromMaybe (_versionBlockRate c) jsonBlockRate
--             -- , _versionForkUpperBound = jsonForkUpperBound <|> _versionForkUpperBound c
--             }

-- -- type ChainwebVersionCode = ChainwebVersion ()
-- -- type ChainwebVersion = ChainwebVersion DevVersionConfig
-- -- -------------------------------------------------------------------------- --
-- -- Chainweb Version

-- -- | Generally, a chain is uniquely identified by it's genesis block. For efficiency
-- -- and convenience we explicitely propagate 'ChainwebVersionCode and the 'ChainId'
-- -- to all blocks in the chain. At runtime the 'ChainId' is represented at
-- -- the type level (but included as value in serialized representations). Thus,
-- -- the ChainwebVersion identifies a chain at runtime at the value level.
-- --
-- -- We assume that values that are identified through different Chainweb
-- -- versions are not mixed at runtime. This is not enforced at the type level.
-- --

-- -- data ChainwebVersion dc
-- --     --------------------
-- --     -- TESTING INSTANCES
-- --     --------------------
-- --     = Test ChainGraph
-- --         -- ^ General-purpose test instance, where:
-- --         --
-- --         --   * the underlying `ChainGraph` is configurable,
-- --         --   * the genesis block time is the Linux epoch,
-- --         --   * each `HashTarget` is maxBound,
-- --         --   * each mining `Nonce` is constant,
-- --         --   * the creationTime of `BlockHeader`s is the parent time plus one second, and
-- --         --   * POW is simulated by poison process thread delay.
-- --         --
-- --         -- This is primarily used in unit tests.
-- --         --

-- --     | TimedConsensus ChainGraph ChainGraph
-- --         -- ^ Test instance for confirming the behaviour of our Consensus
-- --         -- mechanisms (Cut processing, Header validation, etc.), where:
-- --         --
-- --         --   * the underlying `ChainGraph` is configurable,
-- --         --   * the genesis block time is the Linux epoch,
-- --         --   * each `HashTarget` is maxBound,
-- --         --   * each mining `Nonce` is constant,
-- --         --   * the creationTime of `BlockHeader`s is the actual time,
-- --         --   * POW is simulated by poison process thread delay, and
-- --         --   * there are /no/ Pact or mempool operations running.
-- --         --
-- --         -- This is primarily used in our @slow-tests@ executable.
-- --         --

-- --     | PowConsensus ChainGraph
-- --         -- ^ Test instance for confirming the behaviour of the Proof-of-Work
-- --         -- mining algorithm and Difficulty Adjustment, where:
-- --         --
-- --         --   * the underlying `ChainGraph` is configurable,
-- --         --   * the genesis block time the current time,
-- --         --   * the genesis `HashTarget` is 7 bits lower than maxBound,
-- --         --   * the `Nonce` changes with each mining attempt,
-- --         --   * creationTime of BlockHeaders is the actual time, and
-- --         --   * there are /no/ Pact or mempool operations running.
-- --         --
-- --         -- This is primarily used in our @slow-tests@ executable.
-- --         --

-- --     | TimedCPM ChainGraph
-- --         -- ^ Test instance for confirming the combined behaviour of our Consensus
-- --         -- mechanisms, Pact code processing and validation, and Mempool, where:
-- --         --
-- --         --   * the underlying `ChainGraph` is configurable,
-- --         --   * the genesis block time is the Linux epoch,
-- --         --   * each `HashTarget` is maxBound,
-- --         --   * each mining `Nonce` is constant,
-- --         --   * the creationTime of `BlockHeader`s is the actual time,
-- --         --   * POW is simulated by poison process thread delay, and
-- --         --   * the Pact Service and Mempool operations are running.
-- --         --
-- --         -- This is primarily used in our @run-nodes@ executable.
-- --         --

-- --     | FastTimedCPM ChainGraph
-- --         -- ^ Test instance for confirming the combined behaviour of our Consensus
-- --         -- mechanisms, Pact code processing and validation, and Mempool, where:
-- --         --
-- --         -- * the underlying `ChainGraph` is configurable,
-- --         -- * the genesis block time is the Linux epoch,
-- --         -- * each `HashTarget` is maxBound,
-- --         -- * each mining `Nonce` is constant,
-- --         -- * the creationTime of `BlockHeader`'s is the actual time,
-- --         -- * POW is not simulated by poison process thread delay, and
-- --         -- * the Pact Service and Mempool operations are running.
-- --         --
-- --         -- This is primarily used in our @standalone@ executable.
-- --         --

-- --     ------------------------
-- --     -- DEVELOPMENT INSTANCES
-- --     ------------------------
-- --     | Development !dc
-- --         -- ^ An instance which has no guarantees about the long-term stability
-- --         -- of its parameters. They are free to change as developers require.

-- --     -----------------------
-- --     -- PRODUCTION INSTANCES
-- --     -----------------------
-- --     | Testnet04
-- --     | Mainnet01
-- --     deriving (Eq, Ord, Generic)
-- --     deriving anyclass (Hashable, NFData)

-- -- chainwebVersionTag :: ChainwebVersion dc -> ChainwebVersionCode
-- -- chainwebVersionTag (Test g) = Test g
-- -- chainwebVersionTag (TimedConsensus g1 g2) = TimedConsensus g1 g2
-- -- chainwebVersionTag (PowConsensus g) = PowConsensus g
-- -- chainwebVersionTag (TimedCPM g) = TimedCPM g
-- -- chainwebVersionTag (FastTimedCPM g) = FastTimedCPM g
-- -- chainwebVersionTag (Development _) = Development ()
-- -- chainwebVersionTag Testnet04 = Testnet04
-- -- chainwebVersionTag Mainnet01 = Mainnet01

-- -- versionConfig :: Traversal (ChainwebVersion c) (ChainwebVersion c') c c'
-- -- versionConfig _ (Test g) = pure $ Test g
-- -- versionConfig _ (TimedConsensus g1 g2) = pure $ TimedConsensus g1 g2
-- -- versionConfig _ (PowConsensus g) = pure $ PowConsensus g
-- -- versionConfig _ (TimedCPM g) = pure $ TimedCPM g
-- -- versionConfig _ (FastTimedCPM g) = pure $ FastTimedCPM g
-- -- versionConfig f (Development c) = Development <$> f c
-- -- versionConfig _ Testnet04 = pure Testnet04
-- -- versionConfig _ Mainnet01 = pure Mainnet01

-- -- instance Show ChainwebVersionCode where
-- --     show = T.unpack . chainwebVersionToText
-- --     {-# INLINE show #-}

-- -- instance Show ChainwebVersion where
--     -- showsPrec d (Test g) =
--     --   showParen (d > 10) $ showString "Test " . showsPrec 11 g
--     -- showsPrec d (TimedConsensus g1 g2) =
--     --   showParen (d > 10) $ showString "TimedConsensus " . showsPrec 11 g1 . showString " " . showsPrec 11 g2
--     -- showsPrec d (PowConsensus g) =
--     --   showParen (d > 10) $ showString "PowConsensus " . showsPrec 11 g
--     -- showsPrec d (TimedCPM g) =
--     --   showParen (d > 10) $ showString "TimedCPM " . showsPrec 11 g
--     -- showsPrec d (FastTimedCPM g) =
--     --   showParen (d > 10) $ showString "FastTimedCPM " . showsPrec 11 g
--     -- showsPrec d (Development dc) =
--     --   showParen (d > 10) $ showString "Development " . showsPrec 11 dc
--     -- showsPrec _ Testnet04 = showString "Testnet04"
--     -- showsPrec _ Mainnet01 = showString "Mainnet01"

-- -- | This function and its dual `fromChainwebVersionId` are used to efficiently
-- -- serialize a `ChainwebVersion` and its associated internal `ChainGraph` value.
-- -- __This function must be injective (one-to-one)!__ The scheme is as follows:
-- --
-- --   * Production `ChainwebVersion`s start from @0x00000001@ and count upwards.
-- --     Their value must be less than @0x8000000@, but this limit is unlikely to
-- --     ever be reached.
-- --
-- --   * `ChainwebVersion`s for testing begin at @0x80000000@, as can be seen in
-- --     `toTestChainwebVersion`. This value is combined (via `.|.`) with the
-- --     "code" of their associated `ChainGraph` (as seen in `graphToCode`). Such
-- --     codes start at @0x00010000@ and count upwards.
-- --
-- -- chainwebVersionId :: ChainwebVersion dc -> Word32
-- -- chainwebVersionId v@Test{} = toTestChainwebVersionId v
-- -- chainwebVersionId v@TimedConsensus{} = toTestChainwebVersionId v
-- -- chainwebVersionId v@PowConsensus{} = toTestChainwebVersionId v
-- -- chainwebVersionId v@TimedCPM{} = toTestChainwebVersionId v
-- -- chainwebVersionId v@FastTimedCPM{} = toTestChainwebVersionId v
-- -- chainwebVersionId Development{} = 0x00000001
-- -- chainwebVersionId Testnet04 = 0x00000007
-- -- chainwebVersionId Mainnet01 = 0x00000005
-- -- {-# INLINABLE chainwebVersionId #-}

-- -- fromChainwebVersionId :: HasCallStack => Word32 -> ChainwebVersionCode
-- -- fromChainwebVersionId 0x00000001 = Development ()
-- -- fromChainwebVersionId 0x00000007 = Testnet04
-- -- fromChainwebVersionId 0x00000005 = Mainnet01
-- -- fromChainwebVersionId i = fromTestChainwebVersionId i
-- -- {-# INLINABLE fromChainwebVersionId #-}

encodeChainwebVersionCode :: ChainwebVersionCode -> Put
encodeChainwebVersionCode = putWord32le . getChainwebVersionCode
{-# INLINE encodeChainwebVersionCode #-}

decodeChainwebVersionCode :: Get ChainwebVersionCode
decodeChainwebVersionCode = ChainwebVersionCode <$> getWord32le
{-# INLINE decodeChainwebVersionCode #-}

-- instance ToJSON ChainwebVersionCode where
--     toJSON = toJSON . chainwebVersionToText
--     toEncoding = toEncoding . chainwebVersionToText
--     {-# INLINE toJSON #-}
--     {-# INLINE toEncoding #-}

-- instance FromJSON ChainwebVersionCode where
--     parseJSON = parseJsonFromText "ChainwebVersion"
--     {-# INLINE parseJSON #-}

-- instance ToJSON ChainwebVersion where
--     toJSON v
--         | chainwebVersionTag v == chainwebVersionTag mainnet
--         = String "mainnet"
--         | otherwise = undefined

-- instance FromJSON ChainwebVersion where
--     parseJSON v =
--         parseConfigured v <|> parseOnlyTag
--       where
--         parseConfigured =
--             withObject "ChainwebVersion" $ \o -> do
--               tag <- o .: "tag"
--               conf <- o .:? "configuration"
--               return $ case tag :: ChainwebVersionCode of
--                 Test g -> Test g
--                 TimedConsensus g1 g2 -> TimedConsensus g1 g2
--                 PowConsensus g -> PowConsensus g
--                 TimedCPM g -> TimedCPM g
--                 FastTimedCPM g -> FastTimedCPM g
--                 Development () ->
--                     Development (maybe defaultDevVersionConfig ($ defaultDevVersionConfig) conf)
--                 Testnet04 -> Testnet04
--                 Mainnet01 -> Mainnet01
--         parseOnlyTag =
--             (versionConfig .~ defaultDevVersionConfig) <$> parseJSON @ChainwebVersionCode v

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ChainwebVersionCode where
    type Tag ChainwebVersionCode = 'ChainwebVersionTag
    toMerkleNode = encodeMerkleInputNode encodeChainwebVersionCode
    fromMerkleNode = decodeMerkleInputNode decodeChainwebVersionCode
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- -- chainwebVersionToText :: HasCallStack => ChainwebVersion dc -> T.Text
-- -- chainwebVersionToText Development{} = "development"
-- -- chainwebVersionToText Testnet04 = "testnet04"
-- -- chainwebVersionToText Mainnet01 = "mainnet01"
-- -- chainwebVersionToText (Test g) = "test-" <> toText g
-- -- chainwebVersionToText (TimedConsensus g1 g2) = "timedConsensus-" <> toText g1 <> "-" <> toText g2
-- -- chainwebVersionToText (PowConsensus g) =  "powConsensus-" <> toText g
-- -- chainwebVersionToText (TimedCPM g) =  "timedCPM-" <> toText g
-- -- chainwebVersionToText (FastTimedCPM g) =  "fastTimedCPM-" <> toText g
-- {-# INLINABLE chainwebVersionToText #-}

-- -- | Read textual representation of a `ChainwebVersion`.
-- --
-- -- NOTE: This doesn't warn of incomplete pattern matches upon the addition of a
-- -- new `ChainwebVersion` value!
-- --
-- -- chainwebVersionFromText :: MonadThrow m => T.Text -> m ChainwebVersionCode
-- -- chainwebVersionFromText "development" = pure (Development ())
-- -- chainwebVersionFromText "testnet04" = pure Testnet04
-- -- chainwebVersionFromText "mainnet01" = pure Mainnet01
-- -- chainwebVersionFromText t = case T.splitOn "-" t of
-- --     [ "test", g ] -> Test <$> fromText g
-- --     [ "timedConsensus", g1, g2 ] ->  TimedConsensus <$> fromText g1 <*> fromText g2
-- --     [ "powConsensus", g ] -> PowConsensus <$> fromText g
-- --     [ "timedCPM", g ] -> TimedCPM <$> fromText g
-- --     [ "fastTimedCPM", g ] -> FastTimedCPM <$> fromText g
-- --     _ -> throwM . TextFormatException $ "Unknown Chainweb version: " <> t

instance HasTextRepresentation ChainwebVersionName where
    toText = getChainwebVersionName
    {-# INLINE CONLIKE toText #-}
    fromText = pure . ChainwebVersionName
    {-# INLINE CONLIKE fromText #-}

-- -- -------------------------------------------------------------------------- --
-- -- Test instances
-- --
-- -- The code in this section must not be called in production.
-- --

-- data GraphPos = P1 | P2 deriving (Bounded, Enum)

-- graphToCodeN :: GraphPos -> KnownGraph -> Word32
-- graphToCodeN p g = shiftL (graphToCode g) (4 * (4 + fromEnum p))
--   where
--     graphToCode :: KnownGraph -> Word32
--     graphToCode Singleton = 0x00000001
--     graphToCode Pair = 0x00000002
--     graphToCode Triangle = 0x00000003
--     graphToCode Peterson = 0x00000004
--     graphToCode Twenty = 0x00000005
--     graphToCode HoffmanSingleton = 0x00000006

-- codeToGraphN :: HasCallStack => GraphPos -> Word32 -> KnownGraph
-- codeToGraphN p c = codeToGraph (shiftR c (4 * (4 + fromEnum p)) .&. 0x0000000f)
--   where
--     codeToGraph :: HasCallStack => Word32 -> KnownGraph
--     codeToGraph 0x00000001 = Singleton
--     codeToGraph 0x00000002 = Pair
--     codeToGraph 0x00000003 = Triangle
--     codeToGraph 0x00000004 = Peterson
--     codeToGraph 0x00000005 = Twenty
--     codeToGraph 0x00000006 = HoffmanSingleton
--     codeToGraph _ = error "Unknown Graph Code"

-- -- toTestChainwebVersionId :: HasCallStack => ChainwebVersion dc -> Word32
-- -- toTestChainwebVersionId (Test g) = 0x80000000
-- --     .|. graphToCodeN P1 (view chainGraphKnown g)
-- -- toTestChainwebVersionId (TimedConsensus g1 g2) = 0x80000001
-- --     .|. graphToCodeN P1 (view chainGraphKnown g1)
-- --     .|. graphToCodeN P2 (view chainGraphKnown g2)
-- -- toTestChainwebVersionId (PowConsensus g) = 0x80000002
-- --     .|. graphToCodeN P1 (view chainGraphKnown g)
-- -- toTestChainwebVersionId (TimedCPM g) = 0x80000003
-- --     .|. graphToCodeN P1 (view chainGraphKnown g)
-- -- toTestChainwebVersionId (FastTimedCPM g) = 0x80000004
-- --     .|. graphToCodeN P1 (view chainGraphKnown g)
-- -- toTestChainwebVersionId Development{} =
-- --     error "Illegal ChainwebVersion passed to toTestChainwebVersion"
-- -- toTestChainwebVersionId Testnet04 =
-- --     error "Illegal ChainwebVersion passed to toTestChainwebVersion"
-- -- toTestChainwebVersionId Mainnet01 =
-- --     error "Illegal ChainwebVersion passed to toTestChainwebVersion"

-- -- fromTestChainwebVersionId :: HasCallStack => Word32 -> ChainwebVersionCode
-- -- fromTestChainwebVersionId c = case v of
-- --     0x80000000 -> Test (knownChainGraph $ codeToGraphN P1 g)
-- --     0x80000001 -> TimedConsensus
-- --         (knownChainGraph $ codeToGraphN P1 g)
-- --         (knownChainGraph $ codeToGraphN P2 g)
-- --     0x80000002 -> PowConsensus (knownChainGraph $ codeToGraphN P1 g)
-- --     0x80000003 -> TimedCPM (knownChainGraph $ codeToGraphN P1 g)
-- --     0x80000004 -> FastTimedCPM (knownChainGraph $ codeToGraphN P1 g)
-- --     _ -> error "Unknown ChainwebVersion Code"
-- --   where
-- --     (v, g) = (0xf000ffff .&. c, 0x0fff0000 .&. c)

-- -- -------------------------------------------------------------------------- --
-- -- Type level ChainwebVersion

newtype ChainwebVersionT = ChainwebVersionT Symbol

data SomeChainwebVersionT = forall (a :: ChainwebVersionT)
        . KnownChainwebVersionSymbol a => SomeChainwebVersionT (Proxy a)

class KnownSymbol (ChainwebVersionSymbol n) => KnownChainwebVersionSymbol (n :: ChainwebVersionT) where
    type ChainwebVersionSymbol n :: Symbol
    chainwebVersionSymbolVal :: Proxy n -> T.Text

instance (KnownSymbol n) => KnownChainwebVersionSymbol ('ChainwebVersionT n) where
    type ChainwebVersionSymbol ('ChainwebVersionT n) = n
    chainwebVersionSymbolVal _ = T.pack $ symbolVal (Proxy @n)

someChainwebVersionVal :: ChainwebVersionName -> SomeChainwebVersionT
someChainwebVersionVal v = case someSymbolVal (sshow v) of
    (SomeSymbol (Proxy :: Proxy v)) -> SomeChainwebVersionT (Proxy @('ChainwebVersionT v))

-- -- -------------------------------------------------------------------------- --
-- -- Singletons

data instance Sing (v :: ChainwebVersionT) where
    SChainwebVersion :: KnownChainwebVersionSymbol v => Sing v

type SChainwebVersion (v :: ChainwebVersionT) = Sing v

instance KnownChainwebVersionSymbol v => SingI (v :: ChainwebVersionT) where
    sing = SChainwebVersion

instance SingKind ChainwebVersionT where
    type Demote ChainwebVersionT = ChainwebVersionName

    fromSing (SChainwebVersion :: Sing v) = unsafeFromText
        . chainwebVersionSymbolVal $ Proxy @v

    toSing n = case someChainwebVersionVal n of
        SomeChainwebVersionT p -> SomeSing (singByProxy p)

    {-# INLINE fromSing #-}
    {-# INLINE toSing #-}

pattern FromSingChainwebVersion :: Sing (n :: ChainwebVersionT) -> ChainwebVersionName
pattern FromSingChainwebVersion sng <- ((\v -> withSomeSing v SomeSing) -> SomeSing sng)
  where FromSingChainwebVersion sng = fromSing sng
{-# COMPLETE FromSingChainwebVersion #-}

-- -- -------------------------------------------------------------------------- --
-- -- HasChainwebVersion Class
--
class HasChainwebVersionCode a where
    _chainwebVersionCode :: a -> ChainwebVersionCode
    _chainwebVersionCode = view chainwebVersionCode
    {-# INLINE _chainwebVersionCode #-}

    chainwebVersionCode :: Getter a ChainwebVersionCode
    chainwebVersionCode = to _chainwebVersionCode
    {-# INLINE chainwebVersionCode #-}

    {-# MINIMAL _chainwebVersionCode | chainwebVersionCode #-}

class HasChainwebVersion a where
    _chainwebVersion :: a -> ChainwebVersion
    _chainwebVersion = view chainwebVersion
    {-# INLINE _chainwebVersion #-}

    chainwebVersion :: Getter a ChainwebVersion
    chainwebVersion = to _chainwebVersion
    {-# INLINE chainwebVersion #-}

    {-# MINIMAL _chainwebVersion | chainwebVersion #-}

instance HasChainwebVersion ChainwebVersion where
    _chainwebVersion = id
    {-# INLINE _chainwebVersion #-}

instance HasChainwebVersionCode ChainwebVersionCode where
    _chainwebVersionCode = id
    {-# INLINE _chainwebVersionCode #-}

instance HasChainwebVersionCode ChainwebVersion where
    _chainwebVersionCode = _versionCode
    {-# INLINE _chainwebVersionCode #-}

-- | All known chainIds. This includes chains that are not yet "active".
--
chainIds :: HasChainwebVersion v => v -> HS.HashSet ChainId
chainIds = graphChainIds . snd . ruleHead . _versionGraphs . _chainwebVersion
{-# INLINE chainIds #-}

forkUpgrades
    :: ChainwebVersion
    -> [(Fork, ChainMap [ChainwebTransaction])]
    -> ChainMap (HashMap BlockHeight [ChainwebTransaction])
forkUpgrades v = OnChains . foldl' go (HM.empty <$ HS.toMap (chainIds v))
    where
    go acc (fork, txsPerChain) =
        HM.unionWith HM.union acc newTxs
        where
        newTxs = HM.fromList $
            [ (cid, HM.singleton forkHeight txs)
            | cid <- HM.keys acc
            , Just txs <- [txsPerChain ^? onChain cid]
            , let forkHeight = v ^?! versionForks . at fork . _Just . onChain cid
            ]

mkChainId
    :: MonadThrow m
    => HasChainwebVersion v
    => Integral i
    => v
    -> BlockHeight
    -> i
    -> m ChainId
mkChainId v h i = cid
    <$ checkWebChainId (chainGraphAt (_chainwebVersion v) h) cid
  where
    cid = unsafeChainId (fromIntegral i)
{-# INLINE mkChainId #-}

-- -- -------------------------------------------------------------------------- --
-- -- Properties of Chainweb Versions
-- -- -------------------------------------------------------------------------- --

-- -- -------------------------------------------------------------------------- --
-- -- Graph

-- -- | Graphs of chainweb version
-- --
-- -- Invariants:
-- --
-- -- * Entries are sorted by 'BlockHeight' in decreasing order.
-- -- * The last entry is for 'BlockHeight' 0.
-- -- * The graphs decrease in order.
-- --
-- -- The functions provided in 'Chainweb.Version.Utils' are generally more
-- -- convenient to use than this function.
-- --
-- chainwebGraphs :: ChainwebVersion dc -> NE.NonEmpty (BlockHeight, ChainGraph)
-- chainwebGraphs (Test g) = pure (0, g)
-- chainwebGraphs (TimedConsensus g1 g2) = (8, g2) NE.:| [ (0, g1) ]
-- chainwebGraphs (PowConsensus g) = pure (0, g)
-- chainwebGraphs (TimedCPM g) = pure (0, g)
-- chainwebGraphs (FastTimedCPM g) = pure (0, g)
-- chainwebGraphs Testnet04 =
--     ( to20ChainsTestnet, twentyChainGraph ) NE.:|
--     [ ( 0, petersonChainGraph ) ]
-- chainwebGraphs Mainnet01 =
--     ( to20ChainsMainnet, twentyChainGraph ) NE.:|
--     [ ( 0, petersonChainGraph ) ]
-- chainwebGraphs Development {} =
--     ( to20ChainsDevelopment, twentyChainGraph ) NE.:|
--     [ ( 0, petersonChainGraph ) ]
-- {-# INLINE chainwebGraphs #-}

-- | Return the Graph History at a given block height in descending order.
--
-- The functions provided in 'Chainweb.Version.Utils' are generally more
-- convenient to use than this function.
--
-- This function is safe because of invariants provided by 'chainwebGraphs'.
-- (There are also unit tests the confirm this.)
--
chainwebGraphsAt
    :: HasCallStack
    => ChainwebVersion
    -> BlockHeight
    -> Rule BlockHeight ChainGraph
chainwebGraphsAt v h = ruleDropWhile (> h)
    $ _versionGraphs v
{-# INLINE chainwebGraphsAt #-}

-- | The 'ChainGraph' for the given 'BlockHeight'
--
chainGraphAt :: HasCallStack => ChainwebVersion -> BlockHeight -> ChainGraph
chainGraphAt v = snd . ruleHead . chainwebGraphsAt v
-- {-# INLINE chainGraphAt #-}

-- | The 'ChainGraph' for the given 'BlockHeight'
--
chainGraphAt_
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> BlockHeight
    -> ChainGraph
chainGraphAt_ = chainGraphAt . _chainwebVersion
{-# INLINE chainGraphAt_ #-}

instance HasChainGraph (ChainwebVersion, BlockHeight) where
    _chainGraph = uncurry chainGraphAt
    {-# INLINE _chainGraph #-}

-- | By definition, Genesis Blocks are "mined" on the easiest difficulty. No
-- subsequent block mining can have a `HashTarget` easier (re: higher) than
-- this. Equivalent to `maxTarget`.
--
-- When the graph is extended new chains should "enter" with a non-trivial
-- difficulty in order to avoid races and resulting forks during the first two
-- or three difficulty adjustement epochs.
--
-- On devnet, using maxTarget results in a too high block production and
-- consecutively orphans and network congestion. The consequence are
-- osciallations to take serval hundred blocks before the system stabilizes.
-- This setting cools down initial block production.
--
-- TODO: move this and the following definitions to Chainweb.Version (or a
-- submodule of Chainweb.Version`).
--
-- genesisBlockTarget :: ChainwebVersion -> ChainId -> HashTarget
-- genesisBlockTarget v@Mainnet01 cid
--     | genesisHeight v cid > 731382 = mainnet20InitialHashTarget
-- genesisBlockTarget v@Testnet04 cid
--     | genesisHeight v cid > 278626 = testnet20InitialHashTarget
-- genesisBlockTarget v@(Development{}) cid
--     | genesisHeight v cid > to20ChainsDevelopment - min to20ChainsDevelopment 10 =
--         HashTarget 0x0000088f99632cadf39b0db7655be62cb7dbc84ebbd9a90e5b5756d3e7d9196c
--             -- 4 * 10 node-mining
--     | otherwise = HashTarget (maxBound `div` 100000)
-- genesisBlockTarget _ _ = maxTarget


-- -- | The Proof-of-Work `BlockRate` for each `ChainwebVersion`. This is the
-- -- number of seconds we expect to pass while a miner mines on various chains,
-- -- eventually succeeding on one.
-- --
-- blockRate :: ChainwebVersion -> BlockRate
-- blockRate Test{} = BlockRate 0
-- blockRate TimedConsensus{} = BlockRate 4
-- blockRate PowConsensus{} = BlockRate 10
-- blockRate TimedCPM{} = BlockRate 4
-- blockRate FastTimedCPM{} = BlockRate 1
-- -- 120 blocks per hour, 2,880 per day, 20,160 per week, 1,048,320 per year.
-- blockRate Testnet04 = BlockRate 30
-- blockRate Mainnet01 = BlockRate 30
-- blockRate (Development vc) = _versionBlockRate vc


-- -- | The Proof-of-Work `WindowWidth` for each `ChainwebVersion`. For chainwebs
-- -- that do not expect to perform POW, this should be `Nothing`.
-- --
-- window :: ChainwebVersion dc -> Maybe WindowWidth
-- window Test{} = Nothing
-- window TimedConsensus{} = Nothing
-- -- 5 blocks, should take 50 seconds.
-- window PowConsensus{} = Just $ WindowWidth 8
-- window TimedCPM{} = Nothing
-- window FastTimedCPM{} = Nothing
-- -- 120 blocks, should take 1 hour given a 30 second BlockRate.
-- window Development{} = Just $ WindowWidth 120
-- -- 120 blocks, should take 1 hour given a 30 second BlockRate.
-- window Testnet04 = Just $ WindowWidth 120
-- window Mainnet01 = Just $ WindowWidth 120

-- -- -------------------------------------------------------------------------- --
-- -- Header Serialization

-- --

maxBlockGasLimit :: ChainwebVersion -> BlockHeight -> Maybe Natural
maxBlockGasLimit v bh = case measureRule bh $ _versionMaxBlockGasLimit v of
    Bottom limit -> limit
    Top (_, limit) -> limit
    Between (_, limit) _ -> limit

-- findLastFork :: ChainwebVersion -> ChainId -> BlockHeight -> (BlockHeight, ForkEvent)
-- findLastFork v cid bh = case measureRule bh (_versionForks v ^?! onChain cid) of
--     Top (h, a) -> (h, a)
--     Between (h, a) _ -> (h, a)
--     Bottom a -> (BlockHeight 0, a)

-- atFork :: Fork -> ChainwebVersion -> ChainId -> BlockHeight -> Maybe ForkEvent
-- atFork f v cid h =
--     ForkEvent f txs <$ guard (lastForkHeight == h && lastFork == f && not (null txs))
--   where
--     (lastForkHeight, (ForkEvent lastFork txs)) = findLastFork v cid h

-- beforeFork :: Fork -> ChainwebVersion -> ChainId -> BlockHeight -> Bool
-- beforeFork f v cid h =
--     eventFork event < f
--   where
--     (_, event) = findLastFork v cid h

-- afterFork :: Fork -> ChainwebVersion -> ChainId -> BlockHeight -> Bool
-- afterFork f v cid h =
--     lastFork > f || lastFork == f && h > lastForkHeight
--   where
--     (lastForkHeight, ForkEvent lastFork _) = findLastFork v cid h

-- atOrAfterFork :: Fork -> ChainwebVersion -> ChainId -> BlockHeight -> Bool
-- atOrAfterFork f v cid h =
--     lastFork >= f
--   where
--     (_, ForkEvent lastFork _) = findLastFork v cid h

getForkHeight :: Fork -> ChainwebVersion -> BlockHeight
getForkHeight fork v = getForkHeight' fork v (unsafeChainId 0)

getForkHeight' :: Fork -> ChainwebVersion -> ChainId -> BlockHeight
getForkHeight' fork v cid = v ^?! versionForks . at fork . _Just . onChain cid

checkFork'
    :: (BlockHeight -> BlockHeight -> Bool)
    -> Fork
    -> ChainwebVersion -> ChainId -> BlockHeight -> Bool
checkFork' p f v cid h = p h (getForkHeight' f v cid)

checkFork
    :: (BlockHeight -> BlockHeight -> Bool)
    -> Fork
    -> ChainwebVersion -> BlockHeight -> Bool
checkFork p f v h = checkFork' p f v (unsafeChainId 0) h

vuln797Fix
    :: ChainwebVersion
    -> ChainId
    -> BlockHeight
    -> Bool
vuln797Fix = checkFork' (>=) Vuln797Fix
{-# INLINE vuln797Fix #-}

pactBackCompat_v16 :: ChainwebVersion -> BlockHeight -> Bool
pactBackCompat_v16 = checkFork (<) PactBackCompat_v16
{-# INLINE pactBackCompat_v16 #-}

skipTxTimingValidation :: ChainwebVersion -> BlockHeight -> Bool
skipTxTimingValidation = checkFork (<) SkipTxTimingValidation
{-# INLINE skipTxTimingValidation #-}

enableModuleNameFix :: ChainwebVersion -> BlockHeight -> Bool
enableModuleNameFix = checkFork (>=) ModuleNameFix
{-# INLINE enableModuleNameFix #-}

enableModuleNameFix2 :: ChainwebVersion -> BlockHeight -> Bool
enableModuleNameFix2 = checkFork (>=) ModuleNameFix2
{-# INLINE enableModuleNameFix2 #-}

enablePactEvents :: ChainwebVersion -> BlockHeight -> Bool
enablePactEvents = checkFork (>=) PactEvents
{-# INLINE enablePactEvents #-}

enableSPVBridge :: ChainwebVersion -> BlockHeight -> Bool
enableSPVBridge = checkFork (>=) SPVBridge
{-# INLINE enableSPVBridge #-}

enforceKeysetFormats :: ChainwebVersion -> BlockHeight -> Bool
enforceKeysetFormats = checkFork (>=) EnforceKeysetFormats
{-# INLINE enforceKeysetFormats #-}

doCheckTxHash :: ChainwebVersion -> BlockHeight -> Bool
doCheckTxHash = checkFork (>=) CheckTxHash
{-# INLINE doCheckTxHash #-}

pact44NewTrans :: ChainwebVersion -> BlockHeight -> Bool
pact44NewTrans = checkFork (>=) Pact44NewTrans
{-# INLINE pact44NewTrans #-}

slowEpochGuard
    :: ChainwebVersion
    -> BlockHeight
        -- ^ BlockHeight of parent Header
    -> Bool
slowEpochGuard = checkFork (<) SlowEpoch
{-# INLINE slowEpochGuard #-}

oldTargetGuard :: ChainwebVersion -> BlockHeight -> Bool
oldTargetGuard = checkFork (<) OldTargetGuard
{-# INLINE oldTargetGuard #-}

skipFeatureFlagValidationGuard :: ChainwebVersion -> BlockHeight -> Bool
skipFeatureFlagValidationGuard = checkFork (<) SkipFeatureFlagValidation
{-# INLINE skipFeatureFlagValidationGuard #-}

oldDaGuard :: ChainwebVersion -> BlockHeight -> Bool
oldDaGuard = checkFork (<) OldDAGuard
{-# INLINE oldDaGuard #-}

pactParserVersion :: ChainwebVersion -> ChainId -> BlockHeight -> PactParserVersion
pactParserVersion v _cid bh
    | chainweb213Pact v bh = PactParserChainweb213
    | otherwise = PactParserGenesis

-- -------------------------------------------------------------------------- --
-- Pact Validation Parameters

-- | This the hard upper limit of the gas within a block. Blocks that use more
-- gas are invalid and rejected. This limit is needed as a DOS protection.
--
-- Smaller limits can be configured for creating new blocks.
--
-- Before the chainweb-node 2.16 fork, there was no maximum block gas limit.
--
-- maxBlockGasLimit
--     :: ChainwebVersion dc
--     -> ChainId
--     -> BlockHeight
--     -> Maybe Natural
-- maxBlockGasLimit v _ bh = M.lookupLE (versionMaxBlockGasLimit v
-- maxBlockGasLimit Mainnet01 _ bh = 180000 <$ guard (chainweb216Pact GT Mainnet01 bh)
-- maxBlockGasLimit Testnet04 _ bh = 180000 <$ guard (chainweb216Pact GT Testnet04 bh)
-- maxBlockGasLimit Development{} _ _ = Just 180000
-- maxBlockGasLimit _ _ _ = Just 2_000000

-- -------------------------------------------------------------------------- --
-- Pact Validation Guards
--


-- | Mainnet applied vuln797Fix at @[timeMicrosQQ| 2019-12-10T21:00:00.0 |]@.
--
-- This function provides the block heights when the fix became effective on the
-- respective chains.
--

-- | Mainnet upgraded to coin v2 at time at @[timeMicrosQQ| 2019-12-17T15:00:00.0 |]@.
--
-- This function provides the block heights when coin v2 became effective on the
-- respective chains.
--
-- coinV2Upgrade
--     :: ChainwebVersion
--     -> ChainId
--     -> BlockHeight
--     -> Bool
-- coinV2Upgrade = atFork CoinV2
-- coinV2Upgrade Mainnet01 cid h
--     | cid == unsafeChainId 0 = h == 140808
--     | cid == unsafeChainId 1 = h == 140809
--     | cid == unsafeChainId 2 = h == 140808
--     | cid == unsafeChainId 3 = h == 140809
--     | cid == unsafeChainId 4 = h == 140808
--     | cid == unsafeChainId 5 = h == 140808
--     | cid == unsafeChainId 6 = h == 140808
--     | cid == unsafeChainId 7 = h == 140809
--     | cid == unsafeChainId 8 = h == 140808
--     | cid == unsafeChainId 9 = h == 140808
--     -- new chains on mainnet start already with v2 deployed in the genesis block
-- coinV2Upgrade Testnet04 cid h
--     | chainIdInt @Int cid >= 10  && chainIdInt @Int cid < 20 = h == 337000
--     | otherwise = h == 1
-- coinV2Upgrade Development{} cid h
--     | cid == unsafeChainId 0 = h == 3
--     | otherwise = h == 4
-- coinV2Upgrade _ _ 1 = True
-- coinV2Upgrade _ _ _ = False

-- | 20-chain rebalance
--
-- to20ChainRebalance
--     :: ChainwebVersion
--     -> ChainId
--     -> BlockHeight
--     -> Bool
-- to20ChainRebalance v _ h = case heightMapLastNext (max h 1 - 1) (_versionGraphs v) of
    -- LastNext () ((Just _, lastGraph), Just (h', nextGraph)) | h' == h -> undefined
-- to20ChainRebalance Mainnet01 _ h = h == to20ChainsMainnet
-- to20ChainRebalance Testnet04 _ h = h == to20ChainsTestnet
-- to20ChainRebalance (Development vc) _ h = forked At To20Chains vc h
-- to20ChainRebalance _ _ 2 = True
-- to20ChainRebalance _ _ _ = False

-- pactBackCompat_v16 Mainnet01 h = h < 328000
-- pactBackCompat_v16 _ _ = False

-- skipTxTimingValidation Mainnet01 h = h < 449940 -- ~ 2020-04-03T00:00:00Z
-- skipTxTimingValidation _ h = h <= 1
--     -- For most chainweb versions there is a large gap between creation times of
--     -- the genesis blocks and the corresponding first blocks.
--     --
--     -- Some tests fake block heights without updating pdData appropriately. This
--     -- causes tx validation at height 1, even though the block height is larger.
--     -- By using the current header time for the block of height <= 1 we relax
--     -- the tx timing checks a bit.

-- --
-- enableModuleNameFix Mainnet01 bh = bh >= 448501 -- ~ 2020-04-02T12:00:00Z
-- enableModuleNameFix _ bh = bh >= 2

-- --
-- enableModuleNameFix2 Mainnet01 bh = bh >= 752214 -- ~ 2020-07-17 0:00:00 UTC
-- enableModuleNameFix2 Testnet04 bh = bh >= 289966 -- ~ 2020-07-13
-- enableModuleNameFix2 _ bh = bh >= 2

-- enablePactEvents Mainnet01 bh = bh >= 1138000
-- enablePactEvents Testnet04 bh = bh >= 660000
-- enablePactEvents (FastTimedCPM g) bh
--     | g == singletonChainGraph || g == pairChainGraph = True
--     | g == petersonChainGraph = bh > 10
--     | otherwise = False
-- enablePactEvents _ bh = bh >= 2



-- enableSPVBridge Mainnet01 = (>= 1_275_000) -- 2021-01-14T16:35:58
-- enableSPVBridge Testnet04 = (>= 820_000) -- 2021-01-14T17:12:02
-- enableSPVBridge (FastTimedCPM g) = const $ g == pairChainGraph || g == petersonChainGraph
-- enableSPVBridge _ = const True

-- | Pact 4 / coin v3 fork
-- pact4coin3Upgrade :: Ordering -> ChainwebVersion -> BlockHeight -> Bool
-- pact4coin3Upgrade
-- pact4coin3Upgrade aoa v h = case aoa of
--     EQ -> go (==) v h
--     GT -> go (<) v h
--     LT -> go (>) v h
--   where
--     go f Mainnet01 = f 1_722_500 -- 2021-06-19T03:34:05
--     go f Testnet04 = f 1_261_000 -- 2021-06-17T15:54:14
--     go _ (Development vc) = forked aoa Pact4Coin3 vc
--     go f (FastTimedCPM g) | g == petersonChainGraph = f 20
--     go f _ = f 4
--     -- lowering this number causes some tests in Test.Pact.SPV to fail

-- pact420Upgrade :: ChainwebVersion -> BlockHeight -> Bool
-- pact420Upgrade Mainnet01 = (>= 2_334_500) -- 2022-01-17T17:51:12
-- pact420Upgrade Testnet04 = (>= 1_862_000) -- 2022-01-13T16:11:10
-- pact420Upgrade (Development vc) = forked GT Pact420 vc
-- pact420Upgrade (FastTimedCPM g) | g == petersonChainGraph = (>= 5)
-- pact420Upgrade _ = const True

-- enforceKeysetFormats Mainnet01 = (>= 2_162_000) -- 2021-11-18T20:06:55
-- enforceKeysetFormats Testnet04 = (>= 1_701_000) -- 2021-11-18T17:54:36
-- enforceKeysetFormats (FastTimedCPM g) | g == petersonChainGraph = (>= 10)
-- enforceKeysetFormats _ = const True
--
-- doCheckTxHash Mainnet01 = (>= 2_349_800) -- 2022-01-23T02:53:38
-- doCheckTxHash Testnet04 = (>= 1_889_000) -- 2022-01-24T04:19:24
-- doCheckTxHash (FastTimedCPM g) | g == petersonChainGraph = (>= 7)
-- doCheckTxHash _ = const True

-- | Pact changes for Chainweb 2.13
--
chainweb213Pact :: ChainwebVersion -> BlockHeight -> Bool
chainweb213Pact = checkFork (>=) Chainweb213Pact
-- chainweb213Pact Mainnet01 = (>= 2_447_315) -- 2022-02-26 00:00:00
-- chainweb213Pact Testnet04 = (>= 1_974_556) -- 2022-02-25 00:00:00
-- chainweb213Pact (Development vc) = forked GT Chainweb213Pact vc
-- chainweb213Pact (FastTimedCPM g) | g == petersonChainGraph = (> 25)
-- chainweb213Pact _ = const True

-- -- | Fork for musl trans funs
-- pact44NewTrans Mainnet01 = (>= 2_965_885) -- Todo: add date
-- pact44NewTrans Testnet04 = (>= 2_500_369) -- Todo: add date
-- pact44NewTrans _ = const True


-- -- | Pact and coin contract changes for Chainweb 2.14
-- --
-- chainweb214Pact
--     :: Ordering
--     -> ChainwebVersion
--     -> BlockHeight
--     -> Bool
-- chainweb214Pact aoa v h = case aoa of
--     EQ -> go (==) v h
--     GT -> go (<) v h
--     LT -> go (>) v h
--   where
--     go f Mainnet01 = f 2605663 -- 2022-04-22T00:00:00Z
--     go f Testnet04 = f 2134331 -- 2022-04-21T12:00:00Z
--     go _ (Development vc) = forked aoa Chainweb214Pact vc
--     go f (FastTimedCPM g) | g == petersonChainGraph = f 30
--     go f _ = f 5

-- -- | Pact and coin contract changes for Chainweb 2.15
-- --
-- chainweb215Pact
--     :: Ordering
--     -> ChainwebVersion
--     -> BlockHeight
--     -> Bool
-- chainweb215Pact aoa v h = case aoa of
--     EQ -> go (==) v h
--     GT -> go (<) v h
--     LT -> go (>) v h
--   where
--     go f Mainnet01 = f 2766630 -- 2022-06-17T00:00:00+00:00
--     go f Testnet04 = f 2295437 -- 2022-06-16T12:00:00+00:00
--     go _ (Development vc) = forked aoa Chainweb215Pact vc
--     go f (FastTimedCPM g) | g == petersonChainGraph = f 35
--     go f _ = f 10

-- -- | Pact and coin contract changes for Chainweb 2.16
-- --
-- chainweb216Pact
--     :: Ordering
--     -> ChainwebVersion
--     -> BlockHeight
--     -> Bool
-- chainweb216Pact aoa v h = case aoa of
--     EQ -> go (==) v h
--     GT -> go (<) v h
--     LT -> go (>) v h
--   where
--     go f Mainnet01 = f 2988324 -- 2022-09-02 00:00:00+00:00
--     go f Testnet04 = f 2516739 -- 2022-09-01 12:00:00+00:00
--     go _ (Development vc) = forked aoa Chainweb216Pact vc
--     go f (FastTimedCPM g) | g == petersonChainGraph = f 53
--     go f _ = f 16

-- -- | Pact changes for Chainweb 2.17
-- chainweb217Pact
--     :: Ordering
--     -> ChainwebVersion
--     -> BlockHeight
--     -> Bool
-- chainweb217Pact aoa v h = case aoa of
--     EQ -> go (==) v h
--     GT -> go (<) v h
--     LT -> go (>) v h
--   where
--     go f Mainnet01 = f 3_250_348 -- 2022-12-02 00:00:00+00:00
--     go f Testnet04 = f 2_777_367 -- 2022-12-01 12:00:00+00:00
--     go _ (Development vc) = forked aoa Chainweb217Pact vc
--     go f (FastTimedCPM g) | g == petersonChainGraph = f 55
--     go f _ = f 20

-- slowEpochGuard Mainnet01 h = h < 80000
-- slowEpochGuard _ _ = False

-- oldTargetGuard Mainnet01 h = h < 452820 -- ~ 2020-04-04T00:00:00Z
-- oldTargetGuard _ _ = False

-- skipFeatureFlagValidationGuard Mainnet01 h = h < 530500  -- ~ 2020-05-01T00:00:xxZ
-- skipFeatureFlagValidationGuard _ _ = False

-- oldDaGuard Mainnet01 h = h < 771_414 -- ~ 2020-07-23 16:00:00
-- oldDaGuard Testnet04 h = h < 318_204 -- ~ 2020-07-23 16:00:00
-- oldDaGuard Development{} h = h < 11 -- after DA at 10
-- oldDaGuard _ _ = False

-- makeLenses ''DevVersionConfig

-- coinV2Transactions :: ChainwebVersion -> ChainId -> IO [ChainwebTransaction]
-- coinV2Transactions Mainnet01 cid = case cidInt of
--   0 -> MN0.transactions
--   1 -> MN1.transactions
--   2 -> MN2.transactions
--   3 -> MN3.transactions
--   4 -> MN4.transactions
--   5 -> MN5.transactions
--   6 -> MN6.transactions
--   7 -> MN7.transactions
--   8 -> MN8.transactions
--   9 -> MN9.transactions
--   c | c >= 10, c <= 19 -> return []
--   c -> internalError $ "Invalid mainnet chain id: " <> sshow c
--   where cidInt :: Int
--         cidInt = chainIdInt cid
-- upgradeTransactions Development{} cid = case chainIdInt @Int cid of
--   c | c >= 0, c <= 9 -> Devnet.transactions
--   c | c >= 10, c <= 19 -> return []
--   c -> internalError $ "Invalid devnet chain id: "  <> sshow c
-- -- upgradeTransactions _ _ = Other.transactions

-- twentyChainTransactions :: ChainwebVersion dc -> ChainId -> IO [ChainwebTransaction]
-- twentyChainTransactions Mainnet01 cid = case chainIdInt @Int cid of
--   0 -> MNKAD.transactions
--   c | c >= 1, c <= 19 -> return []
--   c -> internalError $ "Invalid mainnet chain id: " <> sshow c
-- twentyChainUpgradeTransactions Development{} cid = case chainIdInt @Int cid of
--   0 -> MNKAD.transactions -- just remeds
--   c | c >= 1, c <= 19 -> return []
--   c -> internalError $ "Invalid devnet chain id: " <> sshow c
-- twentyChainUpgradeTransactions (FastTimedCPM _) cid = case chainIdInt @Int cid of
--   c | c == 0, c == 1, c == 2 -> return []
--   {-- NOTE: Remediations occur in Chain 3 instead of Chain 0 for this version.
--             This allows for testing that Rosetta correctly handles remediation
--             txs without breaking the SPV tests. --}
--   3 -> MNKAD.transactions -- just remeds
--   c | c <= 19 -> return []
--   c -> internalError $ "Invalid fasttimecpm chain id: " <> sshow c
-- twentyChainUpgradeTransactions _ _ = return []

-- otherTransactions :: IO [ChainwebTransaction]
-- otherTransactions =
--   let decodeTx t =
--         fromEitherM . (first (userError . show)) . codecDecode (chainwebPayloadCodec maxBound) =<< decodeB64UrlNoPaddingText t
--   in mapM decodeTx [
--     "eyJoYXNoIjoiMDVCdGo3ZUJaQlc3by1TYUxvVmhBaWNNVVBaVUJiRzZRVDhfTEFrQ3hIcyIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6bnVsbCxcImNvZGVcIjpcIihpbnRlcmZhY2UgZnVuZ2libGUtdjJcXG5cXG4gIFxcXCIgU3RhbmRhcmQgZm9yIGZ1bmdpYmxlIGNvaW5zIGFuZCB0b2tlbnMgYXMgc3BlY2lmaWVkIGluIEtJUC0wMDAyLiBcXFwiXFxuXFxuICAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICAgOyBTY2hlbWFcXG5cXG4gICAoZGVmc2NoZW1hIGFjY291bnQtZGV0YWlsc1xcbiAgICBAZG9jIFxcXCJTY2hlbWEgZm9yIHJlc3VsdHMgb2YgJ2FjY291bnQnIG9wZXJhdGlvbi5cXFwiXFxuICAgIEBtb2RlbCBbIChpbnZhcmlhbnQgKCE9IFxcXCJcXFwiIHNlbmRlcikpIF1cXG5cXG4gICAgYWNjb3VudDpzdHJpbmdcXG4gICAgYmFsYW5jZTpkZWNpbWFsXFxuICAgIGd1YXJkOmd1YXJkKVxcblxcblxcbiAgIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgIDsgQ2Fwc1xcblxcbiAgIChkZWZjYXAgVFJBTlNGRVI6Ym9vbFxcbiAgICAgKCBzZW5kZXI6c3RyaW5nXFxuICAgICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICAgKVxcbiAgICAgQGRvYyBcXFwiIE1hbmFnZWQgY2FwYWJpbGl0eSBzZWFsaW5nIEFNT1VOVCBmb3IgdHJhbnNmZXIgZnJvbSBTRU5ERVIgdG8gXFxcXFxcbiAgICAgICAgICBcXFxcIFJFQ0VJVkVSLiBQZXJtaXRzIGFueSBudW1iZXIgb2YgdHJhbnNmZXJzIHVwIHRvIEFNT1VOVC5cXFwiXFxuICAgICBAbWFuYWdlZCBhbW91bnQgVFJBTlNGRVItbWdyXFxuICAgICApXFxuXFxuICAgKGRlZnVuIFRSQU5TRkVSLW1ncjpkZWNpbWFsXFxuICAgICAoIG1hbmFnZWQ6ZGVjaW1hbFxcbiAgICAgICByZXF1ZXN0ZWQ6ZGVjaW1hbFxcbiAgICAgKVxcbiAgICAgQGRvYyBcXFwiIE1hbmFnZXMgVFJBTlNGRVIgQU1PVU5UIGxpbmVhcmx5LCBcXFxcXFxuICAgICAgICAgIFxcXFwgc3VjaCB0aGF0IGEgcmVxdWVzdCBmb3IgMS4wIGFtb3VudCBvbiBhIDMuMCBcXFxcXFxuICAgICAgICAgIFxcXFwgbWFuYWdlZCBxdWFudGl0eSBlbWl0cyB1cGRhdGVkIGFtb3VudCAyLjAuXFxcIlxcbiAgICAgKVxcblxcbiAgIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgIDsgRnVuY3Rpb25hbGl0eVxcblxcblxcbiAgKGRlZnVuIHRyYW5zZmVyOnN0cmluZ1xcbiAgICAoIHNlbmRlcjpzdHJpbmdcXG4gICAgICByZWNlaXZlcjpzdHJpbmdcXG4gICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICApXFxuICAgIEBkb2MgXFxcIiBUcmFuc2ZlciBBTU9VTlQgYmV0d2VlbiBhY2NvdW50cyBTRU5ERVIgYW5kIFJFQ0VJVkVSLiBcXFxcXFxuICAgICAgICAgXFxcXCBGYWlscyBpZiBlaXRoZXIgU0VOREVSIG9yIFJFQ0VJVkVSIGRvZXMgbm90IGV4aXN0LlxcXCJcXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHNlbmRlciBcXFwiXFxcIikpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoIT0gcmVjZWl2ZXIgXFxcIlxcXCIpKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHNlbmRlciByZWNlaXZlcikpXFxuICAgICAgICAgICBdXFxuICAgIClcXG5cXG4gICAoZGVmdW4gdHJhbnNmZXItY3JlYXRlOnN0cmluZ1xcbiAgICAgKCBzZW5kZXI6c3RyaW5nXFxuICAgICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICAgICByZWNlaXZlci1ndWFyZDpndWFyZFxcbiAgICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICAgKVxcbiAgICAgQGRvYyBcXFwiIFRyYW5zZmVyIEFNT1VOVCBiZXR3ZWVuIGFjY291bnRzIFNFTkRFUiBhbmQgUkVDRUlWRVIuIFxcXFxcXG4gICAgICAgICAgXFxcXCBGYWlscyBpZiBTRU5ERVIgZG9lcyBub3QgZXhpc3QuIElmIFJFQ0VJVkVSIGV4aXN0cywgZ3VhcmQgXFxcXFxcbiAgICAgICAgICBcXFxcIG11c3QgbWF0Y2ggZXhpc3RpbmcgdmFsdWUuIElmIFJFQ0VJVkVSIGRvZXMgbm90IGV4aXN0LCBcXFxcXFxuICAgICAgICAgIFxcXFwgUkVDRUlWRVIgYWNjb3VudCBpcyBjcmVhdGVkIHVzaW5nIFJFQ0VJVkVSLUdVQVJELiBcXFxcXFxuICAgICAgICAgIFxcXFwgU3ViamVjdCB0byBtYW5hZ2VtZW50IGJ5IFRSQU5TRkVSIGNhcGFiaWxpdHkuXFxcIlxcbiAgICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAgKHByb3BlcnR5ICghPSBzZW5kZXIgXFxcIlxcXCIpKVxcbiAgICAgICAgICAgICAgKHByb3BlcnR5ICghPSByZWNlaXZlciBcXFwiXFxcIikpXFxuICAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHNlbmRlciByZWNlaXZlcikpXFxuICAgICAgICAgICAgXVxcbiAgICAgKVxcblxcbiAgIChkZWZwYWN0IHRyYW5zZmVyLWNyb3NzY2hhaW46c3RyaW5nXFxuICAgICAoIHNlbmRlcjpzdHJpbmdcXG4gICAgICAgcmVjZWl2ZXI6c3RyaW5nXFxuICAgICAgIHJlY2VpdmVyLWd1YXJkOmd1YXJkXFxuICAgICAgIHRhcmdldC1jaGFpbjpzdHJpbmdcXG4gICAgICAgYW1vdW50OmRlY2ltYWxcXG4gICAgIClcXG4gICAgIEBkb2MgXFxcIiAyLXN0ZXAgcGFjdCB0byB0cmFuc2ZlciBBTU9VTlQgZnJvbSBTRU5ERVIgb24gY3VycmVudCBjaGFpbiBcXFxcXFxuICAgICAgICAgIFxcXFwgdG8gUkVDRUlWRVIgb24gVEFSR0VULUNIQUlOIHZpYSBTUFYgcHJvb2YuIFxcXFxcXG4gICAgICAgICAgXFxcXCBUQVJHRVQtQ0hBSU4gbXVzdCBiZSBkaWZmZXJlbnQgdGhhbiBjdXJyZW50IGNoYWluIGlkLiBcXFxcXFxuICAgICAgICAgIFxcXFwgRmlyc3Qgc3RlcCBkZWJpdHMgQU1PVU5UIGNvaW5zIGluIFNFTkRFUiBhY2NvdW50IGFuZCB5aWVsZHMgXFxcXFxcbiAgICAgICAgICBcXFxcIFJFQ0VJVkVSLCBSRUNFSVZFUl9HVUFSRCBhbmQgQU1PVU5UIHRvIFRBUkdFVC1DSEFJTi4gXFxcXFxcbiAgICAgICAgICBcXFxcIFNlY29uZCBzdGVwIGNvbnRpbnVhdGlvbiBpcyBzZW50IGludG8gVEFSR0VULUNIQUlOIHdpdGggcHJvb2YgXFxcXFxcbiAgICAgICAgICBcXFxcIG9idGFpbmVkIGZyb20gdGhlIHNwdiAnb3V0cHV0JyBlbmRwb2ludCBvZiBDaGFpbndlYi4gXFxcXFxcbiAgICAgICAgICBcXFxcIFByb29mIGlzIHZhbGlkYXRlZCBhbmQgUkVDRUlWRVIgaXMgY3JlZGl0ZWQgd2l0aCBBTU9VTlQgXFxcXFxcbiAgICAgICAgICBcXFxcIGNyZWF0aW5nIGFjY291bnQgd2l0aCBSRUNFSVZFUl9HVUFSRCBhcyBuZWNlc3NhcnkuXFxcIlxcbiAgICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAgKHByb3BlcnR5ICghPSBzZW5kZXIgXFxcIlxcXCIpKVxcbiAgICAgICAgICAgICAgKHByb3BlcnR5ICghPSByZWNlaXZlciBcXFwiXFxcIikpXFxuICAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHNlbmRlciByZWNlaXZlcikpXFxuICAgICAgICAgICAgICAocHJvcGVydHkgKCE9IHRhcmdldC1jaGFpbiBcXFwiXFxcIikpXFxuICAgICAgICAgICAgXVxcbiAgICAgKVxcblxcbiAgIChkZWZ1biBnZXQtYmFsYW5jZTpkZWNpbWFsXFxuICAgICAoIGFjY291bnQ6c3RyaW5nIClcXG4gICAgIFxcXCIgR2V0IGJhbGFuY2UgZm9yIEFDQ09VTlQuIEZhaWxzIGlmIGFjY291bnQgZG9lcyBub3QgZXhpc3QuXFxcIlxcbiAgICAgKVxcblxcbiAgIChkZWZ1biBkZXRhaWxzOm9iamVjdHthY2NvdW50LWRldGFpbHN9XFxuICAgICAoIGFjY291bnQ6IHN0cmluZyApXFxuICAgICBcXFwiIEdldCBhbiBvYmplY3Qgd2l0aCBkZXRhaWxzIG9mIEFDQ09VTlQuIFxcXFxcXG4gICAgIFxcXFwgRmFpbHMgaWYgYWNjb3VudCBkb2VzIG5vdCBleGlzdC5cXFwiXFxuICAgICApXFxuXFxuICAgKGRlZnVuIHByZWNpc2lvbjppbnRlZ2VyXFxuICAgICAoKVxcbiAgICAgXFxcIlJldHVybiB0aGUgbWF4aW11bSBhbGxvd2VkIGRlY2ltYWwgcHJlY2lzaW9uLlxcXCJcXG4gICAgIClcXG5cXG4gICAoZGVmdW4gZW5mb3JjZS11bml0OmJvb2xcXG4gICAgICggYW1vdW50OmRlY2ltYWwgKVxcbiAgICAgXFxcIiBFbmZvcmNlIG1pbmltdW0gcHJlY2lzaW9uIGFsbG93ZWQgZm9yIHRyYW5zYWN0aW9ucy5cXFwiXFxuICAgICApXFxuXFxuICAgKGRlZnVuIGNyZWF0ZS1hY2NvdW50OnN0cmluZ1xcbiAgICAgKCBhY2NvdW50OnN0cmluZ1xcbiAgICAgICBndWFyZDpndWFyZFxcbiAgICAgKVxcbiAgICAgXFxcIiBDcmVhdGUgQUNDT1VOVCB3aXRoIDAuMCBiYWxhbmNlLCB3aXRoIEdVQVJEIGNvbnRyb2xsaW5nIGFjY2Vzcy5cXFwiXFxuICAgICApXFxuXFxuICAgKGRlZnVuIHJvdGF0ZTpzdHJpbmdcXG4gICAgICggYWNjb3VudDpzdHJpbmdcXG4gICAgICAgbmV3LWd1YXJkOmd1YXJkXFxuICAgICApXFxuICAgICBcXFwiIFJvdGF0ZSBndWFyZCBmb3IgQUNDT1VOVC4gVHJhbnNhY3Rpb24gaXMgdmFsaWRhdGVkIGFnYWluc3QgXFxcXFxcbiAgICAgXFxcXCBleGlzdGluZyBndWFyZCBiZWZvcmUgaW5zdGFsbGluZyBuZXcgZ3VhcmQuIFxcXCJcXG4gICAgIClcXG5cXG4pXFxuXCJ9fSxcInNpZ25lcnNcIjpbXSxcIm1ldGFcIjp7XCJjcmVhdGlvblRpbWVcIjowLFwidHRsXCI6MTcyODAwLFwiZ2FzTGltaXRcIjowLFwiY2hhaW5JZFwiOlwiXCIsXCJnYXNQcmljZVwiOjAsXCJzZW5kZXJcIjpcIlwifSxcIm5vbmNlXCI6XCJmdW5naWJsZS1hc3NldC12MlwifSJ9"
--     ,
--     "eyJoYXNoIjoibVZzMjNxNnJyUjZrWDFGX0ItamNCX05hLXdZdmR3dnRwa1cwQVNaZExjRSIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6bnVsbCxcImNvZGVcIjpcIihtb2R1bGUgY29pbiBHT1ZFUk5BTkNFXFxuXFxuICBAZG9jIFxcXCInY29pbicgcmVwcmVzZW50cyB0aGUgS2FkZW5hIENvaW4gQ29udHJhY3QuIFRoaXMgY29udHJhY3QgcHJvdmlkZXMgYm90aCB0aGUgXFxcXFxcbiAgXFxcXGJ1eS9yZWRlZW0gZ2FzIHN1cHBvcnQgaW4gdGhlIGZvcm0gb2YgJ2Z1bmQtdHgnLCBhcyB3ZWxsIGFzIHRyYW5zZmVyLCAgICAgICBcXFxcXFxuICBcXFxcY3JlZGl0LCBkZWJpdCwgY29pbmJhc2UsIGFjY291bnQgY3JlYXRpb24gYW5kIHF1ZXJ5LCBhcyB3ZWxsIGFzIFNQViBidXJuICAgIFxcXFxcXG4gIFxcXFxjcmVhdGUuIFRvIGFjY2VzcyB0aGUgY29pbiBjb250cmFjdCwgeW91IG1heSB1c2UgaXRzIGZ1bGx5LXF1YWxpZmllZCBuYW1lLCAgXFxcXFxcbiAgXFxcXG9yIGlzc3VlIHRoZSAnKHVzZSBjb2luKScgY29tbWFuZCBpbiB0aGUgYm9keSBvZiBhIG1vZHVsZSBkZWNsYXJhdGlvbi5cXFwiXFxuXFxuICBAbW9kZWxcXG4gICAgWyAoZGVmcHJvcGVydHkgY29uc2VydmVzLW1hc3NcXG4gICAgICAgICg9IChjb2x1bW4tZGVsdGEgY29pbi10YWJsZSAnYmFsYW5jZSkgMC4wKSlcXG5cXG4gICAgICAoZGVmcHJvcGVydHkgdmFsaWQtYWNjb3VudCAoYWNjb3VudDpzdHJpbmcpXFxuICAgICAgICAoYW5kXFxuICAgICAgICAgICg-PSAobGVuZ3RoIGFjY291bnQpIDMpXFxuICAgICAgICAgICg8PSAobGVuZ3RoIGFjY291bnQpIDI1NikpKVxcbiAgICBdXFxuXFxuICAoaW1wbGVtZW50cyBmdW5naWJsZS12MilcXG5cXG4gIDsgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cXG4gIDsgU2NoZW1hcyBhbmQgVGFibGVzXFxuXFxuICAoZGVmc2NoZW1hIGNvaW4tc2NoZW1hXFxuICAgIEBkb2MgXFxcIlRoZSBjb2luIGNvbnRyYWN0IHRva2VuIHNjaGVtYVxcXCJcXG4gICAgQG1vZGVsIFsgKGludmFyaWFudCAoPj0gYmFsYW5jZSAwLjApKSBdXFxuXFxuICAgIGJhbGFuY2U6ZGVjaW1hbFxcbiAgICBndWFyZDpndWFyZClcXG5cXG4gIChkZWZ0YWJsZSBjb2luLXRhYmxlOntjb2luLXNjaGVtYX0pXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENhcGFiaWxpdGllc1xcblxcbiAgKGRlZmNhcCBHT1ZFUk5BTkNFICgpXFxuICAgIChlbmZvcmNlIGZhbHNlIFxcXCJFbmZvcmNlIG5vbi11cGdyYWRlYWJpbGl0eVxcXCIpKVxcblxcbiAgKGRlZmNhcCBHQVMgKClcXG4gICAgXFxcIk1hZ2ljIGNhcGFiaWxpdHkgdG8gcHJvdGVjdCBnYXMgYnV5IGFuZCByZWRlZW1cXFwiXFxuICAgIHRydWUpXFxuXFxuICAoZGVmY2FwIENPSU5CQVNFICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IHRvIHByb3RlY3QgbWluZXIgcmV3YXJkXFxcIlxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBHRU5FU0lTICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IGNvbnN0cmFpbmluZyBnZW5lc2lzIHRyYW5zYWN0aW9uc1xcXCJcXG4gICAgdHJ1ZSlcXG5cXG4gIChkZWZjYXAgUkVNRURJQVRFICgpXFxuICAgIFxcXCJNYWdpYyBjYXBhYmlsaXR5IGZvciByZW1lZGlhdGlvbiB0cmFuc2FjdGlvbnNcXFwiXFxuICAgIHRydWUpXFxuXFxuICAoZGVmY2FwIERFQklUIChzZW5kZXI6c3RyaW5nKVxcbiAgICBcXFwiQ2FwYWJpbGl0eSBmb3IgbWFuYWdpbmcgZGViaXRpbmcgb3BlcmF0aW9uc1xcXCJcXG4gICAgKGVuZm9yY2UtZ3VhcmQgKGF0ICdndWFyZCAocmVhZCBjb2luLXRhYmxlIHNlbmRlcikpKVxcbiAgICAoZW5mb3JjZSAoIT0gc2VuZGVyIFxcXCJcXFwiKSBcXFwidmFsaWQgc2VuZGVyXFxcIikpXFxuXFxuICAoZGVmY2FwIENSRURJVCAocmVjZWl2ZXI6c3RyaW5nKVxcbiAgICBcXFwiQ2FwYWJpbGl0eSBmb3IgbWFuYWdpbmcgY3JlZGl0aW5nIG9wZXJhdGlvbnNcXFwiXFxuICAgIChlbmZvcmNlICghPSByZWNlaXZlciBcXFwiXFxcIikgXFxcInZhbGlkIHJlY2VpdmVyXFxcIikpXFxuXFxuICAoZGVmY2FwIFJPVEFURSAoYWNjb3VudDpzdHJpbmcpXFxuICAgIEBkb2MgXFxcIkF1dG9ub21vdXNseSBtYW5hZ2VkIGNhcGFiaWxpdHkgZm9yIGd1YXJkIHJvdGF0aW9uXFxcIlxcbiAgICBAbWFuYWdlZFxcbiAgICB0cnVlKVxcblxcbiAgKGRlZmNhcCBUUkFOU0ZFUjpib29sXFxuICAgICggc2VuZGVyOnN0cmluZ1xcbiAgICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICAgIGFtb3VudDpkZWNpbWFsXFxuICAgIClcXG4gICAgQG1hbmFnZWQgYW1vdW50IFRSQU5TRkVSLW1nclxcbiAgICAoZW5mb3JjZSAoIT0gc2VuZGVyIHJlY2VpdmVyKSBcXFwic2FtZSBzZW5kZXIgYW5kIHJlY2VpdmVyXFxcIilcXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApIFxcXCJQb3NpdGl2ZSBhbW91bnRcXFwiKVxcbiAgICAoY29tcG9zZS1jYXBhYmlsaXR5IChERUJJVCBzZW5kZXIpKVxcbiAgICAoY29tcG9zZS1jYXBhYmlsaXR5IChDUkVESVQgcmVjZWl2ZXIpKVxcbiAgKVxcblxcbiAgKGRlZnVuIFRSQU5TRkVSLW1ncjpkZWNpbWFsXFxuICAgICggbWFuYWdlZDpkZWNpbWFsXFxuICAgICAgcmVxdWVzdGVkOmRlY2ltYWxcXG4gICAgKVxcblxcbiAgICAobGV0ICgobmV3YmFsICgtIG1hbmFnZWQgcmVxdWVzdGVkKSkpXFxuICAgICAgKGVuZm9yY2UgKD49IG5ld2JhbCAwLjApXFxuICAgICAgICAoZm9ybWF0IFxcXCJUUkFOU0ZFUiBleGNlZWRlZCBmb3IgYmFsYW5jZSB7fVxcXCIgW21hbmFnZWRdKSlcXG4gICAgICBuZXdiYWwpXFxuICApXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENvbnN0YW50c1xcblxcbiAgKGRlZmNvbnN0IENPSU5fQ0hBUlNFVCBDSEFSU0VUX0xBVElOMVxcbiAgICBcXFwiVGhlIGRlZmF1bHQgY29pbiBjb250cmFjdCBjaGFyYWN0ZXIgc2V0XFxcIilcXG5cXG4gIChkZWZjb25zdCBNSU5JTVVNX1BSRUNJU0lPTiAxMlxcbiAgICBcXFwiTWluaW11bSBhbGxvd2VkIHByZWNpc2lvbiBmb3IgY29pbiB0cmFuc2FjdGlvbnNcXFwiKVxcblxcbiAgKGRlZmNvbnN0IE1JTklNVU1fQUNDT1VOVF9MRU5HVEggM1xcbiAgICBcXFwiTWluaW11bSBhY2NvdW50IGxlbmd0aCBhZG1pc3NpYmxlIGZvciBjb2luIGFjY291bnRzXFxcIilcXG5cXG4gIChkZWZjb25zdCBNQVhJTVVNX0FDQ09VTlRfTEVOR1RIIDI1NlxcbiAgICBcXFwiTWF4aW11bSBhY2NvdW50IG5hbWUgbGVuZ3RoIGFkbWlzc2libGUgZm9yIGNvaW4gYWNjb3VudHNcXFwiKVxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBVdGlsaXRpZXNcXG5cXG4gIChkZWZ1biBlbmZvcmNlLXVuaXQ6Ym9vbCAoYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIkVuZm9yY2UgbWluaW11bSBwcmVjaXNpb24gYWxsb3dlZCBmb3IgY29pbiB0cmFuc2FjdGlvbnNcXFwiXFxuXFxuICAgIChlbmZvcmNlXFxuICAgICAgKD0gKGZsb29yIGFtb3VudCBNSU5JTVVNX1BSRUNJU0lPTilcXG4gICAgICAgICBhbW91bnQpXFxuICAgICAgKGZvcm1hdCBcXFwiQW1vdW50IHZpb2xhdGVzIG1pbmltdW0gcHJlY2lzaW9uOiB7fVxcXCIgW2Ftb3VudF0pKVxcbiAgICApXFxuXFxuICAoZGVmdW4gdmFsaWRhdGUtYWNjb3VudCAoYWNjb3VudDpzdHJpbmcpXFxuICAgIEBkb2MgXFxcIkVuZm9yY2UgdGhhdCBhbiBhY2NvdW50IG5hbWUgY29uZm9ybXMgdG8gdGhlIGNvaW4gY29udHJhY3QgXFxcXFxcbiAgICAgICAgIFxcXFxtaW5pbXVtIGFuZCBtYXhpbXVtIGxlbmd0aCByZXF1aXJlbWVudHMsIGFzIHdlbGwgYXMgdGhlICAgIFxcXFxcXG4gICAgICAgICBcXFxcbGF0aW4tMSBjaGFyYWN0ZXIgc2V0LlxcXCJcXG5cXG4gICAgKGVuZm9yY2VcXG4gICAgICAoaXMtY2hhcnNldCBDT0lOX0NIQVJTRVQgYWNjb3VudClcXG4gICAgICAoZm9ybWF0XFxuICAgICAgICBcXFwiQWNjb3VudCBkb2VzIG5vdCBjb25mb3JtIHRvIHRoZSBjb2luIGNvbnRyYWN0IGNoYXJzZXQ6IHt9XFxcIlxcbiAgICAgICAgW2FjY291bnRdKSlcXG5cXG4gICAgKGxldCAoKGFjY291bnQtbGVuZ3RoIChsZW5ndGggYWNjb3VudCkpKVxcblxcbiAgICAgIChlbmZvcmNlXFxuICAgICAgICAoPj0gYWNjb3VudC1sZW5ndGggTUlOSU1VTV9BQ0NPVU5UX0xFTkdUSClcXG4gICAgICAgIChmb3JtYXRcXG4gICAgICAgICAgXFxcIkFjY291bnQgbmFtZSBkb2VzIG5vdCBjb25mb3JtIHRvIHRoZSBtaW4gbGVuZ3RoIHJlcXVpcmVtZW50OiB7fVxcXCJcXG4gICAgICAgICAgW2FjY291bnRdKSlcXG5cXG4gICAgICAoZW5mb3JjZVxcbiAgICAgICAgKDw9IGFjY291bnQtbGVuZ3RoIE1BWElNVU1fQUNDT1VOVF9MRU5HVEgpXFxuICAgICAgICAoZm9ybWF0XFxuICAgICAgICAgIFxcXCJBY2NvdW50IG5hbWUgZG9lcyBub3QgY29uZm9ybSB0byB0aGUgbWF4IGxlbmd0aCByZXF1aXJlbWVudDoge31cXFwiXFxuICAgICAgICAgIFthY2NvdW50XSkpXFxuICAgICAgKVxcbiAgKVxcblxcbiAgOyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxcbiAgOyBDb2luIENvbnRyYWN0XFxuXFxuICAoZGVmdW4gZ2FzLW9ubHkgKClcXG4gICAgXFxcIlByZWRpY2F0ZSBmb3IgZ2FzLW9ubHkgdXNlciBndWFyZHMuXFxcIlxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChHQVMpKSlcXG5cXG4gIChkZWZ1biBnYXMtZ3VhcmQgKGd1YXJkOmd1YXJkKVxcbiAgICBcXFwiUHJlZGljYXRlIGZvciBnYXMgKyBzaW5nbGUga2V5IHVzZXIgZ3VhcmRzXFxcIlxcbiAgICAoZW5mb3JjZS1vbmVcXG4gICAgICBcXFwiRW5mb3JjZSBlaXRoZXIgdGhlIHByZXNlbmNlIG9mIGEgR0FTIGNhcCBvciBrZXlzZXRcXFwiXFxuICAgICAgWyAoZ2FzLW9ubHkpXFxuICAgICAgICAoZW5mb3JjZS1ndWFyZCBndWFyZClcXG4gICAgICBdKSlcXG5cXG4gIChkZWZ1biBidXktZ2FzOnN0cmluZyAoc2VuZGVyOnN0cmluZyB0b3RhbDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCJUaGlzIGZ1bmN0aW9uIGRlc2NyaWJlcyB0aGUgbWFpbiAnZ2FzIGJ1eScgb3BlcmF0aW9uLiBBdCB0aGlzIHBvaW50IFxcXFxcXG4gICAgXFxcXE1JTkVSIGhhcyBiZWVuIGNob3NlbiBmcm9tIHRoZSBwb29sLCBhbmQgd2lsbCBiZSB2YWxpZGF0ZWQuIFRoZSBTRU5ERVIgICBcXFxcXFxuICAgIFxcXFxvZiB0aGlzIHRyYW5zYWN0aW9uIGhhcyBzcGVjaWZpZWQgYSBnYXMgbGltaXQgTElNSVQgKG1heGltdW0gZ2FzKSBmb3IgICAgXFxcXFxcbiAgICBcXFxcdGhlIHRyYW5zYWN0aW9uLCBhbmQgdGhlIHByaWNlIGlzIHRoZSBzcG90IHByaWNlIG9mIGdhcyBhdCB0aGF0IHRpbWUuICAgIFxcXFxcXG4gICAgXFxcXFRoZSBnYXMgYnV5IHdpbGwgYmUgZXhlY3V0ZWQgcHJpb3IgdG8gZXhlY3V0aW5nIFNFTkRFUidzIGNvZGUuXFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gdG90YWwgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHNlbmRlcikpXFxuICAgICAgICAgICBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IHNlbmRlcilcXG5cXG4gICAgKGVuZm9yY2UtdW5pdCB0b3RhbClcXG4gICAgKGVuZm9yY2UgKD4gdG90YWwgMC4wKSBcXFwiZ2FzIHN1cHBseSBtdXN0IGJlIGEgcG9zaXRpdmUgcXVhbnRpdHlcXFwiKVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChHQVMpKVxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChERUJJVCBzZW5kZXIpXFxuICAgICAgKGRlYml0IHNlbmRlciB0b3RhbCkpXFxuICAgIClcXG5cXG4gIChkZWZ1biByZWRlZW0tZ2FzOnN0cmluZyAobWluZXI6c3RyaW5nIG1pbmVyLWd1YXJkOmd1YXJkIHNlbmRlcjpzdHJpbmcgdG90YWw6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiVGhpcyBmdW5jdGlvbiBkZXNjcmliZXMgdGhlIG1haW4gJ3JlZGVlbSBnYXMnIG9wZXJhdGlvbi4gQXQgdGhpcyAgICBcXFxcXFxuICAgIFxcXFxwb2ludCwgdGhlIFNFTkRFUidzIHRyYW5zYWN0aW9uIGhhcyBiZWVuIGV4ZWN1dGVkLCBhbmQgdGhlIGdhcyB0aGF0ICAgICAgXFxcXFxcbiAgICBcXFxcd2FzIGNoYXJnZWQgaGFzIGJlZW4gY2FsY3VsYXRlZC4gTUlORVIgd2lsbCBiZSBjcmVkaXRlZCB0aGUgZ2FzIGNvc3QsICAgIFxcXFxcXG4gICAgXFxcXGFuZCBTRU5ERVIgd2lsbCByZWNlaXZlIHRoZSByZW1haW5kZXIgdXAgdG8gdGhlIGxpbWl0XFxcIlxcblxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKD4gdG90YWwgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHNlbmRlcikpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBtaW5lcikpXFxuICAgICAgICAgICBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IHNlbmRlcilcXG4gICAgKHZhbGlkYXRlLWFjY291bnQgbWluZXIpXFxuICAgIChlbmZvcmNlLXVuaXQgdG90YWwpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKEdBUykpXFxuICAgIChsZXQqXFxuICAgICAgKChmZWUgKHJlYWQtZGVjaW1hbCBcXFwiZmVlXFxcIikpXFxuICAgICAgIChyZWZ1bmQgKC0gdG90YWwgZmVlKSkpXFxuXFxuICAgICAgKGVuZm9yY2UtdW5pdCBmZWUpXFxuICAgICAgKGVuZm9yY2UgKD49IGZlZSAwLjApXFxuICAgICAgICBcXFwiZmVlIG11c3QgYmUgYSBub24tbmVnYXRpdmUgcXVhbnRpdHlcXFwiKVxcblxcbiAgICAgIChlbmZvcmNlICg-PSByZWZ1bmQgMC4wKVxcbiAgICAgICAgXFxcInJlZnVuZCBtdXN0IGJlIGEgbm9uLW5lZ2F0aXZlIHF1YW50aXR5XFxcIilcXG5cXG4gICAgICAgIDsgZGlyZWN0bHkgdXBkYXRlIGluc3RlYWQgb2YgY3JlZGl0XFxuICAgICAgKHdpdGgtY2FwYWJpbGl0eSAoQ1JFRElUIHNlbmRlcilcXG4gICAgICAgIChpZiAoPiByZWZ1bmQgMC4wKVxcbiAgICAgICAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgc2VuZGVyXFxuICAgICAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOj0gYmFsYW5jZSB9XFxuICAgICAgICAgICAgKHVwZGF0ZSBjb2luLXRhYmxlIHNlbmRlclxcbiAgICAgICAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCI6ICgrIGJhbGFuY2UgcmVmdW5kKSB9KSlcXG5cXG4gICAgICAgICAgXFxcIm5vb3BcXFwiKSlcXG5cXG4gICAgICAod2l0aC1jYXBhYmlsaXR5IChDUkVESVQgbWluZXIpXFxuICAgICAgICAoaWYgKD4gZmVlIDAuMClcXG4gICAgICAgICAgKGNyZWRpdCBtaW5lciBtaW5lci1ndWFyZCBmZWUpXFxuICAgICAgICAgIFxcXCJub29wXFxcIikpXFxuICAgICAgKVxcblxcbiAgICApXFxuXFxuICAoZGVmdW4gY3JlYXRlLWFjY291bnQ6c3RyaW5nIChhY2NvdW50OnN0cmluZyBndWFyZDpndWFyZClcXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKSBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgIChpbnNlcnQgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiAwLjBcXG4gICAgICAsIFxcXCJndWFyZFxcXCIgICA6IGd1YXJkXFxuICAgICAgfSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIGdldC1iYWxhbmNlOmRlY2ltYWwgKGFjY291bnQ6c3RyaW5nKVxcbiAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbGFuY2UgfVxcbiAgICAgIGJhbGFuY2VcXG4gICAgICApXFxuICAgIClcXG5cXG4gIChkZWZ1biBkZXRhaWxzOm9iamVjdHtmdW5naWJsZS12Mi5hY2NvdW50LWRldGFpbHN9XFxuICAgICggYWNjb3VudDpzdHJpbmcgKVxcbiAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbFxcbiAgICAgICwgXFxcImd1YXJkXFxcIiA6PSBnIH1cXG4gICAgICB7IFxcXCJhY2NvdW50XFxcIiA6IGFjY291bnRcXG4gICAgICAsIFxcXCJiYWxhbmNlXFxcIiA6IGJhbFxcbiAgICAgICwgXFxcImd1YXJkXFxcIjogZyB9KVxcbiAgICApXFxuXFxuICAoZGVmdW4gcm90YXRlOnN0cmluZyAoYWNjb3VudDpzdHJpbmcgbmV3LWd1YXJkOmd1YXJkKVxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChST1RBVEUgYWNjb3VudClcXG4gICAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiZ3VhcmRcXFwiIDo9IG9sZC1ndWFyZCB9XFxuXFxuICAgICAgICAoZW5mb3JjZS1ndWFyZCBvbGQtZ3VhcmQpXFxuXFxuICAgICAgICAodXBkYXRlIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgICAgICB7IFxcXCJndWFyZFxcXCIgOiBuZXctZ3VhcmQgfVxcbiAgICAgICAgICApKSlcXG4gICAgKVxcblxcblxcbiAgKGRlZnVuIHByZWNpc2lvbjppbnRlZ2VyXFxuICAgICgpXFxuICAgIE1JTklNVU1fUFJFQ0lTSU9OKVxcblxcbiAgKGRlZnVuIHRyYW5zZmVyOnN0cmluZyAoc2VuZGVyOnN0cmluZyByZWNlaXZlcjpzdHJpbmcgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSBjb25zZXJ2ZXMtbWFzcylcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHJlY2VpdmVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICghPSBzZW5kZXIgcmVjZWl2ZXIpKSBdXFxuXFxuICAgIChlbmZvcmNlICghPSBzZW5kZXIgcmVjZWl2ZXIpXFxuICAgICAgXFxcInNlbmRlciBjYW5ub3QgYmUgdGhlIHJlY2VpdmVyIG9mIGEgdHJhbnNmZXJcXFwiKVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBzZW5kZXIpXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IHJlY2VpdmVyKVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJ0cmFuc2ZlciBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUiBzZW5kZXIgcmVjZWl2ZXIgYW1vdW50KVxcbiAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcbiAgICAgICh3aXRoLXJlYWQgY29pbi10YWJsZSByZWNlaXZlclxcbiAgICAgICAgeyBcXFwiZ3VhcmRcXFwiIDo9IGcgfVxcblxcbiAgICAgICAgKGNyZWRpdCByZWNlaXZlciBnIGFtb3VudCkpXFxuICAgICAgKVxcbiAgICApXFxuXFxuICAoZGVmdW4gdHJhbnNmZXItY3JlYXRlOnN0cmluZ1xcbiAgICAoIHNlbmRlcjpzdHJpbmdcXG4gICAgICByZWNlaXZlcjpzdHJpbmdcXG4gICAgICByZWNlaXZlci1ndWFyZDpndWFyZFxcbiAgICAgIGFtb3VudDpkZWNpbWFsIClcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5IGNvbnNlcnZlcy1tYXNzKSBdXFxuXFxuICAgIChlbmZvcmNlICghPSBzZW5kZXIgcmVjZWl2ZXIpXFxuICAgICAgXFxcInNlbmRlciBjYW5ub3QgYmUgdGhlIHJlY2VpdmVyIG9mIGEgdHJhbnNmZXJcXFwiKVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBzZW5kZXIpXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IHJlY2VpdmVyKVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgIFxcXCJ0cmFuc2ZlciBhbW91bnQgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAod2l0aC1jYXBhYmlsaXR5IChUUkFOU0ZFUiBzZW5kZXIgcmVjZWl2ZXIgYW1vdW50KVxcbiAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcbiAgICAgIChjcmVkaXQgcmVjZWl2ZXIgcmVjZWl2ZXItZ3VhcmQgYW1vdW50KSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIGNvaW5iYXNlOnN0cmluZyAoYWNjb3VudDpzdHJpbmcgYWNjb3VudC1ndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiSW50ZXJuYWwgZnVuY3Rpb24gZm9yIHRoZSBpbml0aWFsIGNyZWF0aW9uIG9mIGNvaW5zLiAgVGhpcyBmdW5jdGlvbiBcXFxcXFxuICAgIFxcXFxjYW5ub3QgYmUgdXNlZCBvdXRzaWRlIG9mIHRoZSBjb2luIGNvbnRyYWN0LlxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKD4gYW1vdW50IDAuMCkpXFxuICAgICAgICAgICBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuICAgIChlbmZvcmNlLXVuaXQgYW1vdW50KVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChDT0lOQkFTRSkpXFxuICAgICh3aXRoLWNhcGFiaWxpdHkgKENSRURJVCBhY2NvdW50KVxcbiAgICAgIChjcmVkaXQgYWNjb3VudCBhY2NvdW50LWd1YXJkIGFtb3VudCkpXFxuICAgIClcXG5cXG4gIChkZWZ1biByZW1lZGlhdGU6c3RyaW5nIChhY2NvdW50OnN0cmluZyBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiQWxsb3dzIGZvciByZW1lZGlhdGlvbiB0cmFuc2FjdGlvbnMuIFRoaXMgZnVuY3Rpb24gXFxcXFxcbiAgICAgICAgIFxcXFxpcyBwcm90ZWN0ZWQgYnkgdGhlIFJFTUVESUFURSBjYXBhYmlsaXR5XFxcIlxcbiAgICBAbW9kZWwgWyAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgYWNjb3VudCkpXFxuICAgICAgICAgICAgIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgYWNjb3VudClcXG5cXG4gICAgKGVuZm9yY2UgKD4gYW1vdW50IDAuMClcXG4gICAgICBcXFwiUmVtZWRpYXRpb24gYW1vdW50IG11c3QgYmUgcG9zaXRpdmVcXFwiKVxcblxcbiAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgKHJlcXVpcmUtY2FwYWJpbGl0eSAoUkVNRURJQVRFKSlcXG4gICAgKHdpdGgtcmVhZCBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6PSBiYWxhbmNlIH1cXG5cXG4gICAgICAoZW5mb3JjZSAoPD0gYW1vdW50IGJhbGFuY2UpIFxcXCJJbnN1ZmZpY2llbnQgZnVuZHNcXFwiKVxcblxcbiAgICAgICh1cGRhdGUgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6ICgtIGJhbGFuY2UgYW1vdW50KSB9XFxuICAgICAgICApKVxcbiAgICApXFxuXFxuICAoZGVmcGFjdCBmdW5kLXR4IChzZW5kZXI6c3RyaW5nIG1pbmVyOnN0cmluZyBtaW5lci1ndWFyZDpndWFyZCB0b3RhbDpkZWNpbWFsKVxcbiAgICBAZG9jIFxcXCInZnVuZC10eCcgaXMgYSBzcGVjaWFsIHBhY3QgdG8gZnVuZCBhIHRyYW5zYWN0aW9uIGluIHR3byBzdGVwcywgICAgIFxcXFxcXG4gICAgXFxcXHdpdGggdGhlIGFjdHVhbCB0cmFuc2FjdGlvbiB0cmFuc3BpcmluZyBpbiB0aGUgbWlkZGxlOiAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgIFxcXFwgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXFxcXFxcbiAgICBcXFxcICAxKSBBIGJ1eWluZyBwaGFzZSwgZGViaXRpbmcgdGhlIHNlbmRlciBmb3IgdG90YWwgZ2FzIGFuZCBmZWUsIHlpZWxkaW5nIFxcXFxcXG4gICAgXFxcXCAgICAgVFhfTUFYX0NIQVJHRS4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcXFxcXFxuICAgIFxcXFwgIDIpIEEgc2V0dGxlbWVudCBwaGFzZSwgcmVzdW1pbmcgVFhfTUFYX0NIQVJHRSwgYW5kIGFsbG9jYXRpbmcgdG8gdGhlICAgXFxcXFxcbiAgICBcXFxcICAgICBjb2luYmFzZSBhY2NvdW50IGZvciB1c2VkIGdhcyBhbmQgZmVlLCBhbmQgc2VuZGVyIGFjY291bnQgZm9yIGJhbC0gIFxcXFxcXG4gICAgXFxcXCAgICAgYW5jZSAodW51c2VkIGdhcywgaWYgYW55KS5cXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiB0b3RhbCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IG1pbmVyKSlcXG4gICAgICAgICAgICAgOyhwcm9wZXJ0eSBjb25zZXJ2ZXMtbWFzcykgbm90IHN1cHBvcnRlZCB5ZXRcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHN0ZXAgKGJ1eS1nYXMgc2VuZGVyIHRvdGFsKSlcXG4gICAgKHN0ZXAgKHJlZGVlbS1nYXMgbWluZXIgbWluZXItZ3VhcmQgc2VuZGVyIHRvdGFsKSlcXG4gICAgKVxcblxcbiAgKGRlZnVuIGRlYml0OnN0cmluZyAoYWNjb3VudDpzdHJpbmcgYW1vdW50OmRlY2ltYWwpXFxuICAgIEBkb2MgXFxcIkRlYml0IEFNT1VOVCBmcm9tIEFDQ09VTlQgYmFsYW5jZVxcXCJcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgYWNjb3VudCkpXFxuICAgICAgICAgICBdXFxuXFxuICAgICh2YWxpZGF0ZS1hY2NvdW50IGFjY291bnQpXFxuXFxuICAgIChlbmZvcmNlICg-IGFtb3VudCAwLjApXFxuICAgICAgXFxcImRlYml0IGFtb3VudCBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG5cXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKERFQklUIGFjY291bnQpKVxcbiAgICAod2l0aC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbGFuY2UgfVxcblxcbiAgICAgIChlbmZvcmNlICg8PSBhbW91bnQgYmFsYW5jZSkgXFxcIkluc3VmZmljaWVudCBmdW5kc1xcXCIpXFxuXFxuICAgICAgKHVwZGF0ZSBjb2luLXRhYmxlIGFjY291bnRcXG4gICAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogKC0gYmFsYW5jZSBhbW91bnQpIH1cXG4gICAgICAgICkpXFxuICAgIClcXG5cXG5cXG4gIChkZWZ1biBjcmVkaXQ6c3RyaW5nIChhY2NvdW50OnN0cmluZyBndWFyZDpndWFyZCBhbW91bnQ6ZGVjaW1hbClcXG4gICAgQGRvYyBcXFwiQ3JlZGl0IEFNT1VOVCB0byBBQ0NPVU5UIGJhbGFuY2VcXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAoPiBhbW91bnQgMC4wKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IGFjY291bnQpKVxcbiAgICAgICAgICAgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKSBcXFwiY3JlZGl0IGFtb3VudCBtdXN0IGJlIHBvc2l0aXZlXFxcIilcXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgIChyZXF1aXJlLWNhcGFiaWxpdHkgKENSRURJVCBhY2NvdW50KSlcXG4gICAgKHdpdGgtZGVmYXVsdC1yZWFkIGNvaW4tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDogMC4wLCBcXFwiZ3VhcmRcXFwiIDogZ3VhcmQgfVxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbGFuY2UsIFxcXCJndWFyZFxcXCIgOj0gcmV0ZyB9XFxuICAgICAgOyB3ZSBkb24ndCB3YW50IHRvIG92ZXJ3cml0ZSBhbiBleGlzdGluZyBndWFyZCB3aXRoIHRoZSB1c2VyLXN1cHBsaWVkIG9uZVxcbiAgICAgIChlbmZvcmNlICg9IHJldGcgZ3VhcmQpXFxuICAgICAgICBcXFwiYWNjb3VudCBndWFyZHMgZG8gbm90IG1hdGNoXFxcIilcXG5cXG4gICAgICAod3JpdGUgY29pbi10YWJsZSBhY2NvdW50XFxuICAgICAgICB7IFxcXCJiYWxhbmNlXFxcIiA6ICgrIGJhbGFuY2UgYW1vdW50KVxcbiAgICAgICAgLCBcXFwiZ3VhcmRcXFwiICAgOiByZXRnXFxuICAgICAgICB9KVxcbiAgICAgICkpXFxuXFxuXFxuICAoZGVmc2NoZW1hIGNyb3NzY2hhaW4tc2NoZW1hXFxuICAgIEBkb2MgXFxcIlNjaGVtYSBmb3IgeWllbGRlZCB2YWx1ZSBpbiBjcm9zcy1jaGFpbiB0cmFuc2ZlcnNcXFwiXFxuICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICByZWNlaXZlci1ndWFyZDpndWFyZFxcbiAgICBhbW91bnQ6ZGVjaW1hbClcXG5cXG4gIChkZWZwYWN0IHRyYW5zZmVyLWNyb3NzY2hhaW46c3RyaW5nXFxuICAgICggc2VuZGVyOnN0cmluZ1xcbiAgICAgIHJlY2VpdmVyOnN0cmluZ1xcbiAgICAgIHJlY2VpdmVyLWd1YXJkOmd1YXJkXFxuICAgICAgdGFyZ2V0LWNoYWluOnN0cmluZ1xcbiAgICAgIGFtb3VudDpkZWNpbWFsIClcXG5cXG4gICAgQG1vZGVsIFsgKHByb3BlcnR5ICg-IGFtb3VudCAwLjApKVxcbiAgICAgICAgICAgICAocHJvcGVydHkgKHZhbGlkLWFjY291bnQgc2VuZGVyKSlcXG4gICAgICAgICAgICAgKHByb3BlcnR5ICh2YWxpZC1hY2NvdW50IHJlY2VpdmVyKSlcXG4gICAgICAgICAgIF1cXG5cXG4gICAgKHN0ZXBcXG4gICAgICAod2l0aC1jYXBhYmlsaXR5IChERUJJVCBzZW5kZXIpXFxuXFxuICAgICAgICAodmFsaWRhdGUtYWNjb3VudCBzZW5kZXIpXFxuICAgICAgICAodmFsaWRhdGUtYWNjb3VudCByZWNlaXZlcilcXG5cXG4gICAgICAgIChlbmZvcmNlICghPSBcXFwiXFxcIiB0YXJnZXQtY2hhaW4pIFxcXCJlbXB0eSB0YXJnZXQtY2hhaW5cXFwiKVxcbiAgICAgICAgKGVuZm9yY2UgKCE9IChhdCAnY2hhaW4taWQgKGNoYWluLWRhdGEpKSB0YXJnZXQtY2hhaW4pXFxuICAgICAgICAgIFxcXCJjYW5ub3QgcnVuIGNyb3NzLWNoYWluIHRyYW5zZmVycyB0byB0aGUgc2FtZSBjaGFpblxcXCIpXFxuXFxuICAgICAgICAoZW5mb3JjZSAoPiBhbW91bnQgMC4wKVxcbiAgICAgICAgICBcXFwidHJhbnNmZXIgcXVhbnRpdHkgbXVzdCBiZSBwb3NpdGl2ZVxcXCIpXFxuXFxuICAgICAgICAoZW5mb3JjZS11bml0IGFtb3VudClcXG5cXG4gICAgICAgIDs7IHN0ZXAgMSAtIGRlYml0IGRlbGV0ZS1hY2NvdW50IG9uIGN1cnJlbnQgY2hhaW5cXG4gICAgICAgIChkZWJpdCBzZW5kZXIgYW1vdW50KVxcblxcbiAgICAgICAgKGxldFxcbiAgICAgICAgICAoKGNyb3NzY2hhaW4tZGV0YWlsczpvYmplY3R7Y3Jvc3NjaGFpbi1zY2hlbWF9XFxuICAgICAgICAgICAgeyBcXFwicmVjZWl2ZXJcXFwiIDogcmVjZWl2ZXJcXG4gICAgICAgICAgICAsIFxcXCJyZWNlaXZlci1ndWFyZFxcXCIgOiByZWNlaXZlci1ndWFyZFxcbiAgICAgICAgICAgICwgXFxcImFtb3VudFxcXCIgOiBhbW91bnRcXG4gICAgICAgICAgICB9KSlcXG4gICAgICAgICAgKHlpZWxkIGNyb3NzY2hhaW4tZGV0YWlscyB0YXJnZXQtY2hhaW4pXFxuICAgICAgICAgICkpKVxcblxcbiAgICAoc3RlcFxcbiAgICAgIChyZXN1bWVcXG4gICAgICAgIHsgXFxcInJlY2VpdmVyXFxcIiA6PSByZWNlaXZlclxcbiAgICAgICAgLCBcXFwicmVjZWl2ZXItZ3VhcmRcXFwiIDo9IHJlY2VpdmVyLWd1YXJkXFxuICAgICAgICAsIFxcXCJhbW91bnRcXFwiIDo9IGFtb3VudFxcbiAgICAgICAgfVxcblxcbiAgICAgICAgOzsgc3RlcCAyIC0gY3JlZGl0IGNyZWF0ZSBhY2NvdW50IG9uIHRhcmdldCBjaGFpblxcbiAgICAgICAgKHdpdGgtY2FwYWJpbGl0eSAoQ1JFRElUIHJlY2VpdmVyKVxcbiAgICAgICAgICAoY3JlZGl0IHJlY2VpdmVyIHJlY2VpdmVyLWd1YXJkIGFtb3VudCkpXFxuICAgICAgICApKVxcbiAgICApXFxuXFxuXFxuICA7IC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXFxuICA7IENvaW4gYWxsb2NhdGlvbnNcXG5cXG4gIChkZWZzY2hlbWEgYWxsb2NhdGlvbi1zY2hlbWFcXG4gICAgQGRvYyBcXFwiR2VuZXNpcyBhbGxvY2F0aW9uIHJlZ2lzdHJ5XFxcIlxcbiAgICA7QG1vZGVsIFsgKGludmFyaWFudCAoPj0gYmFsYW5jZSAwLjApKSBdXFxuXFxuICAgIGJhbGFuY2U6ZGVjaW1hbFxcbiAgICBkYXRlOnRpbWVcXG4gICAgZ3VhcmQ6Z3VhcmRcXG4gICAgcmVkZWVtZWQ6Ym9vbClcXG5cXG4gIChkZWZ0YWJsZSBhbGxvY2F0aW9uLXRhYmxlOnthbGxvY2F0aW9uLXNjaGVtYX0pXFxuXFxuICAoZGVmdW4gY3JlYXRlLWFsbG9jYXRpb24tYWNjb3VudFxcbiAgICAoIGFjY291bnQ6c3RyaW5nXFxuICAgICAgZGF0ZTp0aW1lXFxuICAgICAga2V5c2V0LXJlZjpzdHJpbmdcXG4gICAgICBhbW91bnQ6ZGVjaW1hbFxcbiAgICApXFxuXFxuICAgIEBkb2MgXFxcIkFkZCBhbiBlbnRyeSB0byB0aGUgY29pbiBhbGxvY2F0aW9uIHRhYmxlLiBUaGlzIGZ1bmN0aW9uIFxcXFxcXG4gICAgICAgICBcXFxcYWxzbyBjcmVhdGVzIGEgY29ycmVzcG9uZGluZyBlbXB0eSBjb2luIGNvbnRyYWN0IGFjY291bnQgXFxcXFxcbiAgICAgICAgIFxcXFxvZiB0aGUgc2FtZSBuYW1lIGFuZCBndWFyZC4gUmVxdWlyZXMgR0VORVNJUyBjYXBhYmlsaXR5LiBcXFwiXFxuXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBhY2NvdW50KSkgXVxcblxcbiAgICAocmVxdWlyZS1jYXBhYmlsaXR5IChHRU5FU0lTKSlcXG5cXG4gICAgKHZhbGlkYXRlLWFjY291bnQgYWNjb3VudClcXG4gICAgKGVuZm9yY2UgKD49IGFtb3VudCAwLjApXFxuICAgICAgXFxcImFsbG9jYXRpb24gYW1vdW50IG11c3QgYmUgbm9uLW5lZ2F0aXZlXFxcIilcXG5cXG4gICAgKGVuZm9yY2UtdW5pdCBhbW91bnQpXFxuXFxuICAgIChsZXRcXG4gICAgICAoKGd1YXJkOmd1YXJkIChrZXlzZXQtcmVmLWd1YXJkIGtleXNldC1yZWYpKSlcXG5cXG4gICAgICAoY3JlYXRlLWFjY291bnQgYWNjb3VudCBndWFyZClcXG5cXG4gICAgICAoaW5zZXJ0IGFsbG9jYXRpb24tdGFibGUgYWNjb3VudFxcbiAgICAgICAgeyBcXFwiYmFsYW5jZVxcXCIgOiBhbW91bnRcXG4gICAgICAgICwgXFxcImRhdGVcXFwiIDogZGF0ZVxcbiAgICAgICAgLCBcXFwiZ3VhcmRcXFwiIDogZ3VhcmRcXG4gICAgICAgICwgXFxcInJlZGVlbWVkXFxcIiA6IGZhbHNlXFxuICAgICAgICB9KSkpXFxuXFxuICAoZGVmdW4gcmVsZWFzZS1hbGxvY2F0aW9uXFxuICAgICggYWNjb3VudDpzdHJpbmcgKVxcblxcbiAgICBAZG9jIFxcXCJSZWxlYXNlIGZ1bmRzIGFzc29jaWF0ZWQgd2l0aCBhbGxvY2F0aW9uIEFDQ09VTlQgaW50byBtYWluIGxlZGdlci4gICBcXFxcXFxuICAgICAgICAgXFxcXEFDQ09VTlQgbXVzdCBhbHJlYWR5IGV4aXN0IGluIG1haW4gbGVkZ2VyLiBBbGxvY2F0aW9uIGlzIGRlYWN0aXZhdGVkIFxcXFxcXG4gICAgICAgICBcXFxcYWZ0ZXIgcmVsZWFzZS5cXFwiXFxuICAgIEBtb2RlbCBbIChwcm9wZXJ0eSAodmFsaWQtYWNjb3VudCBhY2NvdW50KSkgXVxcblxcbiAgICAodmFsaWRhdGUtYWNjb3VudCBhY2NvdW50KVxcblxcbiAgICAod2l0aC1yZWFkIGFsbG9jYXRpb24tdGFibGUgYWNjb3VudFxcbiAgICAgIHsgXFxcImJhbGFuY2VcXFwiIDo9IGJhbGFuY2VcXG4gICAgICAsIFxcXCJkYXRlXFxcIiA6PSByZWxlYXNlLXRpbWVcXG4gICAgICAsIFxcXCJyZWRlZW1lZFxcXCIgOj0gcmVkZWVtZWRcXG4gICAgICAsIFxcXCJndWFyZFxcXCIgOj0gZ3VhcmRcXG4gICAgICB9XFxuXFxuICAgICAgKGxldCAoKGN1cnItdGltZTp0aW1lIChhdCAnYmxvY2stdGltZSAoY2hhaW4tZGF0YSkpKSlcXG5cXG4gICAgICAgIChlbmZvcmNlIChub3QgcmVkZWVtZWQpXFxuICAgICAgICAgIFxcXCJhbGxvY2F0aW9uIGZ1bmRzIGhhdmUgYWxyZWFkeSBiZWVuIHJlZGVlbWVkXFxcIilcXG5cXG4gICAgICAgIChlbmZvcmNlXFxuICAgICAgICAgICg-PSBjdXJyLXRpbWUgcmVsZWFzZS10aW1lKVxcbiAgICAgICAgICAoZm9ybWF0IFxcXCJmdW5kcyBsb2NrZWQgdW50aWwge30uIGN1cnJlbnQgdGltZToge31cXFwiIFtyZWxlYXNlLXRpbWUgY3Vyci10aW1lXSkpXFxuXFxuICAgICAgICAoZW5mb3JjZS1ndWFyZCBndWFyZClcXG5cXG4gICAgICAgICh3aXRoLWNhcGFiaWxpdHkgKENSRURJVCBhY2NvdW50KVxcbiAgICAgICAgICAoY3JlZGl0IGFjY291bnQgZ3VhcmQgYmFsYW5jZSlcXG5cXG4gICAgICAgICAgKHVwZGF0ZSBhbGxvY2F0aW9uLXRhYmxlIGFjY291bnRcXG4gICAgICAgICAgICB7IFxcXCJyZWRlZW1lZFxcXCIgOiB0cnVlXFxuICAgICAgICAgICAgLCBcXFwiYmFsYW5jZVxcXCIgOiAwLjBcXG4gICAgICAgICAgICB9KVxcblxcbiAgICAgICAgICBcXFwiQWxsb2NhdGlvbiBzdWNjZXNzZnVsbHkgcmVsZWFzZWQgdG8gbWFpbiBsZWRnZXJcXFwiKVxcbiAgICApKSlcXG5cXG4pXFxuXCJ9fSxcInNpZ25lcnNcIjpbXSxcIm1ldGFcIjp7XCJjcmVhdGlvblRpbWVcIjowLFwidHRsXCI6MTcyODAwLFwiZ2FzTGltaXRcIjowLFwiY2hhaW5JZFwiOlwiXCIsXCJnYXNQcmljZVwiOjAsXCJzZW5kZXJcIjpcIlwifSxcIm5vbmNlXCI6XCJjb2luLWNvbnRyYWN0LXYyXCJ9In0"
--     ]

-- NOTE (linda): When adding new forking transactions that are injected
-- into a block's coinbase transaction, please add a corresponding case
-- in Rosetta's `matchLogs` (Chainweb.Rosetta.Internal.hs) function and
-- follow the coinv3 pattern.
--
-- Otherwise, Rosetta tooling has no idea that these upgrade transactions
-- occurred.
-- This is especially important if the transaction changes an account's balance.
-- Rosetta tooling will error out if an account's balance changed and it
-- didn't see the transaction that caused the change.
--
