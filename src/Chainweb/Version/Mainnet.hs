{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module Chainweb.Version.Mainnet(mainnet) where

import Control.Lens
import qualified Data.HashMap.Strict as HM
import Data.Semigroup.Traversable

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Version
import P2P.BootstrapNodes

import qualified Chainweb.BlockHeader.Genesis.Mainnet0Payload as MN0
import qualified Chainweb.BlockHeader.Genesis.Mainnet1Payload as MN1
import qualified Chainweb.BlockHeader.Genesis.Mainnet2Payload as MN2
import qualified Chainweb.BlockHeader.Genesis.Mainnet3Payload as MN3
import qualified Chainweb.BlockHeader.Genesis.Mainnet4Payload as MN4
import qualified Chainweb.BlockHeader.Genesis.Mainnet5Payload as MN5
import qualified Chainweb.BlockHeader.Genesis.Mainnet6Payload as MN6
import qualified Chainweb.BlockHeader.Genesis.Mainnet7Payload as MN7
import qualified Chainweb.BlockHeader.Genesis.Mainnet8Payload as MN8
import qualified Chainweb.BlockHeader.Genesis.Mainnet9Payload as MN9
import qualified Chainweb.BlockHeader.Genesis.MainnetKADPayload as MNKAD
import qualified Chainweb.Pact.Transactions.CoinV3Transactions as CoinV3
import qualified Chainweb.Pact.Transactions.CoinV4Transactions as CoinV4
import qualified Chainweb.Pact.Transactions.CoinV5Transactions as CoinV5
import qualified Chainweb.Pact.Transactions.Mainnet0Transactions as MN0
import qualified Chainweb.Pact.Transactions.Mainnet1Transactions as MN1
import qualified Chainweb.Pact.Transactions.Mainnet2Transactions as MN2
import qualified Chainweb.Pact.Transactions.Mainnet3Transactions as MN3
import qualified Chainweb.Pact.Transactions.Mainnet4Transactions as MN4
import qualified Chainweb.Pact.Transactions.Mainnet5Transactions as MN5
import qualified Chainweb.Pact.Transactions.Mainnet6Transactions as MN6
import qualified Chainweb.Pact.Transactions.Mainnet7Transactions as MN7
import qualified Chainweb.Pact.Transactions.Mainnet8Transactions as MN8
import qualified Chainweb.Pact.Transactions.Mainnet9Transactions as MN9
import qualified Chainweb.Pact.Transactions.MainnetKADTransactions as MNKAD

-- | Initial hash target for mainnet 20-chain transition. Difficulty on the new
-- chains is 1/4 of the current difficulty. It is based on the following header
-- from 2020-07-09. This value should be double checked after the testnet
-- transition and before the release of chainweb node version 2.1.
--
-- @
-- {
--   "creationTime": 1594319266887602,
--   "parent": "aSIkDjuJQGGOwJW-60T_1WRK9KPJm1rz63a4SW8WtSc",
--   "height": 731382,
--   "hash": "Ua_pSMMo-szlMpXMuSYWTcVlaSIf01TxJvBCmFkmhBM",
--   "chainId": 0,
--   "weight": "xo3dabqEYpUPAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
--   "featureFlags": 0,
--   "epochStart": 1594316109999615,
--   "adjacents": {
--     "2": "KuuujcD6yeZ9jRXwlRE0ed5dHc3x_akIz1REmKXuDtk",
--     "5": "qFU32Qmlj-syzuZ2awCvyoW6Jex3TQqGTzd-Dchn1gc",
--     "3": "Lgu1FgiCw4qPpptoVRmijn8WKG2OcAUAp1Ha7KFbrWg"
--   },
--   "payloadHash": "MV079yClHYSYBW74WySK-15AUVQg8QMKHJZbtzTCbgA",
--   "chainwebVersion": "mainnet01",
--   "target": "DOordl9cgfs4ZTBdFnbjRW5th-hW-pL33DIAAAAAAAA",
--   "nonce": "149742924667593745"
-- }
-- @
--
-- It holds that:
--
-- prop> Just mainnet20InitialHashTarget == HashTarget . (4 *) <$> (runGet decodePowHashNat =<< decodeB64UrlNoPaddingText "DOordl9cgfs4ZTBdFnbjRW5th-hW-pL33DIAAAAAAAA")
--
mainnet20InitialHashTarget :: HashTarget
mainnet20InitialHashTarget = HashTarget 0x000000000000cb73de4be95ba21db5b9178dd85974c194e3ee05717dd8afa830

to20ChainsMainnet :: BlockHeight
to20ChainsMainnet = 852_054 -- 2020-08-20 16:00:00

mainnet :: ChainwebVersion
mainnet = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000005
    , _versionName = ChainwebVersionName "mainnet01"
    , _versionForks = HM.fromList
        [ (Chainweb217Pact, AllChains (BlockHeight 3_250_348)) -- 2022-12-02T00:00:00+00:00
        , (Chainweb216Pact, AllChains (BlockHeight 2_988_324)) -- 2022-09-02T00:00:00+00:00
        , (Chainweb215Pact, AllChains (BlockHeight 2_766_630)) -- 2022-06-17T00:00:00+00:00
        , (Chainweb214Pact, AllChains (BlockHeight 2_605_663)) -- 2022-04-22T00:00:00+00:00
        , (Chainweb213Pact, AllChains (BlockHeight 2_447_315)) -- 2022-02-26T00:00:00+00:00
        , (Pact420, AllChains (BlockHeight 2_334_500))         -- 2022-01-17T17:51:12+00:00
        , (Pact4Coin3, AllChains (BlockHeight 1_722_500))      -- 2021-06-19T03:34:05+00:00
        , (CoinV2, onChains $
            [ (unsafeChainId 0, BlockHeight 140_808)
            , (unsafeChainId 1, BlockHeight 140_809)
            , (unsafeChainId 2, BlockHeight 140_808)
            , (unsafeChainId 3, BlockHeight 140_809)
            , (unsafeChainId 4, BlockHeight 140_808)
            , (unsafeChainId 5, BlockHeight 140_808)
            , (unsafeChainId 6, BlockHeight 140_808)
            , (unsafeChainId 7, BlockHeight 140_809)
            , (unsafeChainId 8, BlockHeight 140_808)
            , (unsafeChainId 9, BlockHeight 140_808)
            ])
        , (SlowEpoch, AllChains 80_000)
        , (OldTargetGuard, AllChains 452_820) -- ~ 2020-04-04T00:00:00Z
        , (SkipFeatureFlagValidation, AllChains 530_500) -- ~ 2020-05-01T00:00:xxZ
        , (OldDAGuard, AllChains 771_414) -- ~ 2020-07-23 16:00:00
        , (Vuln797Fix, onChains $
            [ (unsafeChainId 0, BlockHeight 121_452) -- 2019-12-10T21:00:00.0
            , (unsafeChainId 1, BlockHeight 121_452)
            , (unsafeChainId 2, BlockHeight 121_452)
            , (unsafeChainId 3, BlockHeight 121_451)
            , (unsafeChainId 4, BlockHeight 121_451)
            , (unsafeChainId 5, BlockHeight 121_452)
            , (unsafeChainId 6, BlockHeight 121_452)
            , (unsafeChainId 7, BlockHeight 121_451)
            , (unsafeChainId 8, BlockHeight 121_452)
            , (unsafeChainId 9, BlockHeight 121_451)
            ] <> [(unsafeChainId i, BlockHeight 0) | i <- [10..19]])
        , (PactBackCompat_v16, AllChains 328_000)
        , (SkipTxTimingValidation, AllChains 449_940)
        , (ModuleNameFix, AllChains 448_501)
        , (ModuleNameFix2, AllChains 752_214)
        , (PactEvents, AllChains 1_138_000)
        , (SPVBridge, AllChains 1_275_000)
        , (EnforceKeysetFormats, AllChains 2_162_000) -- 2022-01-17T17:51:12
        , (CheckTxHash, AllChains 2_349_800) -- 2022-01-23T02:53:38
        , (Pact44NewTrans, AllChains 2_965_885) -- Todo: add date
        ]
        -- , onChains $
        --         [ (unsafeChainId 0, (BlockHeight 140_808, ForkEvent CoinV2 MN0.transactions))
        --         , (unsafeChainId 1, (BlockHeight 140_809, ForkEvent CoinV2 MN1.transactions))
        --         , (unsafeChainId 2, (BlockHeight 140_808, ForkEvent CoinV2 MN2.transactions))
        --         , (unsafeChainId 3, (BlockHeight 140_809, ForkEvent CoinV2 MN3.transactions))
        --         , (unsafeChainId 4, (BlockHeight 140_808, ForkEvent CoinV2 MN4.transactions))
        --         , (unsafeChainId 5, (BlockHeight 140_808, ForkEvent CoinV2 MN5.transactions))
        --         , (unsafeChainId 6, (BlockHeight 140_808, ForkEvent CoinV2 MN6.transactions))
        --         , (unsafeChainId 7, (BlockHeight 140_809, ForkEvent CoinV2 MN7.transactions))
        --         , (unsafeChainId 8, (BlockHeight 140_808, ForkEvent CoinV2 MN8.transactions))
        --         , (unsafeChainId 9, (BlockHeight 140_808, ForkEvent CoinV2 MN9.transactions))
        --         ] <> [(unsafeChainId i, (BlockHeight 1, ForkEvent CoinV2 [])) | i <- [10..19]]
        -- ]
        -- foldr (\l r -> Above <$> l <*> r) (AllChains $ End (ForkEvent Genesis []))
        -- [ AllChains (BlockHeight 3_250_348, ForkEvent Chainweb217Pact []) -- 2022-12-02T00:00:00+00:00
        -- , AllChains (BlockHeight 2_988_324, ForkEvent Chainweb216Pact []) -- 2022-09-02T00:00:00+00:00
        -- , AllChains (BlockHeight 2_766_630, ForkEvent Chainweb215Pact []) -- 2022-06-17T00:00:00+00:00
        -- , AllChains (BlockHeight 2_605_663, ForkEvent Chainweb214Pact []) -- 2022-04-22T00:00:00+00:00
        -- , AllChains (BlockHeight 2_447_315, ForkEvent Chainweb213Pact []) -- 2022-02-26 00:00:00
        -- , AllChains (BlockHeight 2_334_500, ForkEvent Pact420 []) -- 2022-01-17T17:51:12
        -- , AllChains (BlockHeight 1_722_500, ForkEvent Pact4Coin3 []) -- 2021-06-19T03:34:05
        -- , onChains $
        --         [ (unsafeChainId 0, (BlockHeight 140_808, ForkEvent CoinV2 MN0.transactions))
        --         , (unsafeChainId 1, (BlockHeight 140_809, ForkEvent CoinV2 MN1.transactions))
        --         , (unsafeChainId 2, (BlockHeight 140_808, ForkEvent CoinV2 MN2.transactions))
        --         , (unsafeChainId 3, (BlockHeight 140_809, ForkEvent CoinV2 MN3.transactions))
        --         , (unsafeChainId 4, (BlockHeight 140_808, ForkEvent CoinV2 MN4.transactions))
        --         , (unsafeChainId 5, (BlockHeight 140_808, ForkEvent CoinV2 MN5.transactions))
        --         , (unsafeChainId 6, (BlockHeight 140_808, ForkEvent CoinV2 MN6.transactions))
        --         , (unsafeChainId 7, (BlockHeight 140_809, ForkEvent CoinV2 MN7.transactions))
        --         , (unsafeChainId 8, (BlockHeight 140_808, ForkEvent CoinV2 MN8.transactions))
        --         , (unsafeChainId 9, (BlockHeight 140_808, ForkEvent CoinV2 MN9.transactions))
        --         ] <> [(unsafeChainId i, (BlockHeight 1, ForkEvent CoinV2 [])) | i <- [10..19]]
        -- ]
    -- , _versionFlags = sequence1 Flags
        -- { _slowEpochGuardFlag = AllChains 80_000
        -- , _oldTargetGuardFlag = AllChains 452_820 -- ~ 2020-04-04T00:00:00Z
        -- , _skipFeatureFlagValidationFlag = AllChains 530_500 -- ~ 2020-05-01T00:00:xxZ
        -- , _oldDaGuardFlag = AllChains 771_414 -- ~ 2020-07-23 16:00:00
        -- , _vuln797FixFlag = OnChains $ HM.fromList $
        --     [ (unsafeChainId 0, BlockHeight 121_452) -- 2019-12-10T21:00:00.0
        --     , (unsafeChainId 1, BlockHeight 121_452)
        --     , (unsafeChainId 2, BlockHeight 121_452)
        --     , (unsafeChainId 3, BlockHeight 121_451)
        --     , (unsafeChainId 4, BlockHeight 121_451)
        --     , (unsafeChainId 5, BlockHeight 121_452)
        --     , (unsafeChainId 6, BlockHeight 121_452)
        --     , (unsafeChainId 7, BlockHeight 121_451)
        --     , (unsafeChainId 8, BlockHeight 121_452)
        --     , (unsafeChainId 9, BlockHeight 121_451)
        --     ] <> [(unsafeChainId i, BlockHeight 0) | i <- [10..19]]
        -- , _pactBackCompat_v16Flag = AllChains 328_000
        -- , _skipTxTimingValidationFlag = AllChains 449_940
        -- , _moduleNameFixFlag = AllChains 448_501
        -- , _moduleNameFix2Flag = AllChains 752_214
        -- , _pactEventsFlag = AllChains 1_138_000
        -- , _spvBridgeFlag = AllChains 1_275_000
        -- , _enforceKeysetFormatsFlag = AllChains 2_162_000 -- 2022-01-17T17:51:12
        -- , _checkTxHashFlag = AllChains 2_349_800 -- 2022-01-23T02:53:38
        -- , _pact44NewTransFlag = AllChains 2_965_885 -- Todo: add date
        -- }
    , _versionGraphs =
        (to20ChainsMainnet, twentyChainGraph) `Above`
        End petersonChainGraph
    , _versionBlockRate = BlockRate 30
    , _versionWindow = Just $ WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionMaxBlockGasLimit =
        (mainnet ^?! versionForks . at Chainweb216Pact . _Just . onChain (unsafeChainId 0), Just 180_000) `Above`
        End Nothing
    , _versionFakeFirstEpochStart = False
    , _versionBootstraps = domainAddr2PeerInfo mainnetBootstrapHosts
    , _versionGenesis = ChainwebGenesis
        { _genesisBlockTarget = OnChains $ HM.fromList $ concat
            [ [(unsafeChainId i, maxTarget) | i <- [0..9]]
            , [(unsafeChainId i, mainnet20InitialHashTarget) | i <- [10..19]]
            ]
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-10-30T00:01:00.0 |]
        , _genesisBlockPayload = OnChains $ HM.fromList $ concat
            [ [ (unsafeChainId 0, MN0.payloadBlock)
              , (unsafeChainId 1, MN1.payloadBlock)
              , (unsafeChainId 2, MN2.payloadBlock)
              , (unsafeChainId 3, MN3.payloadBlock)
              , (unsafeChainId 4, MN4.payloadBlock)
              , (unsafeChainId 5, MN5.payloadBlock)
              , (unsafeChainId 6, MN6.payloadBlock)
              , (unsafeChainId 7, MN7.payloadBlock)
              , (unsafeChainId 8, MN8.payloadBlock)
              , (unsafeChainId 9, MN9.payloadBlock)
              , (unsafeChainId 9, MN9.payloadBlock)
              ]
            , [(unsafeChainId i, MNKAD.payloadBlock) | i <- [10..19]]
            ]
        }
    -- , _versionUpgradeTransactions = UpgradeTransactions
    --     { _coinV2Transactions = OnChains $ HM.fromList $
    --         [ (unsafeChainId 0, MN0.transactions)
    --         , (unsafeChainId 1, MN1.transactions)
    --         , (unsafeChainId 2, MN2.transactions)
    --         , (unsafeChainId 3, MN3.transactions)
    --         , (unsafeChainId 4, MN4.transactions)
    --         , (unsafeChainId 5, MN5.transactions)
    --         , (unsafeChainId 6, MN6.transactions)
    --         , (unsafeChainId 7, MN7.transactions)
    --         , (unsafeChainId 8, MN8.transactions)
    --         , (unsafeChainId 9, MN9.transactions)
    --         ] <> [(unsafeChainId i, []) | i <- [10..19]]
    --     , _coinV3Transactions = AllChains CoinV3.transactions
    --     , _coinV4Transactions = AllChains CoinV4.transactions
    --     , _coinV5Transactions = AllChains CoinV5.transactions
    --     , _to20ChainTransactions = OnChains $ HM.fromList $ concat $
    --         [ [(unsafeChainId 0, MNKAD.transactions)]
    --         , [(unsafeChainId i, []) | i <- [1..19]]
    --         ]
    --     }
    , _versionCheats = Cheats
        { _disablePeerValidation = False
        , _disablePow = False
        , _disablePact = False
        }
    }
