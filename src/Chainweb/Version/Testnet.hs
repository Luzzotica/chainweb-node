module Chainweb.Version.Testnet(testnetVersion) where

-- | Initial hash target for testnet 20-chain transition. Based on the following
-- header from devnet running with 5 GPUs hash power. Using this target unchanged
-- means, that we should do to the transition with the hash power of about
-- 5 - 50 GPUs in the system for a smooth transition.
--
-- The value for the initial target is 38 times smaller larger than value of an
-- successful test run on devnet with 5 GPUs. During that test the initial
-- target was about 32 times larger than the actual target at the time of the
-- transition.
--
-- @
-- {
--   "creationTime": 1594433454304125,
--   "parent": "DHSarVwhj6Xvu0KewCI1nRdGcNSWKFoOUy7us27mDac",
--   "height": 200,
--   "hash": "DC8HV9W0JM5gzliwDupjG10Lnwav09xWtxy01kGPTLM",
--   "chainId": 0,
--   "weight": "ReZ2aCAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
--   "featureFlags": 0,
--   "epochStart": 1594430808323849,
--   "adjacents": {
--     "2": "JPbz_YjWIvDgdGxdemkU6vVRimZZawxY_j0Hwo0pzb0",
--     "5": "wMFfoFrQ1GWOFj6jCNGRa3SiuFRGOCmS06F7HfmLnNw",
--     "3": "9WIBnxDGGZsy9FCCorvAUa4SlE5Rqs-cTLEsWCPOVbQ"
--   },
--   "payloadHash": "AOYQdE5xl_YueZSppW4MoadasjF149K28CON2GuH9Mc",
--   "chainwebVersion": "development",
--   "target": "NZIklpW6xujSPrX3gyhXInfxxOS6JDjkW_GbGwAAAAA",
--   "nonce": "5805155470630695"
-- }
-- @
--
-- It holds that:
--
-- prop> Just testnet20InitialHashTarget == HashTarget <$> (runGet decodePowHashNat =<< decodeB64UrlNoPaddingText "NZIklpW6xujSPrX3gyhXInfxxOS6JDjkW_GbGwAAAAA")
-- prop> _hashTarget testnet20InitialHashTarget `div` _hashTarget mainnet20InitialHashTarget == PowHashNat 8893
-- prop> _hashTarget (genesisBlockTarget Development (unsafeChainId 10)) `div` _hashTarget testnet20InitialHashTarget == PowHashNat 38
--
testnet20InitialHashTarget :: HashTarget
testnet20InitialHashTarget = HashTarget 0x000000001b9bf15be43824bae4c4f17722572883f7b53ed2e8c6ba9596249235

to20ChainsTestnet :: BlockHeight
to20ChainsTestnet = 332_604 -- 2020-07-28 16:00:00

testnetVersion :: ChainwebVersion
testnetVersion = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000007
    , _versionName = ChainwebVersionName "testnet04"
    , _versionForks =
        HeightCons (AllChains 2_777_367) Chainweb217Pact $ -- 2022-12-01 12:00:00+00:00
        HeightCons (AllChains 2_516_739) Chainweb216Pact $ -- 2022-09-01 12:00:00+00:00
        HeightCons (AllChains 2_295_437) Chainweb215Pact $ -- 2022-06-16T12:00:00+00:00
        HeightCons (AllChains 2_134_331) Chainweb214Pact $ -- 2022-04-21T12:00:00Z
        HeightCons (AllChains 1_974_556) Chainweb213Pact $ -- 2022-02-25 00:00:00
        HeightCons (AllChains 1_862_000) Pact420 $ -- 2021-06-19T03:34:05
        HeightCons (AllChains 1_261_000) Pact4Coin3 $ -- 2021-06-17T15:54:14
        HeightCons
            (OnChains $ HM.fromList $ concat
                [ [(unsafeChainId i, BlockHeight 1) | i <- [0..19]]
                , [(unsafeChainId i, BlockHeight 337_000) | i <- [10..19]]
                ]
            ) CoinV2 $
        HeightNil Genesis
    , _versionFlags = Flags
        { _slowEpochGuardFlag = AllChains 0
        , _oldTargetGuardFlag = AllChains 0
        , _skipFeatureFlagValidationFlag = AllChains 0
        , _oldDaGuardFlag = AllChains 318_204 -- ~ 2020-07-23 16:00:00
        , _vuln797FixFlag = AllChains 0
        , _pactBackCompat_v16Flag = AllChains 0
        , _skipTxTimingValidationFlag = AllChains 1
        , _moduleNameFixFlag = AllChains 2
        , _moduleNameFix2Flag = AllChains 289_966 -- ~ 2020-07-13
        , _pactEventsFlag = AllChains 660_000
        , _spvBridgeFlag = AllChains 820_000 -- 2021-01-14T17:12:02
        , _enforceKeysetFormatsFlag = AllChains 1_701_000 -- 2021-11-18T17:54:36
        , _checkTxHashFlag = AllChains 1_889_000 -- 2022-01-24T04:19:24
        , _pact44NewTransFlag = AllChains 2_500_369 -- Todo: add date
        }
    , _versionGraphs =
        HeightCons to20ChainsTestnet twentyChainGraph $
        HeightNil petersonChainGraph
    , _versionBlockRate = BlockRate 30
    , _versionWindow = Just $ WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionMaxBlockGasLimit =
        HeightCons
            (heightMapElemHeight Chainweb216Pact (_versionForks testnetVersion) ^?! _Just . _Just . onChain (unsafeChainId 0))
            (Just 180_000) $
        HeightNil Nothing
    , _versionFakeFirstEpochStart = False
    , _versionBootstraps = domainAddr2PeerInfo testnetBootstrapHosts
    , _versionGenesis = ChainwebGenesis
        { _genesisBlockTarget = OnChains $ HM.fromList $ concat
            [ [(unsafeChainId i, maxTarget) | i <- [0..9]]
            , [(unsafeChainId i, testnet20InitialHashTarget) | i <- [10..19]]
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
    , _versionUpgradeTransactions = UpgradeTransactions
        { _coinV2Transactions = OnChains $ HM.fromList $
            [ (unsafeChainId 0, MN0.transactions)
            , (unsafeChainId 1, MN1.transactions)
            , (unsafeChainId 2, MN2.transactions)
            , (unsafeChainId 3, MN3.transactions)
            , (unsafeChainId 4, MN4.transactions)
            , (unsafeChainId 5, MN5.transactions)
            , (unsafeChainId 6, MN6.transactions)
            , (unsafeChainId 7, MN7.transactions)
            , (unsafeChainId 8, MN8.transactions)
            , (unsafeChainId 9, MN9.transactions)
            ] <> [(unsafeChainId i, []) | i <- [10..19]]
        , _coinV3Transactions = AllChains CoinV3.transactions
        , _coinV4Transactions = AllChains CoinV4.transactions
        , _coinV5Transactions = AllChains CoinV5.transactions
        , _to20ChainTransactions = OnChains $ HM.fromList $ concat $
            [ [(unsafeChainId 0, MNKAD.transactions)]
            , [(unsafeChainId i, []) | i <- [1..19]]
            ]
        }
    , _versionCheats = Cheats
        { _disablePeerValidation = False
        , _disablePow = False
        , _disablePact = False
        }
    }
