module Test.Main where

import Prelude

import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Types.Language (Language(PlutusV1))
import Cardano.Types.NetworkId (NetworkId(MainnetId, TestnetId))
import Cardano.Types.OutputDatum (OutputDatum(OutputDatum))
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript (decodeCbor, encodeCbor) as PlutusScript
import Cardano.Types.Relay (Relay(SingleHostAddr, SingleHostName, MultiHostName))
import Cardano.Types.ScriptRef (ScriptRef(PlutusScriptRef))
import Data.Array as Array
import Data.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe, byteArrayToHex, hexToByteArray)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Mote (group, test)
import Mote.TestPlanM (TestPlanM, interpretWithConfig)
import Test.CSLHex as CSLHex
import Test.Fixtures
  ( auxiliaryDataFixture1
  , auxiliaryDataFixture2
  , certsFixture1
  , int1
  , mint0
  , mint1
  , nativeScriptFixture1
  , nativeScriptFixture2
  , nativeScriptFixture3
  , nativeScriptFixture4
  , nativeScriptFixture5
  , nativeScriptFixture6
  , nativeScriptFixture7
  , plutusDataFixture1
  , plutusDataFixture11
  , plutusDataFixture2
  , plutusDataFixture3
  , plutusDataFixture4
  , plutusDataFixture5
  , plutusDataFixture6
  , plutusDataFixture7
  , plutusDataFixture8
  , plutusScriptFixture1
  , plutusScriptFixture2
  , plutusScriptFixture3
  , stake1
  , stake2
  , txBodyFixture1
  , txFixture1
  , txFixture2
  , txFixture3
  , txFixture4
  , txFixture5
  , txFixture6
  , txFixture7
  , txInputFixture1
  , txOutputFixture1
  , txOutputFixture2
  , txOutputFixture3
  , witnessSetFixture2Value
  , witnessSetFixture3Value
  )
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (defaultConfig)
import Type.Proxy (Proxy)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "deserialization and serialization roundtrip" $ do
    group "PlutusScript" do
      roundtripTest "plutusScriptFixture1" $ PlutusScriptV1 plutusScriptFixture1
    -- These are not supposed to work: plutus script version is not encoded
    -- in the cbor. instead, TransactionWitness using the scripts is encoded
    -- differently depending on the language version. See the CDDL spec of Cardano
    -- ledger.

    -- roundtripTest "plutusScriptFixture2" plutusScriptFixture2
    -- roundtripTest "plutusScriptFixture3" plutusScriptFixture3
    group "NativeScript" do
      roundtripTest "nativeScriptFixture1" nativeScriptFixture1
      roundtripTest "nativeScriptFixture2" nativeScriptFixture2
      roundtripTest "nativeScriptFixture3" nativeScriptFixture3
      roundtripTest "nativeScriptFixture4" nativeScriptFixture4
      roundtripTest "nativeScriptFixture5" nativeScriptFixture5
      roundtripTest "nativeScriptFixture6" nativeScriptFixture6
      roundtripTest "nativeScriptFixture7" nativeScriptFixture7
    group "PlutusData" do
      roundtripTest "plutusDataFixture1" plutusDataFixture1
      roundtripTest "plutusDataFixture2" plutusDataFixture2
      roundtripTest "plutusDataFixture3" plutusDataFixture3
      roundtripTest "plutusDataFixture4" plutusDataFixture4
      roundtripTest "plutusDataFixture5" plutusDataFixture5
      roundtripTest "plutusDataFixture6" plutusDataFixture6
      roundtripTest "plutusDataFixture7" plutusDataFixture7
      roundtripTest "plutusDataFixture8" plutusDataFixture8
      -- CDL does not handle maps with duplicated keys.
      -- roundtripTest "plutusDataFixture9" plutusDataFixture9
      -- roundtripTest "plutusDataFixture10" plutusDataFixture10
      roundtripTest "plutusDataFixture11" plutusDataFixture11
    group "OutputDatum" do
      roundtripTest "OutputDatum plutusDataFixture1" (OutputDatum plutusDataFixture1)
    group "PlutusScriptRef" do
      roundtripTest "PlutusScriptRef plutusScriptFixture1" (PlutusScriptRef plutusScriptFixture1)
      roundtripTest "PlutusScriptRef plutusScriptFixture2" (PlutusScriptRef plutusScriptFixture2)
      roundtripTest "PlutusScriptRef plutusScriptFixture3" (PlutusScriptRef plutusScriptFixture3)
    group "AuxiliaryData" do
      roundtripTest "auxiliaryDataFixture1" auxiliaryDataFixture1
      roundtripTest "auxiliaryDataFixture2" auxiliaryDataFixture2
    group "Relays" do
      roundtripTest "relay1"
        ( SingleHostAddr
            { port: Just 8080
            , ipv4: decodeCbor $ wrap $ byteArrayFromIntArrayUnsafe
                [ 127, 0, 0, 1 ]
            , ipv6: decodeCbor $ wrap $ byteArrayFromIntArrayUnsafe
                $ Array.replicate 16 123
            }
        )
      roundtripTest "relay2"
        ( SingleHostName
            { port: Just 8080
            , dnsName: "example.com"
            }
        )
      roundtripTest "relay3"
        ( MultiHostName { dnsName: "example.com" }
        )
    group "Credential" do
      roundtripTest "stake1" stake1
      roundtripTest "stake2" stake2
    group "Certificate" do
      ifor_ certsFixture1 $ (\i cert -> roundtripTest ("certsFixture1 - " <> show i) cert)
    group "Transaction" do
      roundtripTest "txFixture1" txFixture1
      roundtripTest "txFixture2" txFixture2
      roundtripTest "txFixture3" txFixture3
      -- roundtripTest "txFixture4" txFixture4
      roundtripTest "txFixture5" txFixture5
      roundtripTest "txFixture6" txFixture6
      roundtripTest "txFixture7" txFixture7
    group "TransactionBody" do
      roundtripTest "txBodyFixture1" txBodyFixture1
    group "NetworkId" do
      roundtripTest "MainnetId" MainnetId
      roundtripTest "TestnetId" TestnetId
    group "TransactionInput" do
      roundtripTest "txInputFixture1" txInputFixture1
    group "Int" do
      roundtripTest "int1" int1
    group "Mint" do
      roundtripTest "mint1" mint1
      roundtripTest "mint0" mint0
    group "TransactionOutput" do
      roundtripTest "txOutputFixture1" txOutputFixture1
      roundtripTest "txOutputFixture2" txOutputFixture2
      roundtripTest "txOutputFixture3" txOutputFixture3
  group "TransactionWitnessSet" do
    roundtripTest "witnessSetFixture2Value" witnessSetFixture2Value
    roundtripTest "witnessSetFixture3Value" witnessSetFixture3Value

  suiteCSL

newtype PlutusScriptV1 = PlutusScriptV1 PlutusScript

derive instance Generic PlutusScriptV1 _
derive instance Newtype PlutusScriptV1 _
derive instance Eq PlutusScriptV1

instance Show PlutusScriptV1 where
  show = genericShow

instance AsCbor PlutusScriptV1 where
  encodeCbor = PlutusScript.encodeCbor <<< unwrap
  decodeCbor = map wrap <<< flip PlutusScript.decodeCbor PlutusV1

suiteCSL :: TestPlanM (Aff Unit) Unit
suiteCSL = do
  checkCSLHex "plutusScriptFixture1" CSLHex.plutusScriptFixture1_csl_hex $ PlutusScriptV1 plutusScriptFixture1

  checkCSLHex "nativeScriptFixture1" CSLHex.nativeScriptFixture1_csl_hex nativeScriptFixture1
  checkCSLHex "nativeScriptFixture2" CSLHex.nativeScriptFixture2_csl_hex nativeScriptFixture2
  checkCSLHex "nativeScriptFixture3" CSLHex.nativeScriptFixture3_csl_hex nativeScriptFixture3
  checkCSLHex "nativeScriptFixture4" CSLHex.nativeScriptFixture4_csl_hex nativeScriptFixture4
  checkCSLHex "nativeScriptFixture5" CSLHex.nativeScriptFixture5_csl_hex nativeScriptFixture5
  checkCSLHex "nativeScriptFixture6" CSLHex.nativeScriptFixture6_csl_hex nativeScriptFixture6
  checkCSLHex "nativeScriptFixture7" CSLHex.nativeScriptFixture7_csl_hex nativeScriptFixture7

  checkCSLHex "plutusDataFixture1" CSLHex.plutusDataFixture1_csl_hex plutusDataFixture1
  checkCSLHex "plutusDataFixture2" CSLHex.plutusDataFixture2_csl_hex plutusDataFixture2
  checkCSLHex "plutusDataFixture3" CSLHex.plutusDataFixture3_csl_hex plutusDataFixture3
  checkCSLHex "plutusDataFixture4" CSLHex.plutusDataFixture4_csl_hex plutusDataFixture4
  checkCSLHex "plutusDataFixture5" CSLHex.plutusDataFixture5_csl_hex plutusDataFixture5
  checkCSLHex "plutusDataFixture6" CSLHex.plutusDataFixture5_csl_hex plutusDataFixture5
  checkCSLHex "plutusDataFixture7" CSLHex.plutusDataFixture5_csl_hex plutusDataFixture5
  checkCSLHex "plutusDataFixture8" CSLHex.plutusDataFixture5_csl_hex plutusDataFixture5
  -- CDL does not handle maps with duplicated keys.
  -- checkCSLHex "plutusDataFixture9" CSLHex.plutusDataFixture9_csl_hex plutusDataFixture9
  -- checkCSLHex "plutusDataFixture10" CSLHex.plutusDataFixture10_csl_hex plutusDataFixture10
  checkCSLHex "plutusDataFixture11" CSLHex.plutusDataFixture11_csl_hex plutusDataFixture11

  checkCSLHex "PlutusScriptRef plutusScriptFixture1" CSLHex.plutusScriptRef_plutusScriptFixture1_csl_hex
    (PlutusScriptRef plutusScriptFixture1)

  checkCSLHex "PlutusScriptRef plutusScriptFixture2" CSLHex.plutusScriptRef_plutusScriptFixture2_csl_hex
    (PlutusScriptRef plutusScriptFixture2)

  checkCSLHex "PlutusScriptRef plutusScriptFixture3" CSLHex.plutusScriptRef_plutusScriptFixture3_csl_hex
    (PlutusScriptRef plutusScriptFixture3)

  checkCSLHex "auxiliaryDataFixture1" CSLHex.auxiliaryDataFixture1_csl_hex auxiliaryDataFixture1
  checkCSLHex "auxiliaryDataFixture2" CSLHex.auxiliaryDataFixture2_csl_hex auxiliaryDataFixture2

  checkCSLHex "stake1" CSLHex.stake1_csl_hex stake1
  checkCSLHex "stake2" CSLHex.stake2_csl_hex stake2

  checkCSLHex "txFixture1" CSLHex.txFixture1_csl_hex txFixture1
  checkCSLHex "txFixture2" CSLHex.txFixture2_csl_hex txFixture2
  checkCSLHex "txFixture3" CSLHex.txFixture3_csl_hex txFixture3
  checkCSLHex "txFixture4" CSLHex.txFixture4_csl_hex txFixture4
  checkCSLHex "txFixture5" CSLHex.txFixture5_csl_hex txFixture5
  checkCSLHex "txFixture6" CSLHex.txFixture6_csl_hex txFixture6
  checkCSLHex "txFixture7" CSLHex.txFixture7_csl_hex txFixture7

  checkCSLHex "txBodyFixture1" CSLHex.txBodyFixture1_csl_hex txBodyFixture1

  checkCSLHex "MainnetId" CSLHex.mainnetId_csl_hex MainnetId
  checkCSLHex "TestnetId" CSLHex.testnetId_csl_hex TestnetId

  checkCSLHex "txInputFixture1" CSLHex.txInputFixture1_csl_hex txInputFixture1

  checkCSLHex "int1" CSLHex.int1_csl_hex int1

  checkCSLHex "mint1" CSLHex.mint1_csl_hex mint1
  checkCSLHex "mint0" CSLHex.mint0_csl_hex mint0

  checkCSLHex "txOutputFixture1" CSLHex.txOutputFixture1_csl_hex txOutputFixture1
  checkCSLHex "txOutputFixture2" CSLHex.txOutputFixture2_csl_hex txOutputFixture2
  checkCSLHex "txOutputFixture3" CSLHex.txOutputFixture3_csl_hex txOutputFixture3

  checkCSLHex "witnessSetFixture2Value" CSLHex.witnessSetFixture2Value_csl_hex witnessSetFixture2Value
  checkCSLHex "witnessSetFixture3Value" CSLHex.witnessSetFixture3Value_csl_hex witnessSetFixture3Value

{-
roundtripTestBytes "witnessSetFixture1"
  (Proxy :: Proxy TransactionWitnessSet)
  witnessSetFixture1
roundtripTestBytes "witnessSetFixture2"
  (Proxy :: Proxy TransactionWitnessSet)
  witnessSetFixture2
roundtripTestBytes "witnessSetFixture3"
  (Proxy :: Proxy TransactionWitnessSet)
  witnessSetFixture3
roundtripTestBytes "witnessSetFixture4"
  (Proxy :: Proxy TransactionWitnessSet)
  witnessSetFixture4
-}

roundtripTest
  :: forall a
   . Eq a
  => Show a
  => AsCbor a
  => String
  -> a
  -> TestPlanM (Aff Unit) Unit
roundtripTest label a =
  test ("Deserialization is inverse to serialization: " <> label) do
    decodeCbor (encodeCbor a) `shouldEqual` Just a

checkCSLHex
  :: forall a
   . Eq a
  => Show a
  => AsCbor a
  => String
  -> String
  -> a
  -> TestPlanM (Aff Unit) Unit
checkCSLHex label hex a =
  group ("Check with CSL hex: " <> label) do
    test "Can deserialize CSL hex" do
      (decodeCbor =<< wrap <$> hexToByteArray hex) `shouldEqual` Just a
    test "Hex is equal to CSL" do
      (byteArrayToHex $ unwrap $ encodeCbor a) `shouldEqual` hex

roundtripTestBytes
  :: forall a
   . Eq a
  => Show a
  => AsCbor a
  => String
  -> Proxy a
  -> ByteArray
  -> TestPlanM (Aff Unit) Unit
roundtripTestBytes label _ bytes = do
  test ("Serialization is inverse to deserialization: " <> label) do
    (encodeCbor <$> (decodeCbor (wrap bytes) :: Maybe a)) `shouldEqual` Just
      (wrap bytes)

ifor_ :: forall a m. Monad m => Array a -> (Int -> a -> m Unit) -> m Unit
ifor_ arr f = for_ (Array.zip (Array.range 0 (Array.length arr - 1)) arr) \(i /\ e) -> f i e

main :: Effect Unit
main = launchAff_ do
  interpretWithConfig
    defaultConfig { timeout = Just $ Milliseconds 30_000.0, exit = true }
    suite
