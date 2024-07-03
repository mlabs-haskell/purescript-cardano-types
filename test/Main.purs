module Test.Main where

import Prelude

import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Types (TransactionWitnessSet)
import Data.ByteArray (ByteArray)
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Mote (group, test)
import Mote.TestPlanM (TestPlanM, interpretWithConfig)
import Test.Fixtures
  ( int1
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
  , plutusDataFixture2
  , plutusDataFixture3
  , plutusDataFixture4
  , plutusDataFixture5
  , plutusDataFixture6
  , plutusDataFixture7
  , plutusDataFixture8
  , plutusDataFixture9
  , plutusScriptFixture1
  , txFixture1
  , txFixture2
  , txFixture3
  , txFixture4
  , txFixture5
  , txFixture6
  , txFixture7
  , txInputFixture1
  , txOutputFixture1
  , witnessSetFixture1
  , witnessSetFixture2
  , witnessSetFixture2Value
  , witnessSetFixture3
  , witnessSetFixture3Value
  , witnessSetFixture4
  )
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (defaultConfig)
import Type.Proxy (Proxy(Proxy))

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "deserialization and serialization roundtrip" $ do
    group "PlutusScript" do
      roundtripTest "plutusScriptFixture1" plutusScriptFixture1
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
      roundtripTest "plutusDataFixture9" plutusDataFixture9
    group "Transaction" do
      roundtripTest "txFixture1" txFixture1
      roundtripTest "txFixture2" txFixture2
      roundtripTest "txFixture3" txFixture3
      roundtripTest "txFixture4" txFixture4
      roundtripTest "txFixture5" txFixture5
      roundtripTest "txFixture6" txFixture6
      roundtripTest "txFixture6" txFixture7
    group "TransactionInput" do
      roundtripTest "txInputFixture1" txInputFixture1
    group "Int" do
      roundtripTest "int0" int1
    group "Mint" do
      roundtripTest "mint1" mint1
      roundtripTest "mint0" mint0
    group "TransactionOutput" do
      roundtripTest "txOutputFixture1" txOutputFixture1
    group "TransactionWitnessSet" do
      roundtripTest "witnessSetFixture2Value" witnessSetFixture2Value
      roundtripTest "witnessSetFixture3Value" witnessSetFixture3Value
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

main :: Effect Unit
main = launchAff_ do
  interpretWithConfig
    defaultConfig { timeout = Just $ Milliseconds 30_000.0, exit = true }
    suite
