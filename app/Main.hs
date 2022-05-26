module Main where

import Cardano.Api (Error (displayError), writeFileTextEnvelope)
import Cardano.Crypto.DSIGN.Class (
  DSIGNAlgorithm,
  SignKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.DSIGN.EcdsaSecp256k1 (
  EcdsaSecp256k1DSIGN,
  SigDSIGN,
  VerKeyDSIGN,
 )
import Cardano.Crypto.DSIGN.SchnorrSecp256k1 (SchnorrSecp256k1DSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Crypto.Secp256k1 qualified as SECP
import Data.Bifunctor (bimap)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import Data.Either (fromRight)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Plutus.V2.EcdsaSecp256k1Validator qualified as EcdsaSecp256k1Validator
import Plutus.V2.SchnorrSecp256k1Validator qualified as SchnorrSecp256k1Validator
import PlutusTx.Builtins qualified as Builtins
import Prelude

skey :: forall (a :: Type). (DSIGNAlgorithm a) => SignKeyDSIGN a
skey = genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 123

vkey :: forall (a :: Type). (DSIGNAlgorithm a) => VerKeyDSIGN a
vkey = deriveVerKeyDSIGN skey

rawMsg :: ByteString.ByteString
rawMsg =
  fromRight undefined $
    Base16.decode
      "4b688df40bcedbe641ddb16ff0a1842d9c67ea1c3bf63f3e0471baa664531d1a"

ecdsaMsg :: SECP.Msg
ecdsaMsg =
  fromMaybe undefined $ SECP.msg rawMsg

ecdsaSig :: SigDSIGN EcdsaSecp256k1DSIGN
ecdsaSig = signDSIGN () ecdsaMsg skey

schnorrSig :: SigDSIGN SchnorrSecp256k1DSIGN
schnorrSig = signDSIGN () rawMsg skey

main :: IO ()
main = do
  writeEcdsaSecp256k1Script
  writeSchnorrSecp256k1Script

writeEcdsaSecp256k1Script :: IO ()
writeEcdsaSecp256k1Script = do
  let vkey' = rawSerialiseVerKeyDSIGN @EcdsaSecp256k1DSIGN vkey
      (vkey1, vkey2) = bimap Builtins.toBuiltin Builtins.toBuiltin $ ByteString.splitAt 32 vkey'
      msg = Builtins.toBuiltin rawMsg

      sig = rawSerialiseSigDSIGN ecdsaSig

  putStrLn "ECDSA secp256k1"
  putStrLn $ "Msg: " ++ Char8.unpack (Base16.encode rawMsg)
  putStrLn $ "Verification key: " ++ Char8.unpack (Base16.encode vkey')
  putStrLn $ "Signature: " ++ Char8.unpack (Base16.encode sig)

  result <- writeFileTextEnvelope "ecdsaSecp256k1.plutus" Nothing (EcdsaSecp256k1Validator.scriptSerial (vkey1, vkey2) msg)
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

  print $ Builtins.verifyEcdsaSecp256k1Signature (Builtins.appendByteString vkey1 vkey2) msg (Builtins.toBuiltin sig)

writeSchnorrSecp256k1Script :: IO ()
writeSchnorrSecp256k1Script = do
  let vkey' = rawSerialiseVerKeyDSIGN @SchnorrSecp256k1DSIGN vkey
      (vkey1, vkey2) = bimap Builtins.toBuiltin Builtins.toBuiltin $ ByteString.splitAt 32 vkey'
      msg = Builtins.toBuiltin rawMsg

      sig = rawSerialiseSigDSIGN schnorrSig

  putStrLn "Schnorr secp256k1"
  putStrLn $ "Msg: " ++ Char8.unpack (Base16.encode rawMsg)
  putStrLn $ "Verification key: " ++ Char8.unpack (Base16.encode vkey')
  putStrLn $ "Signature: " ++ Char8.unpack (Base16.encode sig)

  result <- writeFileTextEnvelope "schnorrSecp256k1.plutus" Nothing (SchnorrSecp256k1Validator.scriptSerial (vkey1, vkey2) msg)
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

  print $ Builtins.verifySchnorrSecp256k1Signature (Builtins.appendByteString vkey1 vkey2) msg (Builtins.toBuiltin sig)
