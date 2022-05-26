module Main where

import Cardano.Api (Error (displayError), FileError, ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), scriptDataToJson, writeFileJSON, writeFileTextEnvelope)
import Cardano.Api.Shelley (fromPlutusData)
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
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Hash (blake2b_256)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Plutus.V2.EcdsaSecp256k1Validator qualified as EcdsaSecp256k1Validator
import Plutus.V2.Ledger.Api (ToData)
import Plutus.V2.SchnorrSecp256k1Validator qualified as SchnorrSecp256k1Validator
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import Prelude

skey :: forall (a :: Type). (DSIGNAlgorithm a) => SignKeyDSIGN a
skey = genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 123

vkey :: forall (a :: Type). (DSIGNAlgorithm a) => VerKeyDSIGN a
vkey = deriveVerKeyDSIGN skey

main :: IO ()
main = do
  putStrLn "Insert some message"
  rawMsg <- Char8.pack <$> getLine

  writeEcdsaSecp256k1Script rawMsg
  writeSchnorrSecp256k1Script rawMsg

writeEcdsaSecp256k1Script :: ByteString -> IO ()
writeEcdsaSecp256k1Script rawMsg = do
  -- ECDSA SECP256k1 only accepts a 32 byte hash of a message
  let hashedMsg = blake2b_256 rawMsg
      ecdsaMsg = fromMaybe undefined $ SECP.msg hashedMsg

      ecdsaSig :: SigDSIGN EcdsaSecp256k1DSIGN
      ecdsaSig = signDSIGN () ecdsaMsg skey

      vkey' = rawSerialiseVerKeyDSIGN @EcdsaSecp256k1DSIGN vkey
      (vkey1, vkey2) = bimap Builtins.toBuiltin Builtins.toBuiltin $ ByteString.splitAt 32 vkey'
      msg = Builtins.toBuiltin hashedMsg

      sig = rawSerialiseSigDSIGN ecdsaSig
      sig' = Builtins.toBuiltin sig

  putStrLn "ECDSA secp256k1"
  putStrLn $ "Hashed msg: " ++ (Char8.unpack (Base16.encode hashedMsg))
  putStrLn $ "Verification key: " ++ Char8.unpack (Base16.encode vkey')
  putStrLn $ "Signature: " ++ Char8.unpack (Base16.encode sig)

  _ <- writeRedeemer "scripts/ecdsaSecp256k1Redeemer.json" (PlutusTx.toBuiltinData (msg, sig'))
  result <-
    writeFileTextEnvelope
      "scripts/ecdsaSecp256k1.plutus"
      Nothing
      (EcdsaSecp256k1Validator.scriptSerial (vkey1, vkey2))
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

  print $ Builtins.verifyEcdsaSecp256k1Signature (Builtins.appendByteString vkey1 vkey2) msg sig'

writeSchnorrSecp256k1Script :: ByteString -> IO ()
writeSchnorrSecp256k1Script rawMsg = do
  let schnorrSig :: SigDSIGN SchnorrSecp256k1DSIGN
      schnorrSig = signDSIGN () rawMsg skey

      vkey' = rawSerialiseVerKeyDSIGN @SchnorrSecp256k1DSIGN vkey
      (vkey1, vkey2) = bimap Builtins.toBuiltin Builtins.toBuiltin $ ByteString.splitAt 32 vkey'
      msg = Builtins.toBuiltin rawMsg

      sig = rawSerialiseSigDSIGN schnorrSig
      sig' = Builtins.toBuiltin sig

  putStrLn "Schnorr secp256k1"
  putStrLn $ "Verification key: " ++ Char8.unpack (Base16.encode vkey')
  putStrLn $ "Signature: " ++ Char8.unpack (Base16.encode sig)

  _ <- writeRedeemer "scripts/schnorrSecp256k1Redeemer.json" (PlutusTx.toBuiltinData (msg, sig'))
  result <-
    writeFileTextEnvelope
      "scripts/schnorrSecp256k1.plutus"
      Nothing
      (SchnorrSecp256k1Validator.scriptSerial (vkey1, vkey2))
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

  print $ Builtins.verifySchnorrSecp256k1Signature (Builtins.appendByteString vkey1 vkey2) msg sig'

writeRedeemer :: forall (a :: Type). ToData a => FilePath -> a -> IO ((Either (FileError ()) ()))
writeRedeemer path =
  writeFileJSON path
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
