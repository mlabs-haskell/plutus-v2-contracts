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

skeyA :: forall (a :: Type). (DSIGNAlgorithm a) => SignKeyDSIGN a
skeyA = genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 123

skeyB :: SECP.SecKey
-- skeyB = fromMaybe (error undefined) $ SECP.secKey $ ByteString.replicate 32 123
skeyB =
  "f46bf49093d585f2ea781a0bf6d83468919f547999ad91c9210256979d88eef1"

vkeyA :: forall (a :: Type). (DSIGNAlgorithm a) => VerKeyDSIGN a
vkeyA = deriveVerKeyDSIGN skeyA

vkeyB :: SECP.PubKey
vkeyB = SECP.derivePubKey skeyB

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

      ecdsaSigA :: SigDSIGN EcdsaSecp256k1DSIGN
      ecdsaSigA = signDSIGN () ecdsaMsg skeyA

      ecdsaSigB = SECP.signMsg skeyB ecdsaMsg

      vkeyA' = rawSerialiseVerKeyDSIGN @EcdsaSecp256k1DSIGN vkeyA
      vkeyB' = serialisePubKey False vkeyB

      msg = Builtins.toBuiltin hashedMsg

      sigA = rawSerialiseSigDSIGN ecdsaSigA
      sigB = SECP.getCompactSig $ SECP.exportCompactSig ecdsaSigB

      sigA' = Builtins.toBuiltin sigA
      sigB' = Builtins.toBuiltin sigB

  putStrLn "ECDSA secp256k1"
  putStrLn $ "Hashed msg: " ++ (Char8.unpack (Base16.encode hashedMsg))
  putStrLn $ "Verification key (serialised with Cardano):\n" ++ Char8.unpack (Base16.encode vkeyA')
  putStrLn $ "Verification key (serialised with secp256k1-haskell):\n" ++ Char8.unpack (Base16.encode vkeyB')
  putStrLn $ "Signature (serialised with Cardano):\n" ++ Char8.unpack (Base16.encode sigA)
  putStrLn $ "Signature (serialised with secp256k1-haskell):\n" ++ Char8.unpack (Base16.encode sigB)

  _ <- writeRedeemer "scripts/ecdsaSecp256k1Redeemer.json" (PlutusTx.toBuiltinData (msg, sigA'))
  result <-
    writeFileTextEnvelope
      "scripts/ecdsaSecp256k1.plutus"
      Nothing
      (EcdsaSecp256k1Validator.scriptSerial (Builtins.toBuiltin vkeyA'))
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

  print $ Builtins.verifyEcdsaSecp256k1Signature (Builtins.toBuiltin vkeyA') msg sigA'
  print $ Builtins.verifyEcdsaSecp256k1Signature (Builtins.toBuiltin vkeyB') msg sigB'

writeSchnorrSecp256k1Script :: ByteString -> IO ()
writeSchnorrSecp256k1Script rawMsg = do
  let schnorrSig :: SigDSIGN SchnorrSecp256k1DSIGN
      schnorrSig = signDSIGN () rawMsg skeyA

      vkey' = rawSerialiseVerKeyDSIGN @SchnorrSecp256k1DSIGN vkeyA
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
      (SchnorrSecp256k1Validator.scriptSerial (Builtins.toBuiltin vkey'))
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

  print $ Builtins.verifySchnorrSecp256k1Signature (Builtins.toBuiltin vkey') msg sig'

writeRedeemer :: forall (a :: Type). ToData a => FilePath -> a -> IO ((Either (FileError ()) ()))
writeRedeemer path =
  writeFileJSON path
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData

{- | Serialise the public key using secp256k1-haskell directly
 The first byte of the result is a flag to denote if the format is compressed or not
-}
serialisePubKey :: Bool -> SECP.PubKey -> ByteString
serialisePubKey removeFlag =
  if removeFlag
    then
      snd
        . fromMaybe undefined
        . ByteString.uncons
        . SECP.exportPubKey False
    else SECP.exportPubKey False
