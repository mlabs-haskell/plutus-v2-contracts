{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.V2.SchnorrSecp256k1Validator where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Plutus.V1.Ledger.Api qualified as Ledger
import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude hiding (($), (.))

{-# INLINEABLE mkValidator #-}
mkValidator ::
  BuiltinByteString ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkValidator vkey _ red _ =
  case Ledger.fromBuiltinData red of
    Nothing -> PlutusTx.Prelude.error ()
    Just (msg, sig) ->
      if Builtins.verifySchnorrSecp256k1Signature vkey msg sig
        then ()
        else PlutusTx.Prelude.error ()

validator :: BuiltinByteString -> Plutus.Validator
validator vkey =
  Plutus.mkValidatorScript $
    $$(PlutusTx.compile [||mkValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode vkey

script :: BuiltinByteString -> Plutus.Script
script = Plutus.unValidatorScript . validator

scriptShortBs :: BuiltinByteString -> SBS.ShortByteString
scriptShortBs = SBS.toShort . LBS.toStrict . serialise . script

scriptSerial :: BuiltinByteString -> PlutusScript PlutusScriptV2
scriptSerial = PlutusScriptSerialised . scriptShortBs
