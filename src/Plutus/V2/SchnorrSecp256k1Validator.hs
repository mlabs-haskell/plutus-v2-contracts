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
  BuiltinByteString ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkValidator vkey msg _ sig _ =
  case Ledger.fromBuiltinData sig of
    Nothing -> PlutusTx.Prelude.error ()
    Just sig' ->
      if Builtins.verifySchnorrSecp256k1Signature vkey msg sig'
        then ()
        else PlutusTx.Prelude.error ()

validator :: BuiltinByteString -> BuiltinByteString -> Plutus.Validator
validator vkey msg =
  Plutus.mkValidatorScript $
    $$(PlutusTx.compile [||mkValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode vkey
      `PlutusTx.applyCode` PlutusTx.liftCode msg

script :: BuiltinByteString -> BuiltinByteString -> Plutus.Script
script vkey msg = Plutus.unValidatorScript $ validator vkey msg

scriptShortBs :: BuiltinByteString -> BuiltinByteString -> SBS.ShortByteString
scriptShortBs vkey msg = SBS.toShort $ LBS.toStrict $ serialise $ script vkey msg

scriptSerial :: BuiltinByteString -> BuiltinByteString -> PlutusScript PlutusScriptV2
scriptSerial vkey msg = PlutusScriptSerialised $ scriptShortBs vkey msg
