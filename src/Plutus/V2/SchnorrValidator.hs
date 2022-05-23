{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.V2.SchnorrValidator where

import Cardano.Api.Shelley (PlutusScript (..),  PlutusScriptV2)
import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Plutus.V1.Ledger.Api qualified as Ledger
import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude hiding (($), (.))

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ sig _ =
  let vkey = ""
      msg = ""
   in case Ledger.fromBuiltinData sig of
        Nothing -> PlutusTx.Prelude.error ()
        Just sig' ->
          if Builtins.verifyEcdsaSecp256k1Signature vkey msg sig'
            then ()
            else PlutusTx.Prelude.error ()

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [||mkValidator||])

script :: Plutus.Script
script = Plutus.unValidatorScript validator

scriptShortBs :: SBS.ShortByteString
scriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

scriptSerial :: PlutusScript PlutusScriptV2
scriptSerial = PlutusScriptSerialised scriptShortBs
