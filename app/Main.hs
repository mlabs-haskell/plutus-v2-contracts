module Main where

import Cardano.Api
import Plutus.V2.SchnorrValidator (scriptSerial)
import Prelude

main :: IO ()
main = do
  result <- writeFileTextEnvelope "result.plutus" Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
