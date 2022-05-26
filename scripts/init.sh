#!/bin/sh
cd .. && cabal run mk-plutus-v2-contracts && cd scripts

cardano-cli address build --payment-script-file ./ecdsaSecp256k1.plutus --testnet-magic 9 > ./ecdsaSecp256k1.addr
cardano-cli address build --payment-script-file ./schnorrSecp256k1.plutus --testnet-magic 9 > ./schnorrSecp256k1.addr
