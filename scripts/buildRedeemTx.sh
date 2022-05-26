#!/bin/sh
ALGO=$1
ADDR=$2
SCRIPT_TX_IN=$3
TX_IN=$4

cardano-cli query protocol-parameters --out-file protocol.json --testnet-magic 9

cardano-cli transaction build \
  --babbage-era \
  --tx-in-collateral $TX_IN \
  --tx-in $TX_IN \
  --tx-in $SCRIPT_TX_IN \
  --tx-in-script-file ./${ALGO}Secp256k1.plutus \
  --tx-in-datum-file ./unitDatum.json \
  --tx-in-redeemer-file ./${ALGO}Secp256k1Redeemer.json \
  --change-address $ADDR \
  --testnet-magic 9 \
  --protocol-params-file protocol.json \
  --out-file tx.raw 
