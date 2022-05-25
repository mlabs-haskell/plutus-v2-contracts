#!/bin/sh
ALGO=$1
ADDR=$2
TX_IN=$3

cardano-cli transaction build \
  --babbage-era \
  --tx-in $TX_IN \
  --tx-out $(cat ./${ALGO}Secp256k1.addr)+1000000 \
  --tx-out-datum-hash-file unitDatum.json \
  --change-address $ADDR \
  --testnet-magic 9 \
  --out-file tx.raw 
