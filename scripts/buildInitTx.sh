#!/bin/sh
ALGO=$1
SKEY_PATH=$2

ADDR=$(cat ./ownWallet.addr)

cardano-cli query utxo --address $ADDR --testnet-magic 9 --out-file ownUtxos.json
TX_IN=$(cat ownUtxos.json | jq -r "keys | .[0]")

cardano-cli transaction build \
  --babbage-era \
  --tx-in $TX_IN \
  --tx-out $(cat ./${ALGO}Secp256k1.addr)+1000000 \
  --tx-out-datum-hash-file unitDatum.json \
  --change-address $ADDR \
  --testnet-magic 9 \
  --out-file tx.raw 

if [ ! -z $SKEY_PATH ]; then
  cardano-cli transaction sign --signing-key-file $SKEY_PATH --tx-body-file tx.raw --out-file tx.signed

  cardano-cli transaction submit --tx-file tx.signed --testnet-magic 9
fi
