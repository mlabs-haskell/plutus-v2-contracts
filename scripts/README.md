Building a lock and unlock transaction:

1. Run `init.sh`
2. Input a message to sign. This will generate a signature and print it to the screen, it also outputs the correct redeemers in json format
3. Get some utxos for the transaction `cardano-cli query utxo --address $ADDR --testnet-magic 9`
4. Build the lock tx by running `./buildInitTx.sh ecdsa $ADDR $TX_IN`
5. Sign and submit (`cardano-cli transaction sign --tx-body-file tx.raw --signing-key-file $SKEY-PATH --out-file tx.signed && cardano-cli transaction submit --tx-file tx.signed --testnet-magic 9`)
6. Wait for the next block
7. Build the unlock tx by running `./buildRedeemTx.sh ecdsa $ADDR $SCRIPT_TX_IN $TX_IN`
8. Sign and submit
9. Rejoice
