Building a lock and unlock transaction:

1. Run `init.sh $VKEY_PATH $MSG` (VKEY_PATH: path of the verification key, MSG: message string to be signed)
2. Build and submit lock tx by running `./buildInitTx.sh ecdsa $SKEY_PATH `
3. Wait for the next block
4. Build the unlock tx by running `./buildRedeemTx.sh ecdsa $SKEY_PATH`
