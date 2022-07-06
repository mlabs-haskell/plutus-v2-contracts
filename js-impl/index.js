const { randomBytes } = require("crypto");
const secp256k1 = require("secp256k1");
// or require('secp256k1/elliptic')
//   if you want to use pure js implementation in node

// generate message to sign
// message should have 32-byte length, if you have some other length you can hash message
// for example `msg = sha256(rawMessage)`
const msg = Buffer.alloc(
  32,
  "16e0bf1f85594a11e75030981c0b670370b3ad83a43f49ae58a2fd6f6513cde9",
  "hex"
);

// generate privKey
const privKey = Buffer.alloc(
  32,
  "f46bf49093d585f2ea781a0bf6d83468919f547999ad91c9210256979d88eef1",
  "hex"
);
// let privKey;
// do {
//   privKey = randomBytes(32);

// } while (!secp256k1.privateKeyVerify(privKey));

console.log("Node JS implementation:");

// get the public key in a compressed format
const pubKey = secp256k1.publicKeyCreate(privKey, false);
console.log("Verification key: ", Buffer.from(pubKey).toString("hex"));

// sign the message
const sigObj = secp256k1.ecdsaSign(msg, privKey);
console.log("Signature: ", Buffer.from(sigObj.signature).toString("hex"));

// verify the signature
console.log(secp256k1.ecdsaVerify(sigObj.signature, msg, pubKey));
// => true
