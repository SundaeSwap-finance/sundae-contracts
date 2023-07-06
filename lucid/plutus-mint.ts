import {
  Blockfrost,
  Emulator,
  fromText,
  generatePrivateKey,
  getAddressDetails,
  Lucid,
  toUnit,
  TxHash,
  C,
  Utils,
  fromHex,
  toHex,
  concat,
  toPublicKey
} from "https://deno.land/x/lucid@0.10.6/mod.ts";
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";
import { parse } from "https://deno.land/std@0.184.0/flags/mod.ts";

function assert(p) {
  if (!p) {
    throw "Assertion failed!"
  }
}

const flags = parse(Deno.args, {
  string: ["scriptsFile"]
});

const s = await Deno.readTextFile(flags.scriptsFile);
const scriptsJson = JSON.parse(s);
const poolValidator = scriptsJson["pool-validator"];
const factoryValidator = scriptsJson["factory-validator"];
const escrowValidator = scriptsJson["escrow-validator"];
const poolMint = scriptsJson["pool-mint"];
const factoryMint = scriptsJson["factory-mint"];

const dummy = await Lucid.new(undefined, "Custom");

const userPrivateKey = generatePrivateKey();
const userPublicKey = toPublicKey(userPrivateKey);
console.log(userPublicKey);
const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
console.log("userPkh", userPkh.to_hex());
const userAddress = await dummy.selectWalletFromPrivateKey(userPrivateKey).wallet.address();

const accounts =
  [
    {
      address: userAddress,
      assets: {
        lovelace: 1_000_000_000_000n
      }
    }
  ];

let emulator = new Emulator(accounts);

const lucid = await Lucid.new(emulator);

lucid.selectWalletFromPrivateKey(userPrivateKey);
console.log("User address", userAddress);

const poolMintingPolicy = { type: "PlutusV2", script: poolMint };
const poolPolicyId = lucid.utils.mintingPolicyToId(poolMintingPolicy);
console.log("poolPolicyId: ", poolPolicyId);
const poolScript = { type: "PlutusV2", script: poolValidator };
const poolScriptHash = lucid.utils.validatorToScriptHash(poolScript);
console.log("poolScriptHash: ", poolScriptHash);
const escrowScript = { type: "PlutusV2", script: escrowValidator };
const escrowScriptHash = lucid.utils.validatorToScriptHash(escrowScript);
console.log("escrowScriptHash: ", escrowScriptHash);

const factoryAddress = lucid.utils.validatorToAddress({ type: "PlutusV2", script: factoryValidator });
const poolAddress = lucid.utils.validatorToAddress(poolScript);
const escrowAddress = lucid.utils.validatorToAddress(escrowScript);

// Using a native script works
const dummyMintingPolicy = lucid.utils.nativeScriptFromJson({
  type: "all",
  scripts: [],
});
const dummyPolicyId = lucid.utils.mintingPolicyToId(dummyMintingPolicy);

async function mintDummyTokens(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .mintAssets({
      [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000_000n,
    })
    .validTo(emulator.now() + 30000)
    .attachMintingPolicy(dummyMintingPolicy)
    .payToAddress(userAddress, {
      [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000_000n
    })
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}

let mintedHash: TxHash = await mintDummyTokens();
let okMinted = await emulator.awaitTx(mintedHash);
console.log(`minted dummy tokens: ${okMinted}`);
console.log(mintedHash);

emulator.ledger["00000000000000000000000000000000000000000000000000000000000000000"] = {
  utxo: {
    txHash: "0000000000000000000000000000000000000000000000000000000000000000",
    outputIndex: 0,
    assets: {
      "lovelace": 10_000_000n,
    },
    address: userAddress,
  },
  spent: false
};

const factoryMintingPolicy = { type: "PlutusV2", script: factoryMint };
const factoryMintRedeemer = "d87980"; // MakeFactory
const factoryPolicyId = scriptsJson["factory-boot-cs"];
assert(factoryPolicyId == lucid.utils.mintingPolicyToId(factoryMintingPolicy));
console.log(`factoryPolicyId: ${factoryPolicyId}`);

// poolSH = "", poolCS = "", no scoopers
const newFactoryDatum = "d8799f410041008080ff";

async function bootFactory(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .collectFrom([
      emulator.ledger["00000000000000000000000000000000000000000000000000000000000000000"].utxo
    ])
    .mintAssets({
      [toUnit(factoryPolicyId, fromText("factory"))]: 1n
    }, factoryMintRedeemer)
    .validTo(emulator.now() + 30000)
    .attachMintingPolicy(factoryMintingPolicy)
    .payToAddressWithData(factoryAddress, newFactoryDatum, {
      "lovelace": 2_000_000n,
      [toUnit(factoryPolicyId, fromText("factory"))]: 1n
    })
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}

const bootedHash = await bootFactory();
const okBooted = await emulator.awaitTx(bootedHash);
console.log(`booted factory: ${okBooted}`);
console.log(bootedHash);

let ref = { txHash: bootedHash, outputIndex: 0 };
console.log(`get ${ref.txHash}#${ref.outputIndex}`);
const newFactory = (await emulator.getUtxosByOutRef([ref]))[0];

const configuredFactoryDatum =
  "d8799f581c" +
  poolScriptHash +
  "581c" +
  poolPolicyId +
  "8080ff";

const configureFactoryRedeemer = "d87980";

async function configureFactory(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .collectFrom([newFactory], configureFactoryRedeemer)
    .validTo(emulator.now() + 30000)
    .attachSpendingValidator({ type: "PlutusV2", script: factoryValidator })
    .payToAddressWithData(factoryAddress, configuredFactoryDatum, {
      "lovelace": 2_000_000n,
      [toUnit(factoryPolicyId, fromText("factory"))]: 1n
    })
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}

const configuredHash = await configureFactory();
const okConfigured = await emulator.awaitTx(configuredHash);
console.log(`configured factory: ${okConfigured}`);
console.log(configuredHash);

ref = { txHash: configuredHash, outputIndex: 0 };
console.log(`get ${ref.txHash}#${ref.outputIndex}`);
const factory = (await emulator.getUtxosByOutRef([ref]))[0];
if (!factory) { throw "No factory"; }
console.log(factory);
const factoryChange = (await emulator.getUtxosByOutRef([{
  txHash: configuredHash,
  outputIndex: 1
}]))[0];
if (!factoryChange) { throw "No factory change"; }

// !newPoolIdent = dropByteString 1 $ blake2b_256 $
//   getTxId (txOutRefId firstInput) <> "#" <> getIdent (intToIdent (txOutRefIdx firstInput))
const poolInputTxHash = fromHex(factoryChange.txHash);
const numberSign = new Uint8Array([0x23]);
const poolInputTxIx = new Uint8Array([0x01]); // ident encoding for output index 1
let poolInputRef = new Uint8Array([]);
poolInputRef = concat(poolInputRef, poolInputTxHash);
poolInputRef = concat(poolInputRef, numberSign);
poolInputRef = concat(poolInputRef, poolInputTxIx);
console.log("poolInputRef: ", poolInputRef);
let newPoolId = C.hash_blake2b256(poolInputRef).slice(1); // Truncate first byte
const p = new Uint8Array([0x70]); // 'p'
const l = new Uint8Array([0x6c]); // 'l'
const poolNftName = concat(p, newPoolId);
const poolLqName = concat(l, newPoolId);
const poolNftNameHex = toHex(poolNftName);
const poolLqNameHex = toHex(poolLqName);
console.log("poolNftName (hex): ", poolNftNameHex);
console.log("poolLqName (hex): ", poolLqNameHex);

const newPoolDatum =
  "d8799fd8799fd8799f" +
  "40" + // Empty string for ADA
  "40" + // Empty string for ADA
  "ffd8799f" +
  "581c" + dummyPolicyId +
  "45" + fromText("DUMMY") +
  "ffff" +
  "581f" + toHex(newPoolId) +
  "1a3b9aca00d8799f011907d0ff0000ff";

console.log("newPoolDatum: ", newPoolDatum);

const poolMintRedeemer =
  "d87a9fd8799f4040ffd8799f" +
  "581c" + dummyPolicyId +
  "45" + fromText("DUMMY") +
  "ffff";

console.log("poolMintRedeemer: ", poolMintRedeemer);

console.log(emulator.ledger);

async function mintPool(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .mintAssets({
      [toUnit(poolPolicyId, poolNftNameHex)]: 1n,
      [toUnit(poolPolicyId, poolLqNameHex)]: 1_000_000_000n,
    }, poolMintRedeemer)
    .validTo(emulator.now() + 30000)
    .attachMintingPolicy(poolMintingPolicy)
    .readFrom([factory])
    .payToAddressWithData(poolAddress, newPoolDatum, {
      "lovelace": 1_000_000_000n + 2_000_000n,
      [toUnit(poolPolicyId, poolNftNameHex)]: 1n,
      [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000n,
    })
    .payToAddress(userAddress, {
      "lovelace": 2_000_000n,
      [toUnit(poolPolicyId, poolLqNameHex)]: 1_000_000_000n,
    })
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}

mintedHash = await mintPool();
okMinted = await emulator.awaitTx(mintedHash);
console.log(`minted pool: ${okMinted}`);
console.log(mintedHash);

const newEscrowDatum =
  "d8799fd8799fd8799fd8799fd8799f" +
  "581c" + userPkh.to_hex() + // destination pkh
  "ffd87a80ffd87a80ffd87a80ff" +
  "1a002625a0" + // Scooper fee
  "d8799fd8799fd8799f" +
  "40" + // give coin policy id
  "40" + // give coin token name
  "ff" +
  "1a00989680" + // give amount
  "ffd8799fd8799f" +
  "581c" + dummyPolicyId + // take coin policy id
  "45" + fromText("DUMMY") + // take coin token name
  "ff" +
  "d87a80ff" + // market order
  "ffff";

console.log(`newEscrowDatum: ${newEscrowDatum}`);

async function listEscrow(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .validTo(emulator.now() + 30000)
    .payToAddressWithData(escrowAddress, newEscrowDatum, {
      "lovelace": 4_500_000n + 10_000_000n,
    })
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}

let listedHash = await listEscrow();
let okListed = await emulator.awaitTx(listedHash);
console.log(`listed escrow 1: ${okListed}`);
console.log(listedHash);

listedHash = await listEscrow();
okListed = await emulator.awaitTx(listedHash);
console.log(`listed escrow 2: ${okListed}`);
console.log(listedHash);
