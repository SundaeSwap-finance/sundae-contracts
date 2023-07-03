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
  concat
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
const poolMint = scriptsJson["pool-mint"];
const factoryMint = scriptsJson["factory-mint"];

const dummy = await Lucid.new(undefined, "Custom");

const userPrivateKey = generatePrivateKey();
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

// poolSH and poolCS all 0, no scoopers
const newFactoryDatum = "d8799f58200000000000000000000000000000000000000000000000000000000000000000581c000000000000000000000000000000000000000000000000000000008080ff";

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
    .payToAddressWithData(userAddress, newFactoryDatum, {
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
const factory = (await emulator.getUtxosByOutRef([ref]))[0];
if (!factory) { throw "No factory"; }
console.log(factory);
const factoryChange = (await emulator.getUtxosByOutRef([{
  txHash: bootedHash,
  outputIndex: 1
}]))[0];
if (!factoryChange) { throw "No factory change"; }

// Using a plutus script doesn't seem to work
const poolMintingPolicy = { type: "PlutusV2", script: poolMint };
const poolMintRedeemer = "d87a9fd8799f4040ffd8799f4040ffff";
const poolPolicyId = lucid.utils.mintingPolicyToId(poolMintingPolicy);

//          !newPoolIdent = dropByteString 1 $ blake2b_256 $
//            getTxId (txOutRefId firstInput) <> "#" <> getIdent (intToIdent (txOutRefIdx firstInput))

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
newPoolId = concat(p, newPoolId);
const newPoolIdHex = toHex(newPoolId);
console.log("newPoolId (hex): ", newPoolIdHex);

async function mintPool(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .mintAssets({
      [toUnit(poolPolicyId, newPoolIdHex)]: 1n,
    }, poolMintRedeemer)
    .validTo(emulator.now() + 30000)
    .attachMintingPolicy(poolMintingPolicy)
    .readFrom([factory])
    .payToAddress(userAddress, {
      "lovelace": 2_000_000n,
      [toUnit(poolPolicyId, newPoolIdHex)]: 1n
    })
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}

mintedHash = await mintPool();
okMinted = await emulator.awaitTx(mintedHash);
console.log(`minted pool: ${okMinted}`);
console.log(mintedHash);
