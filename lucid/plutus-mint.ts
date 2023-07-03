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
  Utils
} from "https://deno.land/x/lucid@0.10.6/mod.ts";
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";
import { parse } from "https://deno.land/std@0.184.0/flags/mod.ts";

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
console.log(emulator.ledger);

// Using a plutus script doesn't seem to work
const factoryMintingPolicy = { type: "PlutusV2", script: factoryMint };
const factoryMintRedeemer = "d87980"; // MakeFactory
const factoryPolicyId = lucid.utils.mintingPolicyToId(factoryMintingPolicy);

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
    .payToAddress(userAddress, {
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

quit();

// Using a plutus script doesn't seem to work
const poolMintingPolicy = { type: "PlutusV2", script: poolMint };
const poolMintRedeemer = "d87a9fd8799f4040ffd8799f4040ffff";
const poolPolicyId = lucid.utils.mintingPolicyToId(poolMintingPolicy);

async function mintPool(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .mintAssets({
      [toUnit(poolPolicyId, fromText("p"))]: 1n,
    }, poolMintRedeemer)
    .validTo(emulator.now() + 30000)
    .attachMintingPolicy(poolMintingPolicy)
    //.readFrom(factory)
    .payToAddress(userAddress, {
      [toUnit(poolPolicyId, fromText("p"))]: 1n
    })
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}

mintedHash = await mintPool();
okMinted = await emulator.awaitTx(mintedHash);
console.log(`minted pool: ${okMinted}`);
console.log(mintedHash);
