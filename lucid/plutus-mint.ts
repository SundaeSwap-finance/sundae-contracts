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
/*
const dummyMintingPolicy = lucid.utils.nativeScriptFromJson({
  type: "all",
  scripts: [],
});
*/

const s = await Deno.readTextFile(flags.scriptsFile);
const scriptsJson = JSON.parse(s);
const poolMint = scriptsJson["pool-mint"];

// Using a plutus script doesn't seem to work
const dummyMintingPolicy = { type: "PlutusV1", script: poolMint };

const dummyPolicyId = lucid.utils.mintingPolicyToId(dummyMintingPolicy);

const poolMintRedeemer = "d87a9fd8799f4040ffd8799f4040ffff";

async function mintDummyTokens(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .mintAssets({
      [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000_000n,
    }, poolMintRedeemer)
    .validTo(emulator.now() + 30000)
    .attachMintingPolicy(dummyMintingPolicy)
    .payToAddress(userAddress, {
      [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000_000n
    })
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}
const mintedHash: TxHash = await mintDummyTokens();
const okMinted = await emulator.awaitTx(mintedHash);
console.log(okMinted);
console.log(emulator.ledger);
console.log(mintedHash);
