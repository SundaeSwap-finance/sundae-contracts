import {
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
import { Buffer } from "https://deno.land/std@0.184.0/io/buffer.ts";
import { decode } from "https://deno.land/std@0.184.0/encoding/hex.ts";

const flags = parse(Deno.args, {
  string: ["scriptsFile"]
});

/*
const privateKey = generatePrivateKey();
const address = await (await Lucid.new(undefined, "Custom"))
  .selectWalletFromPrivateKey(privateKey).wallet.address();
const { paymentCredential } = getAddressDetails(address);
const emulator = new Emulator([{ address, assets: { lovelace: 3000000000n } }]);
const lucid = await Lucid.new(emulator);
lucid.selectWalletFromPrivateKey(privateKey);
const mintingPolicy = lucid.utils.nativeScriptFromJson({
  type: "all",
  scripts: [
    {
      type: "before",
      slot: lucid.utils.unixTimeToSlot(emulator.now() + 60000),
    },
    { type: "sig", keyHash: paymentCredential?.hash! },
  ],
});
const policyId = lucid.utils.mintingPolicyToId(mintingPolicy);
async function mint(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .mintAssets({
      [toUnit(policyId, fromText("Wow"))]: 123n,
    })
    .validTo(emulator.now() + 30000)
    .attachMintingPolicy(mintingPolicy)
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}
await mint();
emulator.awaitBlock(4);
console.log(privateKey);
console.log(policyId);
console.log(await lucid.wallet.getUtxos());
*/

type Scripts = {
  factoryValidator: string;
  factoryMint: string;
  poolValidator: string;
  poolMint: string;
  escrowValidator: string;
};

function parseScripts(o: string): Scripts {
  const obj = JSON.parse(o);
  const parsePlutusScript = function(s: string) {
    const binary: Uint8Array = new TextEncoder().encode(s);
    const rawBytes: Uint8Array = decode(binary);
    return C.PlutusScript.from_bytes(rawBytes);
  };
  return {
    factoryValidator: parsePlutusScript(obj["factory-validator"]),
    factoryMint: parsePlutusScript(obj["factory-mint"]),
    poolValidator: parsePlutusScript(obj["pool-validator"]),
    poolMint: parsePlutusScript(obj["pool-mint"]),
    escrowValidator: parsePlutusScript(obj["escrow-validator"])
  };
}

const scriptsJson: string = await Deno.readTextFile(flags.scriptsFile);
const scripts: Scripts = parseScripts(scriptsJson);
console.log(scripts);

const lucid = await Lucid.new(undefined, "Custom");

console.log(scripts.escrowValidator.to_bytes())

const escrowAddress = lucid.utils.validatorToAddress({
  type: "PlutusV2",
  script: scriptsJson["escrow-validator"]
});

const accounts = 
  [
    { 
      address: escrowAddress,
      assets: {
        lovelace: 1_000_000n
      }
    },
    {
      address: escrowAddress,
      assets: {
        lovelace: 1_000_000n
      }
    }
  ];

let datumTable = {};
datumTable[escrow1DatumHash] = escrow1Datum;
datumTable[escrow2DatumHash] = escrow2Datum;

let emulator = new Emulator(accounts);
emulator.datumTable = datumTable;

/*
try {
  await mint();
} catch (_e) {
  console.error(
    "Error: Second transaction failed because upper bound exceeded in mint script.",
  );
}
*/
