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
import { encode, decode } from "https://deno.land/std@0.184.0/encoding/hex.ts";
import { concat } from "https://deno.land/std@0.184.0/bytes/mod.ts";

const flags = parse(Deno.args, {
  string: ["scriptsFile"]
});

type Scripts = {
  factoryValidator: string;
  factoryMint: string;
  poolValidator: string;
  poolMint: string;
  escrowValidator: string;
};

function hexEncodeString(s: string): string {
  const binary: Uint8Array = new TextEncoder().encode(s);
  const hexBytes: Uint8Array = encode(binary);
  const utf8: string = new TextDecoder().decode(hexBytes);
  return utf8;
}

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

const scriptsHex = JSON.parse(scriptsJson);

const dummy = await Lucid.new(undefined, "Custom");

const escrowAddress = dummy.utils.validatorToAddress({
  type: "PlutusV2",
  script: scriptsHex["escrow-validator"]
});

const poolAddress = dummy.utils.validatorToAddress({
  type: "PlutusV2",
  script: scriptsHex["pool-validator"]
});
const factoryAddress = dummy.utils.validatorToAddress({
  type: "PlutusV2",
  script: scriptsHex["factory-validator"]
});

const poolMintingPolicyId = dummy.utils.mintingPolicyToId({
  type: "PlutusV2",
  script: scriptsHex["pool-mint"]
});

const userPrivateKey = generatePrivateKey();
const userAddress = await dummy.selectWalletFromPrivateKey(userPrivateKey).wallet.address();
const scooperPrivateKey = generatePrivateKey();
const scooperAddress = await dummy.selectWalletFromPrivateKey(scooperPrivateKey).wallet.address();

const accounts = 
  [
    {
      address: userAddress,
      assets: {
        lovelace: 1_000_000_000_000n
      }
    },
    {
      address: scooperAddress,
      assets: {
        lovelace: 1_000_000_000_000n
      }
    }
  ];

const escrow1Datum = "d8799fd8799fd8799fd8799fd8799f40ffd87a80ffd87a80ffd87a80ff1a000f4240d8799fd8799fd8799f4040ff1a000f4240ffd8799fd8799f416943464f4fffd87a80ffffff";
const escrow1DatumHash = C.hash_blake2b256(escrow1Datum);

const escrow2Datum = escrow1Datum;
const escrow2DatumHash = escrow1DatumHash;

let datumTable = {};
datumTable[escrow1DatumHash] = escrow1Datum;
datumTable[escrow2DatumHash] = escrow2Datum;

let emulator = new Emulator(accounts);
//emulator.datumTable = datumTable;

const lucid = await Lucid.new(emulator);

lucid.selectWalletFromPrivateKey(userPrivateKey);
console.log("User address", userAddress);

const initialFactoryDatum = "00";

async function createFactory(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .payToContract(
      factoryAddress,
      initialFactoryDatum,
      {
        lovelace: 2_000_000n,
      }
    )
    .validTo(emulator.now() + 30000)
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}
const createFactoryHash = await createFactory();
const ok = await emulator.awaitTx(createFactoryHash);
console.log(ok);

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
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}
const mintedHash: TxHash = await mintDummyTokens();
const okMinted = await emulator.awaitTx(mintedHash);
console.log(okMinted);
console.log(emulator.ledger);
console.log(mintedHash);

function makePoolIdent(txHash: TxHash, txout: int): Uint8Array {
  if (txout < 0 || txout >= 256) { throw "Bad txout"; }
  const txHashRaw: Uint8Array = decode(new TextEncoder().encode(txHash));
  let dest: Uint8Array = new Uint8Array(34);
  dest.set(txHashRaw, 0);
  dest.set([0x23, txout], 32);
  return C.hash_blake2b256(dest).slice(1);
}

const newPoolIdent = makePoolIdent(mintedHash, 1);
const newPoolIdentHex = new TextDecoder().decode(encode(newPoolIdent));
const newPoolNftTokenName = "70" + newPoolIdentHex; // 0x70 is 'p'

console.log("dummyPolicyId:       " + dummyPolicyId);
console.log("newPoolIdent:        " + newPoolIdentHex);
console.log("newPoolNftTokenName: " + newPoolNftTokenName);

function encodeIntegerBigEndian32(n: BigInt): Uint8Array {
  let output = new Uint8Array(4);
  if (n >= 2n ** 32n) { throw "encodeIntegerBigEndian32: too big"; }
  let ix = 3;
  while(ix >= 0) {
    output.set(n % 256n, ix);
    n /= 256n;
    ix--;
  }
  return output;
}

function encodeIntegerBigEndian64(n: BigInt): Uint8Array {
  let output = new Uint8Array(8);
  if (n >= 2n ** 64n) { throw "encodeIntegerBigEndian64: too big"; }
  let ix = 7;
  while(ix >= 0) {
    output.set([Number(n % 256n)], ix);
    n = n / 256n;
    ix--;
  }
  return output;
}

function mkPoolDatum(coinA: string, coinB: string, liq: BigInt, ident: string): Uint8Array {
  let output = new Uint8Array();
  const hsConstructor = new Uint8Array([0xd8, 0x79, 0x9f]);
  output = concat(output, hsConstructor); // Begin pool datum record 
  
  output = concat(output, hsConstructor); // Begin coin pair record
  output = concat(output, hsConstructor); // Begin AssetClass
  const emptyString = new Uint8Array([0x40]);
  output = concat(output, emptyString); // Currency symbol ""
  output = concat(output, emptyString); // Token name ""
  const breakArray = new Uint8Array([0xff]);
  output = concat(output, breakArray);  // End AssetClass
  output = concat(output, hsConstructor); // Begin AssetClass
  output = concat(output, emptyString); // Currency symbol ""
  output = concat(output, emptyString); // Currency symbol ""
  output = concat(output, breakArray);  // End AssetClass
  output = concat(output, breakArray);  // End coin pair

  const identBytes: Uint8Array = decode(new TextEncoder().encode(ident));
  if (identBytes.byteLength != 31) {
    throw(`identBytes not equal to 31: ${identBytes.byteLength}`);
  }
  output = concat(output, new Uint8Array([0x58, 0x1f])); // 0x1f-byte pool id follows
  output = concat(output, identBytes); // pool id

  // Initial liquidity
  if (liq >= 2n ** 32n && liq < 2n ** 64n) {
    output = concat(output, new Uint8Array([0x1b]));
    output = concat(output, encodeIntegerBigEndian64(liq));
  } else {
    throw("liq is not within the expected range (unimplemented)")
  }

  output = concat(output, hsConstructor); // Begin swap fees record
  output = concat(output, new Uint8Array([0x01, 0x18, 0xc8])); // Hard coded 1/200
  output = concat(output, breakArray); // End swap fees record

  output = concat(output, new Uint8Array([0x00])); // Market open time
  output = concat(output, new Uint8Array([0x00])); // Locked Rewards 

  output = concat(output, breakArray); // End pool datum record
  return output;
}


const initialPoolDatum = mkPoolDatum(
  "",
  "",
  1_000_000_000n * 1_000_000_000n,
  newPoolIdentHex
);
const initialPoolDatumHex = new TextDecoder().decode(encode(initialPoolDatum));

console.log("initialPoolDatum (hex): ", initialPoolDatumHex);

console.log("poolMintingPolicyId: ", poolMintingPolicyId);
console.log("scriptsHex: ", scriptsHex);

async function createPool(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .payToContract(
      poolAddress,
      initialPoolDatumHex,
      {
        lovelace: 1_000_000_000n,
        [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000n,
        [toUnit(poolMintingPolicyId, newPoolNftTokenName)]: 1n
      })
    .attachMintingPolicy({ type: "PlutusV2", script: scriptsHex["pool-mint"] })
    .mintAssets({ [toUnit(poolMintingPolicyId, newPoolNftTokenName)]: 1n })
    .validTo(emulator.now() + 30000)
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}

await createPool();
