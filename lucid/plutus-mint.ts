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

const poolMint = "4601010000228001";

//"584d01010033222222800245209b4e9e9af27b5dfa4d383be20535b94b16f4c0aa00ddc4b1ea4391396518bb500048811c000000000000000000000000000000000000000000000000000000000001";

// Using a plutus script doesn't seem to work
const dummyMintingPolicy = { type: "PlutusV2", script: poolMint };

const dummyPolicyId = lucid.utils.mintingPolicyToId(dummyMintingPolicy);

const poolMintRedeemer = "d87a9fd8799f4040ffd8799f4040ffff";

async function mintDummyTokens(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .mintAssets({
      [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000_000n,
    }, poolMintRedeemer)
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
