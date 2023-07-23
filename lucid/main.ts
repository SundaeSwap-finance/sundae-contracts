import {
  Blockfrost,
  Data,
  Constr,
  Script,
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
  toPublicKey,
  UTxO,
  PROTOCOL_PARAMETERS_DEFAULT
} from "../../lucid/mod.ts";
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";
import { parse } from "https://deno.land/std@0.184.0/flags/mod.ts";
import { ABL, Coin, SwapFees, doSwap } from "./cpp.ts";
import { Datum } from "../../lucid/src/core/libs/cardano_multiplatform_lib/cardano_multiplatform_lib.generated.js";

const verbose = true;

function log(...args) {
  if (verbose) {
    console.log(...args);
  }
}

function assert(p: boolean) {
  if (!p) {
    throw "Assertion failed!"
  }
}

const flags = parse(Deno.args, {
  string: ["scriptsFile"],
  boolean: ["aiken"],
});

if (flags.scriptsFile == undefined) {
  throw "no scripts file";
}

let s = await Deno.readTextFile(flags.scriptsFile);
let scriptsJson = JSON.parse(s);

let poolValidator;
let factoryValidator;
let escrowValidator;
let steakValidator;
let poolMint;
let factoryMint;

if (flags.aiken) {
  console.log("Benchmarking Aiken contracts");
  let validator = scriptsJson["validators"];
  for (let v of validator) {
    if (v.title == "order.spend") escrowValidator = v.compiledCode;
    if (v.title == "settings.spend") factoryValidator = v.compiledCode;
    if (v.title == "pool.spend") poolValidator = v.compiledCode;
    if (v.title == "stake.stake") steakValidator = v.compiledCode;
    if (v.title == "pool.mint") poolMint = v.compiledCode;
    if (v.title == "settings.mint") factoryMint = v.compiledCode;
  }
} else {
  console.log("Benchmarking Plutus contracts");
  let poolValidator = scriptsJson["pool-validator"];
  let factoryValidator = scriptsJson["factory-validator"];
  let escrowValidator = scriptsJson["escrow-validator"];
  let steakValidator = scriptsJson["steak-validator"];
  let poolMint = scriptsJson["pool-mint"];
  let factoryMint = scriptsJson["factory-mint"];
}

const dummy = await Lucid.new(undefined, "Custom");

const userPrivateKey = "ed25519_sk1zxsfsl8ehspny4750jeydt5she7dzstrj7za5vgxl6929kr9d33quqkgp3";
//generatePrivateKey();
const userPublicKey = toPublicKey(userPrivateKey);
log("userPublicKey", userPublicKey);
const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
log("userPkh", userPkh.to_hex());
const userAddress = await dummy.selectWalletFromPrivateKey(userPrivateKey).wallet.address();
log("userAddress", userAddress);

function factoryDatum(poolHash: string, userPkh: string): string {
  return "d879" +
    "9f" +
      "581c" +
        poolHash +
      "d879" +
        "9f" +
          "581c" +
            userPkh +
        "ff" +
      "d879" +
        "9f" +
          "581c" +
            userPkh +
        "ff" +
      "9f" + 
        "581c" +
          userPkh +
      "ff" +
      "9f" +
        "581c" +
          userPkh +
      "ff" +
    "ff"
}

for (let escrowsCount = 1n; escrowsCount <= 20n; escrowsCount++) {
  const accounts =
    [
      {
        address: userAddress,
        assets: {
          lovelace: 1_000_000_000_000n
        }
      }
    ];

  let emulator = new Emulator(accounts, {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxTxSize: 999999999999,
  });

  const lucid = await Lucid.new(emulator);

  lucid.selectWalletFromPrivateKey(userPrivateKey);

  const poolMintingPolicy: Script = { type: "PlutusV2", script: poolMint };
  const poolPolicyId = lucid.utils.mintingPolicyToId(poolMintingPolicy);
  log("poolPolicyId: ", poolPolicyId);
  const poolScript: Script = { type: "PlutusV2", script: poolValidator };
  const poolScriptHash = lucid.utils.validatorToScriptHash(poolScript);
  log("poolScriptHash: ", poolScriptHash);
  const escrowScript: Script = { type: "PlutusV2", script: escrowValidator };
  const escrowScriptHash = lucid.utils.validatorToScriptHash(escrowScript);
  log("escrowScriptHash: ", escrowScriptHash);
  const steakScript: Script = { type: "PlutusV2", script: steakValidator };
  const steakScriptHash = lucid.utils.validatorToScriptHash(steakScript);

  const factoryScript: Script = { type: "PlutusV2", script: factoryValidator };
  const factoryAddress = lucid.utils.validatorToAddress(factoryScript);
  const poolAddress = lucid.utils.validatorToAddress(poolScript);
  const escrowAddress = lucid.utils.validatorToAddress(escrowScript);

  const steakAddress = lucid.utils.validatorToRewardAddress(steakScript);

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
  log(`minted dummy tokens: ${okMinted}`);
  log("mintedHash:", mintedHash);

  let ref = { txHash: mintedHash, outputIndex: 1 };
  let mintedChange = (await emulator.getUtxosByOutRef([ref]))[0];

  const factoryMintingPolicy: Script = { type: "PlutusV2", script: factoryMint };
  const factoryMintRedeemer = "d87980"; // MakeFactory
  const factoryPolicyId = lucid.utils.mintingPolicyToId(factoryMintingPolicy);
  log(`factoryPolicyId: ${factoryPolicyId}`);

  // poolSH = "", poolCS = "", 1 scooper (userPkh)
  /*
  const newFactoryDatum =
    "d8799f41009f41004100ff" +
    "9f581c" + userPkh.to_hex() + "ff" +
    "9fffff";
  */
  const newFactoryDatum = factoryDatum(poolScriptHash, userPkh.to_hex())
  log("newFactoryDatum", newFactoryDatum);

  async function bootFactory(): Promise<TxHash> {
    const tx = await lucid.newTx()
      .collectFrom([mintedChange])
      .mintAssets({
        [toUnit(factoryPolicyId, fromText("factory"))]: 1n
      }, factoryMintRedeemer)
      .validTo(emulator.now() + 30000)
      .attachMintingPolicy(factoryMintingPolicy)
      .payToContract(factoryAddress, { inline: newFactoryDatum }, {
        "lovelace": 2_000_000n,
        [toUnit(factoryPolicyId, fromText("factory"))]: 1n
      })
      .complete();
    const signedTx = await tx.sign().complete();
    return signedTx.submit();
  }

  const bootedHash = await bootFactory();
  const okBooted = await emulator.awaitTx(bootedHash);
  log(`booted factory: ${okBooted}`);
  log("bootedHash:", bootedHash);

  ref = { txHash: bootedHash, outputIndex: 0 };
  log(`get ${ref.txHash}#${ref.outputIndex}`);
  let newFactory = (await emulator.getUtxosByOutRef([ref]))[0];
  newFactory.datum = newFactoryDatum;

  ref = { txHash: bootedHash, outputIndex: 1 };
  let newFactoryChange = (await emulator.getUtxosByOutRef([ref]))[0];

  log("ledger after boot: ")
  log(emulator.ledger);

  const configuredFactoryDatum = factoryDatum(poolScriptHash, userPkh.to_hex());

  log("configured factory datum: " + configuredFactoryDatum)

  const configureFactoryRedeemer = "d87a81d87980"; // Note: wrapped in ctor-2 tag, to trick the compiler into running the spend validator

  async function configureFactory(): Promise<TxHash> {
    const tx = await lucid.newTx()
      .collectFrom([newFactoryChange])
      .collectFrom([newFactory], configureFactoryRedeemer)
      .validTo(emulator.now() + 30000)
      .attachSpendingValidator({ type: "PlutusV2", script: factoryValidator })
      .payToContract(factoryAddress, { inline: configuredFactoryDatum }, {
        "lovelace": 2_000_000n,
        [toUnit(factoryPolicyId, fromText("factory"))]: 1n
      })
      .complete();
    const signedTx = await tx.sign().complete();
    return signedTx.submit();
  }

  const configuredHash = await configureFactory();
  const okConfigured = await emulator.awaitTx(configuredHash);
  log(`configured factory: ${okConfigured}`);
  log(configuredHash);

  log("ledger after configure: ")
  log(emulator.ledger);

  ref = { txHash: configuredHash, outputIndex: 0 };
  log(`get ${ref.txHash}#${ref.outputIndex}`);
  const factory = (await emulator.getUtxosByOutRef([ref]))[0];
  if (!factory) { throw "No factory"; }
  const factoryChange = (await emulator.getUtxosByOutRef([{
    txHash: configuredHash,
    outputIndex: 1
  }]))[0];
  if (!factoryChange) { throw "No factory change"; }
  factory.datum = configuredFactoryDatum;

  // !newPoolIdent = dropByteString 1 $ blake2b_256 $
  //   getTxId (txOutRefId firstInput) <> "#" <> getIdent (intToIdent (txOutRefIdx firstInput))
  const poolInputTxHash = fromHex(factoryChange.txHash);
  const numberSign = new Uint8Array([0x23]);
  const poolInputTxIx = new Uint8Array([0x01]); // ident encoding for output index 1
  let poolInputRef = new Uint8Array([]);
  poolInputRef = concat(poolInputRef, poolInputTxHash);
  poolInputRef = concat(poolInputRef, numberSign);
  poolInputRef = concat(poolInputRef, poolInputTxIx);
  let newPoolId = C.hash_blake2b256(poolInputRef).slice(1); // Truncate first byte
  const p = new Uint8Array([0x70]); // 'p'
  const l = new Uint8Array([0x6c]); // 'l'
  const poolNftName = concat(p, newPoolId);
  const poolLqName = concat(l, newPoolId);
  const poolNftNameHex = toHex(poolNftName);
  const poolLqNameHex = toHex(poolLqName);
  log("poolNftName (hex): ", poolNftNameHex);
  log("poolLqName (hex): ", poolLqNameHex);

  const newPoolDatum =
    "d8799f" +
    "581f" + toHex(newPoolId) +
    "9f9f" +
    "40" + // Empty string for ADA
    "40" + // Empty string for ADA
    "ff" +
    "9f" +
    "581c" + dummyPolicyId +
    "45" + fromText("DUMMY") +
    "ffff" +
    "1a3b9aca009f011907d0ff00" +
    Data.to(2000000n) +
    "ff";

  log("newPoolDatum: ", newPoolDatum);

  const poolMintRedeemer =
    "d87a9f9f9f4040ff9f" +
    "581c" + dummyPolicyId +
    "45" + fromText("DUMMY") +
    "ffffff";

  log("poolMintRedeemer: ", poolMintRedeemer);

  log("ledger before mint:", emulator.ledger);

  assert(factory.datum == configuredFactoryDatum);
  assert(factoryChange.datum == null);

  log("datum table", emulator.datumTable);

  async function mintPool(): Promise<TxHash> {
    const tx = await lucid.newTx()
      .mintAssets({
        [toUnit(poolPolicyId, poolNftNameHex)]: 1n,
        [toUnit(poolPolicyId, poolLqNameHex)]: 1_000_000_000n,
      }, poolMintRedeemer)
      .validTo(emulator.now() + 30000)
      .attachMintingPolicy(poolMintingPolicy)
      .readFrom([factory])
      .collectFrom([factoryChange])
      .payToContract(poolAddress, { inline: newPoolDatum }, {
        "lovelace": 1_000_000_000n + 2_000_000n,
        [toUnit(poolPolicyId, poolNftNameHex)]: 1n,
        [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000n,
      })
      .payToAddress(userAddress, {
        "lovelace": 2_000_000n,
        [toUnit(poolPolicyId, poolLqNameHex)]: 1_000_000_000n,
      })
      .complete();
    log("mintPoolTx: ", tx.toString());
    const signedTx = await tx.sign().complete();
    return signedTx.submit();
  }

  mintedHash = await mintPool();
  okMinted = await emulator.awaitTx(mintedHash);
  log(`minted pool: ${okMinted}`);
  log("mintedHash:", mintedHash);

  ref = { txHash: mintedHash, outputIndex: 0 };
  log(`get ${ref.txHash}#${ref.outputIndex}`);
  const pool = (await emulator.getUtxosByOutRef([ref]))[0];

  /*
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
  */
  const ownerMultisigScript = lucid.utils.nativeScriptFromJson({
    type: "all",
    scripts: [
      { type: "sig", keyHash: userPkh.to_hex() }
    ],
  });
  const ownerMultisig = lucid.utils.validatorToScriptHash(ownerMultisigScript);
  const newEscrowDatum =
    "d8799fd8799f" +
    "581c" + userPkh.to_hex() + "ff" +
    "1a002625a0" + // Scooper fee
    "d8799fd8799fd8799f" +
    "581c" + userPkh.to_hex() + "ff" + // destination pkh
    "d87a80ff" + // No staking credential
    "d87980ff" + // No datum
    "d8799f9f" +
    "40" + "40" +
    "1a00989680" + "ff" +
    "9f" +
    "581c" + dummyPolicyId +
    "45" + fromText("DUMMY") +
    "00" +
    "ff" +
    "ff" +
    "d87980" + // extra void for extension data
    "ff";

  log(`newEscrowDatum: ${newEscrowDatum}`);

  async function listEscrow(): Promise<TxHash> {
    const tx = await lucid.newTx()
      .validTo(emulator.now() + 30000)
      .payToContract(escrowAddress, { inline: newEscrowDatum }, {
        "lovelace": 4_500_000n + 10_000_000n,
      })
      .complete();
    const signedTx = await tx.sign().complete();
    return signedTx.submit();
  }

  let listedEscrows: UTxO[] = [];
  for (let i = 0n; i < escrowsCount; i++) {
    let listedHash = await listEscrow();
    const escrowHash = listedHash;
    let okListed = await emulator.awaitTx(listedHash);
    log(`listed escrow ${i}: ${okListed}`);
    ref = { txHash: escrowHash, outputIndex: 0 };
    const escrow = (await emulator.getUtxosByOutRef([ref]))[0];
    listedEscrows.push(escrow);
  }

  let escrowTakes: bigint[] = [];
  let poolABL: ABL = {
    a: 1_000_000_000n,
    b: 1_000_000_000n,
    liq: 1_000_000_000n,
  };
  let takes: bigint = 0n;
  const swapFees: SwapFees = { numerator: 1n, denominator: 2000n };
  for (let e of listedEscrows) {
    [takes, poolABL] = doSwap(Coin.CoinA, 10_000_000n, swapFees, poolABL);
    escrowTakes.push(takes);
  }
  log("escrowTakes:", escrowTakes);

  const totalRewards = 2_500_000n * escrowsCount;

  const scoopedPoolDatum =
    "d8799f" +
    "581f" + toHex(newPoolId) +
    "9f9f" +
    "40" + // Empty string for ADA
    "40" + // Empty string for ADA
    "ff" +
    "9f" +
    "581c" + dummyPolicyId +
    "45" + fromText("DUMMY") +
    "ffff" +
    "1a3b9aca00" + // Liquidity unchanged
    "9f011907d0ff00" +
    Data.to(2000000n + escrowsCount * 2500000n) +
    "ff";

  log("newPoolDatum: " + newPoolDatum);
  log("scoopedPoolDatum: " + scoopedPoolDatum);

  let toSpend: { txHash: string, outputIndex: number, address: string }[] = [];
  toSpend.push({ txHash: pool.txHash, outputIndex: pool.outputIndex, address: poolAddress });
  for (let e of listedEscrows) {
    toSpend.push({ txHash: e.txHash, outputIndex: e.outputIndex, address: e.address });
  }
  toSpend.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));
  let i = 0n;
  let orderString = "";
  for (let u of toSpend) {
    if (u.address == escrowAddress) {
      orderString += i.toString(16).padStart(2, '0');
      //orderString += "00";
    }
    i++;
  }
  console.log("orderString:", orderString);

  const scoopPoolRedeemer =
    "d87a81" + // wrap this to avoid running the minting script
    "d8799f" +
    "00" + // settings input; only one reference input, so it's always first
    "00" + // signatory index; only one required signer, so it's always first
    "00" + // scooper index; only one scooper, so it's always first
    "9f" + orderString + "ff" +
    "ff";
  
  log("scoopPoolRedeemer: " + scoopPoolRedeemer);

  const escrowScoopRedeemer = "d87980"; // Scoop!

  const scooperFee = 2_500_000n;
  const rider = 2_000_000n;

  const totalReturn = escrowTakes.reduce((x,acc) => x + acc);

  log(emulator.ledger);
  console.log("about to scoop the pool");
  console.log("spending:");
  console.log("escrows: ");
  console.log(listedEscrows);
  console.log("pool: ");
  console.log(pool);

  emulator.ledger["ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0"] = {
    utxo: {
      txHash: "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
      outputIndex: 0,
      assets: { lovelace: 1_000_000_000_000_000_000n },
      address: "addr_test1vp4020l57p2rfzkcyhrf9hvahrchvz5wp6k0ntulnycx2yclzlnzv",
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };

  let dummyChange = 
    emulator
      .ledger["ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0"]
      .utxo;

  async function scoopPool(): Promise<TxHash> {
    let tx = await lucid.newTx()
      .validFrom(emulator.now())
      .validTo(emulator.now() + 30000);
    let i = 0n;
    for (let e of listedEscrows) {
      // The integer field is currently unused so don't bother
      /*let ix = i.toString(16).padStart(2, '0');
      tx.collectFrom([e], "d8799f" + i + "ff");
      i++;*/
      tx.collectFrom([e], escrowScoopRedeemer);
      log("collect from escrow");
      log(e);
    }
    tx
      .collectFrom([dummyChange])
      .collectFrom([pool], scoopPoolRedeemer)
      .readFrom([factory])
      .attachSpendingValidator({ type: "PlutusV2", script: escrowValidator })
      .attachSpendingValidator({ type: "PlutusV2", script: poolValidator })
      .attachSpendingValidator({ type: "PlutusV2", script: steakValidator })
      .addSigner(userAddress)
      .withdraw(steakAddress, 0n, "00")
      .payToContract(poolAddress, { inline: scoopedPoolDatum }, {
        "lovelace":
          1_000_000_000n +
          escrowsCount * 10_000_000n +
          escrowsCount * scooperFee +
          rider,
        [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000n - totalReturn,
        [toUnit(poolPolicyId, poolNftNameHex)]: 1n,
      });

    for(let e of escrowTakes) {
      tx.payToAddress(userAddress, {
        "lovelace": rider,
        [toUnit(dummyPolicyId, fromText("DUMMY"))]: e,
      })
    }
    log(await tx.toString());
    let completeTx = await tx.complete();
    console.log(`${escrowsCount} escrows:\tcpu=${completeTx.exUnits.cpu}\tmem=${completeTx.exUnits.mem}`);
    log("complete:", completeTx.toString());
    const signedTx = await completeTx.sign().complete();
    log(signedTx.toString());
    return signedTx.submit();
  }

  const scoopedHash = await scoopPool();
  const okScooped = await emulator.awaitTx(scoopedHash);
  log(`scooped pool: ${okScooped}`);
  log(scoopedHash);

  log(emulator.ledger);
}