import {
  Address,
  Blockfrost,
  Data,
  Constr,
  Script,
  ScriptHash,
  PolicyId,
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
import { Args, parse } from "https://deno.land/std@0.184.0/flags/mod.ts";
import { ABL, Coin, SwapFees, doSwap } from "./cpp.ts";
import { Datum } from "../../lucid/src/core/libs/cardano_multiplatform_lib/cardano_multiplatform_lib.generated.js";

const verbose = true;

function log(...args: any[]) {
  if (verbose) {
    console.log(...args);
  }
}

function assert(p: boolean) {
  if (!p) {
    throw "Assertion failed!"
  }
}

interface Scripts {
  poolValidator: Script;
  poolScriptHash: ScriptHash;
  poolAddress: Address;
  factoryValidator: Script;
  factoryScriptHash: ScriptHash;
  factoryAddress: Address;
  escrowValidator: Script;
  escrowScriptHash: ScriptHash;
  escrowAddress: Address;
  steakValidator: Script;
  steakScriptHash: ScriptHash;
  steakAddress: Address;
  poolMint: Script;
  poolPolicyId: PolicyId;
  factoryMint: Script;
  factoryPolicyId: PolicyId;
};

function bytesToScript(bytes: string) {
  return { type: "PlutusV2", script: bytes };
}

function getScriptsAiken(lucid: Lucid, json: any): Scripts {
  let validator = json["validators"];
  let out: any = {};
  for (let v of validator) {
    if (v.title == "order.spend") {
      out.escrowValidator = bytesToScript(v.compiledCode);
      out.escrowScriptHash = lucid.utils.validatorToScriptHash(out.escrowValidator);
      out.escrowAddress = lucid.utils.validatorToAddress(out.escrowValidator);
    }
    if (v.title == "settings.spend") {
      out.factoryValidator = bytesToScript(v.compiledCode);
      out.factoryScriptHash = lucid.utils.validatorToScriptHash(out.factoryValidator);
      out.factoryAddress = lucid.utils.validatorToAddress(out.factoryValidator);
    }
    if (v.title == "pool.spend") {
      out.poolValidator = bytesToScript(v.compiledCode);
      out.poolScriptHash = lucid.utils.validatorToScriptHash(out.poolValidator);
      out.poolAddress = lucid.utils.validatorToAddress(out.poolValidator);
    }
    if (v.title == "stake.stake") {
      out.steakValidator = bytesToScript(v.compiledCode);
      out.steakScriptHash = lucid.utils.validatorToScriptHash(out.steakValidator);
      out.steakAddress = lucid.utils.validatorToRewardAddress(out.steakValidator);
    }
    if (v.title == "pool.mint") {
      out.poolMint = bytesToScript(v.compiledCode);
      out.poolPolicyId = lucid.utils.mintingPolicyToId(out.poolMint);
    }
    if (v.title == "settings.mint") {
      out.factoryMint = bytesToScript(v.compiledCode);
      out.factoryPolicyId = lucid.utils.mintingPolicyToId(out.factoryMint);
    }
  }
  return out;
}

function getScriptsPlutusTx(lucid: Lucid, json: any): Scripts {
  let out: any = {};
  out.poolValidator = bytesToScript(json["pool-validator"]);
  out.poolScriptHash = lucid.utils.validatorToScriptHash(out.poolValidator);
  out.poolAddress = lucid.utils.validatorToAddress(out.poolValidator);

  out.factoryValidator = bytesToScript(json["factory-validator"]);
  out.factoryScriptHash = lucid.utils.validatorToScriptHash(out.factoryValidator);
  out.factoryAddress = lucid.utils.validatorToAddress(out.factoryValidator);

  out.escrowValidator = bytesToScript(json["escrow-validator"]);
  out.escrowScriptHash = lucid.utils.validatorToScriptHash(out.escrowValidator);
  out.escrowAddress = lucid.utils.validatorToAddress(out.escrowValidator);

  out.steakValidator = bytesToScript(json["steak-validator"]);
  out.steakScriptHash = lucid.utils.validatorToScriptHash(out.steakValidator);
  out.steakAddress = lucid.utils.validatorToAddress(out.steakValidator);

  out.poolMint = bytesToScript(json["pool-mint"]);
  out.poolPolicyId = lucid.utils.mintingPolicyToId(out.poolMint);

  out.factoryMint = bytesToScript(json["factory-mint"]);
  out.factoryPolicyId = lucid.utils.mintingPolicyToId(out.factoryMint);

  return out;
}

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

function factoryMintRedeemer() { return "d87980" };
function factorySpendRedeemer() {
  return "d87a81d87980" // Note: wrapped in ctor-2 tag, to trick the compiler into running the spend validator
};

function poolDatum(poolIDHex: string, dummyPolicyHex: string, rewards: bigint): string {
  return "d8799f" +
    "581f" + poolIDHex +
    "9f9f" +
    "40" + // Empty string for ADA
    "40" + // Empty string for ADA
    "ff" +
    "9f" +
    "581c" + dummyPolicyHex +
    "45" + fromText("DUMMY") +
    "ffff" +
    "1a3b9aca009f011907d0ff00" +
    Data.to(rewards) +
    "ff";
}

function poolMintRedeemer(dummyPolicyHex: string) {
  return "d87a9f9f9f4040ff9f" +
    "581c" + dummyPolicyHex +
    "45" + fromText("DUMMY") +
    "ffff" +
    "00" +
    "ff";
}

function scoopRedeemer(inputOrder: bigint[]): string {
  let orderString = "";
  for (let i of inputOrder) {
      orderString += Data.to(i);
  }
  return "d87a81" + // wrap this to avoid running the minting script
    "d8799f" +
    "00" + // signatory index; only one required signer, so it's always first
    "00" + // scooper index; only one scooper, so it's always first
    "9f" + orderString + "ff" +
    "ff";
}

function orderDatum(userPkhHex: string, dummyPolicyHex: string): string {
  return "d8799fd8799f" +
    "581c" + userPkhHex + "ff" +
    "1a002625a0" + // Scooper fee
    "d8799fd8799fd8799f" +
    "581c" + userPkhHex + "ff" + // destination pkh
    "d87a80ff" + // No staking credential
    "d87980ff" + // No datum
    "d8799f9f" +
    "40" + "40" +
    "1a00989680" + "ff" +
    "9f" +
    "581c" + dummyPolicyHex +
    "45" + fromText("DUMMY") +
    "00" +
    "ff" +
    "ff" +
    "d87980" + // extra void for extension data
    "ff";
}

function computePoolId(utxo: UTxO) {
  const poolInputTxHash = fromHex(utxo.txHash);
  const numberSign = new Uint8Array([0x23]);
  const poolInputTxIx = new Uint8Array([utxo.outputIndex]); // ident encoding for output index 1
  let poolInputRef = new Uint8Array([]);
  poolInputRef = concat(poolInputRef, poolInputTxHash);
  poolInputRef = concat(poolInputRef, numberSign);
  poolInputRef = concat(poolInputRef, poolInputTxIx);
  return C.hash_blake2b256(poolInputRef).slice(1); // Truncate first byte
}

function computePoolNftName(poolId: Uint8Array) {
  const p = new Uint8Array([0x70]); // 'p'
  return toHex(concat(p, poolId));
}

function computePoolLqName(poolId: Uint8Array) {
  const l = new Uint8Array([0x6c]); // 'l'
  return toHex(concat(l, poolId));
}

async function bench_endToEndScoop(flags: Args, scripts: Scripts, dummy: Lucid) {
  const userPrivateKey = "ed25519_sk1zxsfsl8ehspny4750jeydt5she7dzstrj7za5vgxl6929kr9d33quqkgp3";
  //generatePrivateKey();
  const userPublicKey = toPublicKey(userPrivateKey);
  log("userPublicKey", userPublicKey);
  const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
  log("userPkh", userPkh.to_hex());
  const userAddress = await dummy.selectWalletFromPrivateKey(userPrivateKey).wallet.address();
  log("userAddress", userAddress);

  const max = flags.max ? BigInt(flags.max) : 30n;
  const min = flags.min ? BigInt(flags.min) : (flags.findMax ? 1n : max);
  for (let escrowsCount = min; escrowsCount <= max; escrowsCount++) {
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
      maxTxExMem: flags.findMax ? PROTOCOL_PARAMETERS_DEFAULT.maxTxExMem : 999999999999999n,
    });

    const lucid = await Lucid.new(emulator);

    lucid.selectWalletFromPrivateKey(userPrivateKey);

    const newFactoryDatum = factoryDatum(scripts.poolScriptHash, userPkh.to_hex())
    log("newFactoryDatum", newFactoryDatum);

    log("ledger before boot: ")
    log(emulator.ledger);

    async function bootFactory(): Promise<TxHash> {
      const tx = await lucid.newTx()
        .mintAssets({
          [toUnit(scripts.factoryPolicyId, fromText("settings"))]: 1n
        }, factoryMintRedeemer())
        .validTo(emulator.now() + 30000)
        .attachMintingPolicy(scripts.factoryMint)
        .payToContract(scripts.factoryAddress, { inline: newFactoryDatum }, {
          "lovelace": 2_000_000n,
          [toUnit(scripts.factoryPolicyId, fromText("settings"))]: 1n
        })
        .complete();
      const signedTx = await tx.sign().complete();
      return signedTx.submit();
    }

    const bootedHash = await bootFactory();
    const okBooted = await emulator.awaitTx(bootedHash);
    log(`booted factory: ${okBooted}`);
    log("bootedHash:", bootedHash);

    let ref = { txHash: bootedHash, outputIndex: 0 };
    log(`get ${ref.txHash}#${ref.outputIndex}`);
    let newFactory = (await emulator.getUtxosByOutRef([ref]))[0];
    newFactory.datum = newFactoryDatum;

    ref = { txHash: bootedHash, outputIndex: 1 };
    let newFactoryChange = (await emulator.getUtxosByOutRef([ref]))[0];

    log("ledger after boot: ")
    log(emulator.ledger);

    const configuredFactoryDatum = factoryDatum(scripts.poolScriptHash, userPkh.to_hex());

    log("configured factory datum: " + configuredFactoryDatum)

    async function configureFactory(): Promise<TxHash> {
      const tx = await lucid.newTx()
        .collectFrom([newFactoryChange])
        .collectFrom([newFactory], factorySpendRedeemer())
        .validTo(emulator.now() + 30000)
        .attachSpendingValidator(scripts.factoryValidator)
        .payToContract(scripts.factoryAddress, { inline: configuredFactoryDatum }, {
          "lovelace": 2_000_000n,
          [toUnit(scripts.factoryPolicyId, fromText("settings"))]: 1n
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
    factory.datum = configuredFactoryDatum;

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

    ref = { txHash: mintedHash, outputIndex: 1 };
    let mintedChange = (await emulator.getUtxosByOutRef([ref]))[0];

    let walletUtxos = await lucid.wallet.getUtxos();
    walletUtxos.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));

    console.log(`first input: ${walletUtxos[0].txHash}#${walletUtxos[0].outputIndex}`);
    let newPoolId = computePoolId(walletUtxos[0]);
    const poolNftNameHex = computePoolNftName(newPoolId);
    const poolLqNameHex = computePoolLqName(newPoolId);
    log("poolNftName (hex): ", poolNftNameHex);
    log("poolLqName (hex): ", poolLqNameHex);

    const newPoolDatum = poolDatum(toHex(newPoolId), dummyPolicyId, 2000000n);
    log("newPoolDatum: ", newPoolDatum);
    log("poolMintRedeemer: ", poolMintRedeemer(dummyPolicyId));
    log("ledger before mint:", emulator.ledger);

    assert(factory.datum == configuredFactoryDatum);

    log("datum table", emulator.datumTable);
    log("pool id hash", toHex(newPoolId));
    log("pool policy id, pool nft token", scripts.poolPolicyId, poolNftNameHex);

    async function mintPool(): Promise<TxHash> {
      const tx = await lucid.newTx()
        .mintAssets({
          [toUnit(scripts.poolPolicyId, poolNftNameHex)]: 1n,
          [toUnit(scripts.poolPolicyId, poolLqNameHex)]: 1_000_000_000n,
        }, poolMintRedeemer(dummyPolicyId))
        .validTo(emulator.now() + 30000)
        .attachMintingPolicy(scripts.poolMint)
        .readFrom([factory])
        .collectFrom(walletUtxos)
        .payToContract(scripts.poolAddress, { inline: newPoolDatum }, {
          "lovelace": 1_000_000_000n + 2_000_000n,
          [toUnit(scripts.poolPolicyId, poolNftNameHex)]: 1n,
          [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000n,
        })
        .payToAddress(userAddress, {
          "lovelace": 2_000_000n,
          [toUnit(scripts.poolPolicyId, poolLqNameHex)]: 1_000_000_000n,
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

    const ownerMultisigScript = lucid.utils.nativeScriptFromJson({
      type: "all",
      scripts: [
        { type: "sig", keyHash: userPkh.to_hex() }
      ],
    });
    const ownerMultisig = lucid.utils.validatorToScriptHash(ownerMultisigScript);

    log(`newEscrowDatum: ${orderDatum(userPkh.to_hex(), dummyPolicyId)}`);

    async function listEscrow(): Promise<TxHash> {
      const tx = await lucid.newTx()
        .validTo(emulator.now() + 30000)
        .payToContract(scripts.escrowAddress, { inline: orderDatum(userPkh.to_hex(), dummyPolicyId) }, {
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

    const scoopedPoolDatum = poolDatum(toHex(newPoolId), dummyPolicyId, 2000000n + totalRewards);

    log("newPoolDatum: " + newPoolDatum);
    log("scoopedPoolDatum: " + scoopedPoolDatum);

    emulator.ledger["00000000000000000000000000000000000000000000000000000000000000000"] = {
      utxo: {
        txHash: "0000000000000000000000000000000000000000000000000000000000000000",
        outputIndex: 0,
        assets: { lovelace: 1_000_000_000_000_000_000n },
        address: userAddress,
        datumHash: undefined,
        datum: undefined,
        scriptRef: undefined
      },
      spent: false
    };

    let toSpend: UTxO[] = [];
    toSpend.push(emulator.ledger["00000000000000000000000000000000000000000000000000000000000000000"].utxo)
    toSpend.push(pool);
    for (let e of listedEscrows) {
      toSpend.push(e);
    }
    toSpend.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));
    let i = 0n;
    let indexingSet: bigint[] = [];
    for (let i = 0n; i < toSpend.length; i++) {
      if (toSpend[Number(i)].address == scripts.escrowAddress) {
        indexingSet.push(i);
      }
    }
    console.log("indexing set:", indexingSet);

    const scoopPoolRedeemer = scoopRedeemer(indexingSet)

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

    async function scoopPool(): Promise<{ txHash: TxHash, cpu?: number, mem?: number }> {
      let tx = await lucid.newTx()
        .validFrom(emulator.now())
        .validTo(emulator.now() + 30000);
      let i = 0n;
      for (let e of toSpend) {
        if (e.address == scripts.poolAddress) {
          tx.collectFrom([pool], scoopPoolRedeemer)
        } else if (e.address == scripts.escrowAddress) {
          tx.collectFrom([e], escrowScoopRedeemer);
        } else {
          tx.collectFrom([e])
        }
      }
      tx
        .readFrom([factory])
        .attachSpendingValidator(scripts.escrowValidator)
        .attachSpendingValidator(scripts.poolValidator)
        .attachSpendingValidator(scripts.steakValidator)
        .addSigner(userAddress)
        .withdraw(scripts.steakAddress, 0n, "00")
        .payToContract(scripts.poolAddress, { inline: scoopedPoolDatum }, {
          "lovelace":
            1_000_000_000n +
            escrowsCount * 10_000_000n +
            escrowsCount * scooperFee +
            rider,
          [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000n - totalReturn,
          [toUnit(scripts.poolPolicyId, poolNftNameHex)]: 1n,
        });

      // We add the escrows to the order in reverse, because in the script, prepending to the list is cheaper
      for(let e of escrowTakes) {
        tx.payToAddress(userAddress, {
          "lovelace": rider,
          [toUnit(dummyPolicyId, fromText("DUMMY"))]: e,
        })
      }
      log(await tx.toString());
      try {
        let completeTx = await tx.complete({ coinSelection: false });
        log("complete:", completeTx.toString());
        const signedTx = await completeTx.sign().complete();
        log(signedTx.toString());
        return { txHash: await signedTx.submit(), cpu: completeTx?.exUnits?.cpu, mem: completeTx?.exUnits?.mem };
      } catch(e) {
        log("Maximum escrows:", escrowsCount - 1n)
        throw e;
      }
    }

    const scoopResult = await scoopPool();
    const okScooped = await emulator.awaitTx(scoopResult.txHash);
    log(`scooped pool: ${okScooped}`);
    log(scoopResult.txHash);
    log(emulator.ledger);

    console.log(`${escrowsCount} escrows:\tcpu=${scoopResult.cpu}\tmem=${scoopResult.mem}`);
  }
}

async function main() {
  const flags = parse(Deno.args, {
    string: ["scriptsFile"],
    boolean: ["aiken", "findMax"],
    //number: ["min", "max"],
  });
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }
  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);
  const dummy = await Lucid.new(undefined, "Custom");
  let scripts: Scripts;
  if (flags.aiken) {
    console.log("Benchmarking Aiken contracts");
    scripts = getScriptsAiken(dummy, scriptsJson);
  } else {
    console.log("Benchmarking Plutus contracts");
    scripts = getScriptsPlutusTx(dummy, scriptsJson);
  }
  await bench_endToEndScoop(flags, scripts, dummy);
}

await main();
