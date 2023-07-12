import {
  Blockfrost,
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
  toPublicKey
} from "../../lucid/mod.ts";
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";
import { parse } from "https://deno.land/std@0.184.0/flags/mod.ts";
import { ABL, Coin, SwapFees, doSwap } from "./cpp.ts";

const verbose = false ;

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
  string: ["scriptsFile"]
});

if (flags.scriptsFile == undefined) {
  throw "no scripts file";
}
const s = await Deno.readTextFile(flags.scriptsFile);
const scriptsJson = JSON.parse(s);
const poolValidator = scriptsJson["pool-validator"];
const factoryValidator = scriptsJson["factory-validator"];
const escrowValidator = scriptsJson["escrow-validator"];
const steakValidator = scriptsJson["steak-validator"];
const poolMint = scriptsJson["pool-mint"];
const factoryMint = scriptsJson["factory-mint"];

const dummy = await Lucid.new(undefined, "Custom");

const userPrivateKey = "ed25519_sk1zxsfsl8ehspny4750jeydt5she7dzstrj7za5vgxl6929kr9d33quqkgp3";
//generatePrivateKey();
const userPublicKey = toPublicKey(userPrivateKey);
log(userPublicKey);
const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
log("userPkh", userPkh.to_hex());
const userAddress = await dummy.selectWalletFromPrivateKey(userPrivateKey).wallet.address();

for (let escrowsCount = 1n; escrowsCount < 20n; escrowsCount++) {
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
  log("User address", userAddress);

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
  log(mintedHash);

  const factoryMintingPolicy: Script = { type: "PlutusV2", script: factoryMint };
  const factoryMintRedeemer = "d87980"; // MakeFactory
  const factoryPolicyId = scriptsJson["factory-boot-cs"];
  assert(factoryPolicyId == lucid.utils.mintingPolicyToId(factoryMintingPolicy));
  log(`factoryPolicyId: ${factoryPolicyId}`);

  // poolSH = "", poolCS = "", 1 scooper (userPkh)
  const newFactoryDatum =
    "d8799f41004100" +
    "81581c" + userPkh.to_hex() +
    "80ff";

  async function bootFactory(): Promise<TxHash> {
    const tx = await lucid.newTx()
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
  log(bootedHash);

  let ref = { txHash: bootedHash, outputIndex: 0 };
  log(`get ${ref.txHash}#${ref.outputIndex}`);
  let newFactory = (await emulator.getUtxosByOutRef([ref]))[0];
  newFactory.datum = newFactoryDatum;

  log("ledger after boot: ")
  log(emulator.ledger);

  const configuredFactoryDatum =
    "d8799f581c" +
    poolScriptHash +
    "581c" +
    poolPolicyId +
    "9f581c" + userPkh.to_hex() +
    "ff80ff";

  log("configured factory datum: " + configuredFactoryDatum)

  const configureFactoryRedeemer = "d87980";

  // Spend all our utxos when configuring to guarantee deterministic evaluation
  let myUtxos = await lucid.utxosAt(userAddress);

  async function configureFactory(): Promise<TxHash> {
    const tx = await lucid.newTx()
      .collectFrom(myUtxos)
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
    "d8799fd8799fd8799f" +
    "40" + // Empty string for ADA
    "40" + // Empty string for ADA
    "ffd8799f" +
    "581c" + dummyPolicyId +
    "45" + fromText("DUMMY") +
    "ffff" +
    "581f" + toHex(newPoolId) +
    "1a3b9aca00d8799f011907d0ff0000ff";

  log("newPoolDatum: ", newPoolDatum);

  const poolMintRedeemer =
    "d87a9fd8799f4040ffd8799f" +
    "581c" + dummyPolicyId +
    "45" + fromText("DUMMY") +
    "ffff";

  log("poolMintRedeemer: ", poolMintRedeemer);

  log(emulator.ledger);

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
    log(tx.toString());
    const signedTx = await tx.sign().complete();
    return signedTx.submit();
  }

  mintedHash = await mintPool();
  okMinted = await emulator.awaitTx(mintedHash);
  log(`minted pool: ${okMinted}`);
  log(mintedHash);

  ref = { txHash: mintedHash, outputIndex: 0 };
  log(`get ${ref.txHash}#${ref.outputIndex}`);
  const pool = (await emulator.getUtxosByOutRef([ref]))[0];

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

  let listedEscrows = [];
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
  log(escrowTakes);

  const totalRewards = 2_500_000n * escrowsCount;

  const scoopedPoolDatum =
    "d8799fd8799fd8799f" +
    "40" + // Empty string for ADA
    "40" + // Empty string for ADA
    "ffd8799f" +
    "581c" + dummyPolicyId +
    "45" + fromText("DUMMY") +
    "ffff" +
    "581f" + toHex(newPoolId) +
    "1a3b9aca00" + // Liquidity unchanged
    "d8799f011907d0ff00" +
    "1a" + totalRewards.toString(16).padStart(8, '0') +
    "ff";

  log("newPoolDatum: " + newPoolDatum);
  log("scoopedPoolDatum: " + scoopedPoolDatum);

  let orderString = "";
  for (let i = 0n; i < escrowsCount; i++) {
    orderString += i.toString(16).padStart(2, '0');
  }
  const scoopPoolRedeemer =
    "d8799f" +
    "581c" + userPkh.to_hex() +
    "9f" + orderString +
    "ffff";

  const escrowScoopRedeemer = "d87980"; // Scoop!

  const scooperFee = 2_500_000n;
  const rider = 2_000_000n;

  const totalReturn = escrowTakes.reduce((x,acc) => x + acc);

  async function scoopPool(): Promise<TxHash> {
    let tx = await lucid.newTx()
      .validFrom(emulator.now())
      .validTo(emulator.now() + 30000);
    for (let e of listedEscrows) {
      tx.collectFrom([e], escrowScoopRedeemer);
    }
    let completeTx = await tx
      .collectFrom([pool], scoopPoolRedeemer)
      .readFrom([factory])
      .attachSpendingValidator({ type: "PlutusV2", script: escrowValidator })
      .attachSpendingValidator({ type: "PlutusV2", script: poolValidator })
      .attachSpendingValidator({ type: "PlutusV2", script: steakValidator })
      .withdraw(steakAddress, 0, "00")
      .payToAddress(userAddress, {
        "lovelace": escrowsCount * rider,
        [toUnit(dummyPolicyId, fromText("DUMMY"))]: totalReturn,
      })
      .payToContract(poolAddress, { inline: scoopedPoolDatum }, {
        "lovelace":
          1_000_000_000n +
          escrowsCount * 10_000_000n +
          escrowsCount * scooperFee +
          rider,
        [toUnit(dummyPolicyId, fromText("DUMMY"))]: 1_000_000_000n - totalReturn,
        [toUnit(poolPolicyId, poolNftNameHex)]: 1n,
      })
      .complete();
    console.log(`${escrowsCount} escrows:\tcpu=${completeTx.exUnits.cpu}\tmem=${completeTx.exUnits.mem}`);
    const signedTx = await completeTx.sign().complete();
    return signedTx.submit();
  }

  const scoopedHash = await scoopPool();
  const okScooped = await emulator.awaitTx(scoopedHash);
  log(`scooped pool: ${okScooped}`);
  log(scoopedHash);

  log(emulator.ledger);
}
