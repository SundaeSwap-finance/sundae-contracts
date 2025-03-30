import {
  Blockfrost,
  Data,
  fromText,
  Lucid,
  toUnit,
  Hasher,
  fromHex,
  toHex,
  concat,
  Provider,
  Utxo,
  Script,
  Credential,
  Addresses,
  Assets,
  Crypto,
  NativeScript,
  Codec,
  AddressDetails,
  Network,
  applyParamsToScript
} from "https://deno.land/x/lucid@0.20.5/mod.ts";
import { parse } from "https://deno.land/std@0.184.0/flags/mod.ts";
import { ABL, Coin, doSwap } from "./cpp.ts";
import * as random from "https://deno.land/x/random@v1.1.2/Random.js";
import * as types from "./types.ts";
import { EmulatorWithState, PROTOCOL_PARAMETERS_DEFAULT } from "./emulator.ts";

const rand = new random.Random();

const bootUtxoHash = "ebcee8dcdbd7312f5e04a0033472465003617abe9935a6e56f007961897cfabb";
const bootUtxoIx = 1;

function settingsDatum(poolStakeHash: string, userPkh: string): string {
  const datum: types.SettingsDatum = {
    settingsAdmin: {
      Signature: { keyHash: userPkh},
    },
    metadataAdmin: {
      paymentCredential: {
        VerificationKeyCredential: [userPkh],
      },
      stakeCredential: null,
    },
    treasuryAdmin: {
      Signature: { keyHash: userPkh},
    },
    treasuryAddress: {
      paymentCredential: {
        VerificationKeyCredential: [userPkh],
      },
      stakeCredential: null,
    },
    treasuryAllowance: [1n, 10n],
    authorizedScoopers: [
      userPkh,
    ],
    authorizedStakingKeys: [
      {
        ScriptCredential: [poolStakeHash],
      }
    ],
    baseFee: 1000000n,
    simpleFee: 100000n,
    strategyFee: 200000n,
    poolCreationFee: 0n,
    extensions: Data.void(),
  };
  console.log(datum);
  console.log(Data.from(Data.to(datum, types.SettingsDatum), types.SettingsDatum));                    
  return Data.to(datum, types.SettingsDatum).replaceAll("43" + Data.void() + "ff", Data.void() + "ff");
}

function settingsMintRedeemer() { return "d87980" };

async function bootSettings(lucid: Lucid, scripts: Scripts, userPkh: string, inputs: Utxo[]): Promise<string> {
  const newSettingsDatum = settingsDatum(scripts.poolStakeHash, userPkh);
  console.log(inputs);
  const tx = await lucid.newTx()
    .collectFrom(inputs)
    .mint({
      [toUnit(scripts.settingsScriptHash, fromText("settings"))]: 1n,
    }, settingsMintRedeemer())
    .attachScript(scripts.settingsValidator)
    .payToContract(scripts.settingsAddress.address, { Inline: newSettingsDatum }, {
      "lovelace": 2_000_000n,
      [toUnit(scripts.settingsScriptHash, fromText("settings"))]: 1n,
    })
    .withChangeTo({ address: userAddress.address })
    .withoutCoinSelection().commit();
  const signedTx = await tx.sign().commit();
  return signedTx.submit();
}

async function listOrder(lucid: Lucid, scripts: Scripts, userPkh: string, assets: CoinPair, gives: bigint, scooperFee: bigint, changeUtxo: Utxo, count: bigint, targetPoolId: string | null): Promise<{ listedHash: string, utxos: Utxo[] }> {
  const rider = 2_000_000n;
  const tx = lucid.newTx();
  tx.collectFrom([changeUtxo]);
  for (let i = 0n; i < count; i++) {
    const flip = flags.random && rand.pick(true, false);
    if (flip) {
      const [tmpPolicyId, tmpTokenName] = assets[0];
      assets[0][0] = assets[1][0];
      assets[0][1] = assets[1][1];
      assets[1][0] = tmpPolicyId;
      assets[1][1] = tmpTokenName;
    }
    const newOrderDatum: types.OrderDatum = {
      poolIdent: targetPoolId,
      owner: {
        Signature: { keyHash: userPkh},
      },
      scooperFee: scooperFee,
      destination: {
        Fixed: {
          address: {
            paymentCredential: {
              VerificationKeyCredential: [userPkh],
            },
            stakeCredential: null,
          },
          datum: "NoDatum",
        },
      },
      order: {
        Swap: {
          offer: [assets[0][0], assets[0][1], gives],
          minReceived: [assets[1][0], assets[1][1], 0n],
        },
      },
      extension: "NoExtension",
    };
    const value: Assets = {
      "lovelace": rider + scooperFee,
    };
    if (assets[0][0] == "") {
      value["lovelace"] += gives;
    } else {
      value[assets[0][0] + assets[0][1]] = gives;
    }
    console.log("value out: ");
    console.log(value);
    tx.payToContract(scripts.orderAddress.address, { Inline: Data.to(newOrderDatum, types.OrderDatum) }, value);
  }
  const completed = await tx.withChangeTo({ address: userAddress.address }).withoutCoinSelection().commit();
  const signedTx = await completed.sign().commit();
  const signedStr = await signedTx.toString();
  console.log("signed tx for listOrder: " + signedStr);
  const listedHash = await signedTx.submit();
  await emulator.awaitTx(listedHash);
  const refs = [];
  for (let i = 0; i < count; i++) {
    refs.push({
      txHash: listedHash,
      outputIndex: i
    });
  }
  const listedUtxos = await emulator.getUtxosByOutRef(refs);
  return {
    listedHash: listedHash,
    utxos: listedUtxos
  };
}

async function cancelOrder(lucid: Lucid, scripts: Scripts, userAddress: AddressDetails, orderUtxo: Utxo, changeUtxo: Utxo): Promise<string> {
  const _rider = 2_000_000n;
  const tx = lucid.newTx();
  tx.collectFrom([changeUtxo]);
  console.log("Cancel redeemer: ");
  console.log(Data.to("Cancel", types.OrderRedeemer));
  tx.collectFrom([orderUtxo], Data.to("Cancel", types.OrderRedeemer));
  tx.attachScript(scripts.orderValidator);
  tx.addSigner(userAddress.payment!.hash);
  const completed = await tx.withChangeTo({ address: userAddress.address }).withoutCoinSelection().commit();
  const signedTx = await completed.sign().commit();
  return signedTx.submit();
}

function fakeAddress(network: Network): [AddressDetails, string, string] {
  const userPrivateKey = "ed25519_sk1zxsfsl8ehspny4750jeydt5she7dzstrj7za5vgxl6929kr9d33quqkgp3";
  const keyDetails = Crypto.privateKeyToDetails(userPrivateKey);

  const address_string = Addresses.credentialToAddress(network, keyDetails.credential, keyDetails.credential);

  const address: AddressDetails = Addresses.inspect(address_string);

  return [address, keyDetails.credential.hash, userPrivateKey];
}
interface Scripts {
  poolValidator: Script;
  poolScriptHash: string;
  poolAddress: AddressDetails;
  settingsValidator: Script;
  settingsScriptHash: string;
  settingsAddress: AddressDetails;
  orderValidator: Script;
  orderScriptHash: string;
  orderAddress: AddressDetails;
  steakValidator: Script;
  steakScriptHash: string;
  steakAddress: AddressDetails;
  poolMint: Script;
  poolPolicyId: string;
  poolStakeHash: string;
  poolStakeAddress: AddressDetails;
};

function bytesToScript(bytes: string) {
  return { type: "PlutusV2", script: bytes };
}

// deno-lint-ignore no-explicit-any
function getScriptsAiken(json: any, protocol_boot_utxo: types.OutputReference, network: Network): Scripts {
  const validator = json["validators"];
  // deno-lint-ignore no-explicit-any
  const out: any = {};

  console.log("protocol_boot_utxo: ");
  console.log(Data.to(protocol_boot_utxo, types.OutputReference));

  // deno-lint-ignore no-explicit-any
  const settingsSpendValidator = validator.find((v: any) => v.title == "settings.spend");
  out.settingsValidator = bytesToScript(applyParamsToScript([Data.to(protocol_boot_utxo, types.OutputReference)], settingsSpendValidator.compiledCode));
  out.settingsAddress = Addresses.inspect(Addresses.scriptToAddress(network, out.settingsValidator));
  out.settingsScriptHash = out.settingsAddress.payment!.hash;

  // deno-lint-ignore no-explicit-any
  const settingsMintValidator = validator.find((v: any) => v.title == "settings.mint");
  out.settingsMint = bytesToScript(applyParamsToScript([Data.to(protocol_boot_utxo, types.OutputReference)], settingsMintValidator.compiledCode));
  out.settingsPolicyId = Addresses.inspect(Addresses.scriptToAddress(network, out.settingsMint)).payment!.hash;

  const poolStakeValidator = validator.find((v: any) => v.title == "pool_stake.stake");
  out.poolStakeValidator = bytesToScript(applyParamsToScript([Data.to(out.settingsPolicyId)], poolStakeValidator.compiledCode));
  out.poolStakeAddress = Addresses.inspect(Addresses.scriptToAddress(network, out.poolStakeValidator));
  out.poolStakeHash = out.poolStakeAddress.payment!.hash;

  const poolSpendValidator = validator.find((v: any) => v.title == "pool.spend");
  out.poolValidator = bytesToScript(applyParamsToScript([Data.to(out.poolStakeHash),Data.to(out.settingsPolicyId)], poolSpendValidator.compiledCode));
  out.poolAddress = Addresses.inspect(Addresses.scriptToAddress(network, out.poolValidator));
  out.poolScriptHash = out.poolAddress.payment!.hash;

  const poolMintValidator = validator.find((v: any) => v.title == "pool.mint");
  out.poolMint = bytesToScript(applyParamsToScript([Data.to(out.poolStakeHash), Data.to(out.settingsPolicyId)], poolMintValidator.compiledCode));
  out.poolPolicyId = Addresses.inspect(Addresses.scriptToAddress(network, out.poolMint)).payment!.hash;

  const steakValidator = validator.find((v: any) => v.title == "stake.stake");
  out.steakValidator = bytesToScript(applyParamsToScript([Data.to(out.poolScriptHash)], steakValidator.compiledCode));
  out.steakAddress = Addresses.inspect(Addresses.scriptToAddress(network, out.steakValidator));
  out.steakScriptHash = out.steakAddress.payment!.hash;

  const orderValidator = validator.find((v: any) => v.title == "order.spend");
  out.orderValidator = bytesToScript(applyParamsToScript([Data.to(out.steakScriptHash)], orderValidator.compiledCode));
  out.orderAddress = Addresses.inspect(Addresses.scriptToAddress(network, out.orderValidator));
  out.orderScriptHash = out.orderAddress.payment!.hash;

  return out;
}

async function testSettingsBoot(lucid: Lucid, emulator: EmulatorWithState, scripts: Scripts) {
  const [_userAddress, userPkh, userPrivateKey] = fakeAddress(lucid.network);

  const bootUtxo = (await emulator.getUtxosByOutRef([{
    txHash: bootUtxoHash,
    outputIndex: bootUtxoIx,
  }]))[0];

  console.log(bootUtxo);

  lucid.selectWalletFromPrivateKey(userPrivateKey);
  const bootedHash = await bootSettings(lucid, scripts, userPkh, [bootUtxo]);
  console.log("bootedHash: " + bootedHash);
  await emulator.awaitTx(bootedHash);
  return bootedHash;
}

// deno-lint-ignore no-unused-vars
async function realSettingsBoot(scripts: Scripts, privateKeyFile: string) {
  const sk = await Deno.readTextFile(privateKeyFile);
  const skCborHex = JSON.parse(sk).cborHex;
  const keyDetails = Crypto.privateKeyToDetails(skCborHex);
  const userPublicKey = keyDetails.publicKey;
  const userPkh = keyDetails.credential.hash;
  const userAddress =  keyDetails.credential;

  console.log("private key: " + skCborHex);
  console.log("public key: " + userPublicKey);
  console.log("public key hash: " + userPkh);
  console.log("address: " + userAddress);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = new Lucid({provider: blockfrost, network: "Preview" });

  lucid.selectWalletFromPrivateKey(keyDetails.privateKey);

  const walletUtxos = await lucid.wallet.getUtxos();
  let toSpend = undefined;
  for (const utxo of walletUtxos) {
    if (utxo.txHash == bootUtxoHash && utxo.outputIndex == bootUtxoIx) {
      toSpend = utxo;
      break;
    }
  }

  if (toSpend == undefined) {
    throw new Error("the boot utxo was not found in the wallet");
  }

  const bootedHash = await bootSettings(lucid, scripts, userPkh, [toSpend]);
  console.log("bootedHash: " + bootedHash);
}

// deno-lint-ignore no-unused-vars
async function doCancelOrder(scripts: Scripts, privateKeyFile: string, order: string, change: string) {
  const sk = await Deno.readTextFile(privateKeyFile);
  const skCborHex = JSON.parse(sk).cborHex;
  const keyDetails = Crypto.privateKeyToDetails(skCborHex);
  const userPublicKey = keyDetails.publicKey;
  const userPkh = keyDetails.credential.hash;
  const userAddress = Addresses.inspect(Addresses.credentialToAddress("Preview", keyDetails.credential, keyDetails.credential));

  console.log("private key: " + skCborHex);
  console.log("public key: " + userPublicKey);
  console.log("public key hash: " + userPkh);
  console.log("address: " + userAddress);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId);
  const lucid = new Lucid({provider: blockfrost, network: "Preview" });

  lucid.selectWalletFromPrivateKey(skCborHex);

  const [orderHash, orderIx] = order.split("#");
  const [orderUtxo] = await blockfrost.getUtxosByOutRef([{
    txHash: orderHash,
    outputIndex: Number(orderIx),
  }]);

  const [changeHash, changeIx] = change.split("#");
  const [changeUtxo] = await blockfrost.getUtxosByOutRef([{
    txHash: changeHash,
    outputIndex: Number(changeIx),
  }]);

  const canceledHash = await cancelOrder(
    lucid,
    scripts,
    userAddress,
    orderUtxo,
    changeUtxo
  );
  console.log("canceledHash: " + canceledHash);
}

async function testListOrder(lucid: Lucid, emulator: EmulatorWithState, scripts: Scripts, coinA: string, coinB: string, change: Utxo, poolIdent: string, orderCount: bigint) {
  const assets: CoinPair = [
    assetFromString(coinA),
    assetFromString(coinB),
  ];

  let targetPoolId = null;
  if (poolIdent) {
    if (poolIdent.length == 56) {
      targetPoolId = poolIdent;
    } else {
      throw new Error("Malformed pool ident");
    }
  }

  const listed = await listOrder(
    lucid,
    scripts,
    userPkh,
    assets,
    1_000_000n,
    1_100_000n,
    change,
    orderCount,
    targetPoolId
  );
  console.log("listedHash: " + listed.listedHash);
  await emulator.awaitTx(listed.listedHash);
  return listed;
}

function computePoolId(utxo: Utxo) {
  const poolInputTxHash = fromHex(utxo.txHash);
  const numberSign = new Uint8Array([0x23]);
  const poolInputTxIx = new Uint8Array([utxo.outputIndex]); // ident encoding for output index 1
  const poolInputRef = concat([poolInputTxHash, numberSign, poolInputTxIx]);
  return fromHex(Hasher.hashWithBlake2b256(toHex(poolInputRef))).slice(4); // Truncate first four bytes
}

function computePoolNftName(poolId: Uint8Array) {
  const prefix = new Uint8Array([0x00, 0x0d, 0xe1, 0x40]);
  return toHex(concat([prefix, poolId]));
}

function computePoolLqName(poolId: Uint8Array) {
  const prefix = new Uint8Array([0x00, 0x14, 0xdf, 0x10]);
  return toHex(concat([prefix, poolId]));
}

function computePoolRefName(poolId: Uint8Array) {
  const prefix = new Uint8Array([0x00, 0x06, 0x43, 0xb0]);
  return toHex(concat([prefix, poolId]));
}

type Asset = [string, string];
type CoinPair = [Asset, Asset];

function isqrt(n: bigint): bigint {
  if (n < 0n) {
    throw new Error('isqrt: negative input');
  }

  if (n < 2n) {
    return n;
  }

  function go(m: bigint, x0: bigint): bigint {
    const x1 = ((m / x0) + x0) / 2n;
    if (x0 == x1 || x0 == (x1 - 1n)) {
      return x0;
    }
    return go(m, x1);
  }

  return go(n, 1n);
}

function initialLiquidity(coinA: bigint, coinB: bigint): bigint {
  return isqrt(coinA * coinB);
}

function getRberryPolicyId(): [Script, string] {
  const rberryMintingPolicyNative: NativeScript = { "type": "All", "scripts": [] };
  const rberryMintingPolicy: Script = { "type": "Native", "script": Codec.encodeNativeScript(rberryMintingPolicyNative) };
  return [rberryMintingPolicy, Hasher.hashScript(rberryMintingPolicy)];
}

async function postReferenceScript(scripts: Scripts, lucid: Lucid, userAddress: AddressDetails, scriptName: string, changeUtxo: Utxo, settings: Utxo): Promise<string> {
  if (scriptName in scripts) {
    let signedTx;
    const retry = true;
    let nonce = 0n;
    while (retry) {
      const tx = await lucid.newTx()
        .collectFrom([changeUtxo])
        .payToWithData(userAddress.address, {
          scriptRef: scripts[scriptName as keyof Scripts] as Script,
        }, {
          "lovelace": 2_000_000n,
        })
        .payTo(userAddress.address, { "lovelace": 2_000_000n + nonce })
        .withoutCoinSelection()
        .withChangeTo({ address: userAddress.address })
        .commit();
      signedTx = await tx.sign().commit();
      const hash = signedTx.toHash();
      if (hash > settings.txHash) {
        break;
      }
      nonce += 1n;
    }
    console.log("post reference script: ", signedTx?.toString());
    if (signedTx == undefined) {
      throw new Error("post reference script failed");
    } else {
      return signedTx.submit();
    }
  } else {
    throw new Error("script does not exist: " + scriptName);
  }
}

async function mintRberry(lucid: Lucid, userAddress: AddressDetails): Promise<string> {
  const [rberryMintingPolicy, rberryPolicyId]: [Script, string] = await getRberryPolicyId();
  const tx = await lucid.newTx()
    .mint({
      [toUnit(rberryPolicyId, fromText("RBERRY"))]: 10_000_000_000n,
    })
    .attachScript(rberryMintingPolicy)
    .payTo(userAddress.address, {
      "lovelace": 2_000_000n,
      [toUnit(rberryPolicyId, fromText("RBERRY"))]: 10_000_000_000n,
    })
    .withChangeTo({ address: userAddress.address })
    .commit();
  console.log("mintRberry: ", tx.toString());
  const signedTx = await tx.sign().commit();
  return signedTx.submit();
}

async function mintPool(scripts: Scripts, lucid: Lucid, userAddress: AddressDetails, settings: Utxo, references: Utxo[], assets: CoinPair, seed: Utxo, amountA: bigint, amountB: bigint, fees: [bigint,bigint], marketOpen?: bigint): Promise<{ poolMintedHash: string, poolId: string }> {
  const poolId = computePoolId(seed);
  const liq = initialLiquidity(amountA, amountB);
  const newPoolDatum: types.PoolDatum = {
    identifier: toHex(poolId),
    assets: assets,
    circulatingLp: liq,
    bidFeesPer10Thousand: fees,
    askFeesPer10Thousand: fees,
    feeManager: null,
    marketOpen: marketOpen || 0n,
    protocolFees: 2_000_000n,
  };
  const poolMintRedeemer: types.PoolMintRedeemer = {
    CreatePool: {
      assets: assets,
      poolOutput: 0n,
      metadataOutput: 2n,
    }
  };
  const poolNftNameHex = computePoolNftName(poolId);
  const poolLqNameHex = computePoolLqName(poolId);
  const poolRefNameHex = computePoolRefName(poolId);
  const poolValue = {
    [toUnit(scripts.poolPolicyId, poolNftNameHex)]: 1n,
    [toUnit(assets[1][0], assets[1][1])]: amountB,
  };
  if (assets[0][0] == "") {
    poolValue["lovelace"] = amountA + 2_000_000n;
  } else {
    poolValue[toUnit(assets[0][0], assets[0][1])] = amountA;
  }

  const poolMintRedeemerBytes = Data.to(poolMintRedeemer, types.PoolMintRedeemer);
  const _poolMintTest = Data.from(poolMintRedeemerBytes, types.PoolMintRedeemer);
  console.log("poolMintTest: ");
  console.log(_poolMintTest);
  const poolDatumBytes = Data.to(newPoolDatum, types.PoolDatum);
  const poolDatumTest = Data.from(poolDatumBytes, types.PoolDatum);
  console.log("poolDatumTest: ");
  console.log(poolDatumTest);

  const poolAddress = Addresses.credentialToAddress(lucid.network,
    {
      type: "Script",
      hash: scripts.poolScriptHash,
    },
    {
      type: "Script",
      hash: scripts.poolStakeHash,
    }
  );

  console.log("value: ");
  console.log(poolValue);
  console.log("newPoolDatum: ");
  console.log(poolDatumBytes);
  console.log("mint redeemer: ");
  console.log(poolMintRedeemerBytes);
  console.log("settings datum: ");
  console.log(settings.datum);
  console.log("pool address: ");
  console.log(poolAddress);
  console.log("-------");
  console.log("seed: ", seed);
  const tx = lucid.newTx()
    .mint({
      [toUnit(scripts.poolPolicyId, poolNftNameHex)]: 1n,
      [toUnit(scripts.poolPolicyId, poolRefNameHex)]: 1n,
      [toUnit(scripts.poolPolicyId, poolLqNameHex)]: liq,
    }, poolMintRedeemerBytes)
    .readFrom([...references, settings])
    .collectFrom([seed])
    .payToContract(poolAddress, { Inline: poolDatumBytes }, poolValue)
    .payTo(userAddress.address, {
      "lovelace": 2_000_000n,
      [toUnit(scripts.poolPolicyId, poolLqNameHex)]: liq,
    })
    .payToWithData(
      userAddress.address,
      { Inline: "d87980" },
      {
        "lovelace": 2_000_000n,
        [toUnit(scripts.poolPolicyId, poolRefNameHex)]: 1n,
      }
    )
    .withChangeTo({ address: userAddress.address });

  const str = await tx.toString();
  console.log("building tx: " + str);
  const completed = await tx.withoutCoinSelection().commit();
  const signedTx = await completed.sign().commit();
  const signedStr = await signedTx.toString()
  console.log("signed tx: " + signedStr);
  const hash = await signedTx.submit();
  return {
    poolMintedHash: hash,
    poolId: toHex(poolId),
  };
}

function assetFromString(s: string): Asset {
  if (s == "lovelace") {
    return ["", ""];
  }
  const chunks = s.split(".");
  if (chunks.length != 2) {
    throw new Error("Asset format is wrong: " + s);
  }
  return [chunks[0], chunks[1]];
}

//async function testMakePoolFunds(lucid: Lucid, emulator: Emulator, scripts: Scripts, coinA: string, coinAAmount: bigint, coinB: string, coinBAmount: bigint) {
//  const dummy = await Lucid.new(undefined, "Custom");
//  const [userAddress, userPkh, userPrivateKey] = fakeAddress(dummy);
//
//  const change = await findChange(emulator, userAddress);
//  let ok = false;
//  let nonce = 0n;
//  while (!ok) {
//    let tx = lucid.newTx()
//      .collectFrom([change]);
//    if (coinA == "lovelace") {
//      tx = tx.payToAddress(userAddress, {
//        [coinA]: coinAAmount + nonce,
//        [coinB]: coinBAmount,
//      });
//    } else {
//      tx = tx.payToAddress(userAddress, {
//        "lovelace": 2_000_000n + nonce,
//        [coinA]: coinAAmount,
//        [coinB]: coinBAmount,
//      });
//    }
//    const completed = await tx.complete({
//      coinSelection: false,
//    });
//    const hash = completed.toHash();
//    if (hash.startsWith('00')) {
//      ok = true;
//    }
//  }
//
//}

async function testMintPool(lucid: Lucid, emulator: EmulatorWithState, scripts: Scripts, coinA: string, coinB: string, refUtxo: Utxo, seed: Utxo) {
  const [userAddress, _userPkh, _userPrivateKey] = fakeAddress(lucid.network);

  const assets: CoinPair = [
    assetFromString(coinA),
    assetFromString(coinB),
  ];

  const settingsUtxos = await emulator.getUtxos(scripts.settingsAddress.payment!);

  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }

  const settings = settingsUtxos[0];

  const minted = await mintPool(scripts, lucid, userAddress, settings, [refUtxo], assets, seed, 1_000_000_000n, 1_000_000_000n, [5n, 5n]);
  await emulator.awaitTx(minted.poolMintedHash);
  console.log("Minted a pool, hash: " + minted.poolMintedHash);
  return minted;
}

function computeIndexingSet(scripts: Scripts, changeUtxo: Utxo, targetPool: Utxo, orderUtxos: Utxo[]): [bigint, null, bigint][] {
  const toSpend = [];
  toSpend.push(changeUtxo);
  toSpend.push(targetPool);
  toSpend.push(...orderUtxos);
  toSpend.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));
  const indexingSet: [bigint, null, bigint][] = [];
  for (let i = 0n; i < toSpend.length; i++) {
    const e = toSpend[Number(i)];
    if (e.address == scripts.orderAddress.address) {
      indexingSet.push([i, null, 0n]);
    }
  }
  return indexingSet;
}

function getPoolABL(targetPool: Utxo, datum: types.PoolDatum): ABL {
  let poolCoinA = datum.assets[0][0] + datum.assets[0][1];
  const poolCoinB = datum.assets[1][0] + datum.assets[1][1];
  if (datum.assets[0][0] == "") {
    poolCoinA = "lovelace";
  }

  let poolAmountA = targetPool.assets[poolCoinA];
  if (poolCoinA == "lovelace") {
    poolAmountA -= datum.protocolFees;
  }

  const poolAmountB = targetPool.assets[poolCoinB];

  console.log("getPoolABL: poolAmountA: ");
  console.log(poolAmountA);

  const abl: ABL = {
    a: poolAmountA,
    b: poolAmountB,
    liq: datum.circulatingLp,
  };
  return abl;
}

type EscrowTakes = {
  abl: ABL,
  destination: AddressDetails,
};

// deno-lint-ignore no-explicit-any
function fromOrderDatumAddress(addr: any): AddressDetails {
  let paymentCred: Credential | null = null;
  if (addr.Fixed.address.paymentCredential.VKeyCredential) {
    paymentCred = { type: "Key", hash: addr.Fixed.address.paymentCredential.VKeyCredential.bytes };
  } else if (addr.Fixed.address.paymentCredential.SCredential) {
    paymentCred = { type: "Script", hash: addr.Fixed.address.paymentCredential.SCredential.bytes };
  } else {
    throw new Error("couldn't convert order datum address for lucid");
  }

  let stakingCred: Credential | null = null;
  if (addr.Fixed.address.stakeCredential == null) {
    // That's ok 
  } else if (addr.Fixed.address.stakeCredential.VKeyCredential) {
    stakingCred = { type: "Key", hash: addr.Fixed.address.stakeCredential.VKeyCredential.bytes };
  } else if (addr.Fixed.address.stakeCredential.SCredential) {
    stakingCred = { type: "Script", hash: addr.Fixed.address.stakeCredential.SCredential.bytes };
  } else {
    throw new Error("couldn't convert order datum address for lucid: stake cred invalid");
  }
  console.log("debug: ");
  console.log(paymentCred);
  console.log(stakingCred);
  if (stakingCred) {
    return Addresses.inspect(Addresses.credentialToAddress("Preview", paymentCred, stakingCred));
  } else {
    return Addresses.inspect(Addresses.credentialToAddress("Preview", paymentCred));
  }
}

async function executeOrder(poolABL: ABL, poolDatum: types.PoolDatum, order: Utxo): Promise<[ABL, EscrowTakes]> {
  console.log("poolABL is: ");
  console.log(poolABL);
  const poolCoinA = poolDatum.assets[0][0] + poolDatum.assets[0][1];
  const poolCoinB = poolDatum.assets[1][0] + poolDatum.assets[1][1];
  if (!order.datum) {
    throw new Error("executeOrder: order utxo had no datum");
  }
  const orderDatum: types.OrderDatum = Data.from(order.datum, types.OrderDatum);
  let res: ABL = { a: 0n, b: 0n, liq: 0n };
  if ("Swap" in orderDatum.order) {
    if (orderDatum.order.Swap.offer[0] + orderDatum.order.Swap.offer[1] == poolCoinA) {
      [res, poolABL] = doSwap(Coin.CoinA, orderDatum.order.Swap.offer[2], poolDatum.bidFeesPer10Thousand, poolABL);
      console.log("after swapping for coinA, poolABL will be: ");
      console.log(poolABL);
    } else if (orderDatum.order.Swap.offer[0] + orderDatum.order.Swap.offer[1] == poolCoinB) {
      [res, poolABL] = doSwap(Coin.CoinB, orderDatum.order.Swap.offer[2], poolDatum.askFeesPer10Thousand, poolABL);
    } else {
      throw new Error("Order does not appear to match the pool");
    }
  }
  const dest = await fromOrderDatumAddress(orderDatum.destination);
  return [poolABL, {
    abl: res,
    destination: dest,
  }];
}

async function updateABL(poolABL: ABL, poolDatum: types.PoolDatum, orders: Utxo[]): Promise<[ABL, EscrowTakes[]]> {
  orders.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));
  const takes: EscrowTakes[] = [];
  let currentPoolABL: ABL = {
    a: poolABL.a,
    b: poolABL.b,
    liq: poolABL.liq,
  };
  let take = null;
  for (const o of orders) {
    [currentPoolABL, take] = await executeOrder(currentPoolABL, poolDatum, o);
    takes.push(take);
  }
  return [currentPoolABL, takes];
}

async function scoopPool(scripts: Scripts, lucid: Lucid, userAddress: AddressDetails, settings: Utxo, orderUtxos: Utxo[], targetPool: Utxo, references: Utxo[], changeUtxo: Utxo): Promise<string> {
  console.log("settings: ");
  console.log(settings);

  console.log("targetPool: ");
  console.log(targetPool);

  const settingsDatum = Data.from(settings.datum!, types.SettingsDatum);
  const protocolBaseFee = settingsDatum.baseFee;
  const simpleFee = settingsDatum.simpleFee;
  const ordersCount: bigint = BigInt(orderUtxos.length);
  const rider = 2_000_000n;
  const indexingSet = computeIndexingSet(scripts, changeUtxo, targetPool, orderUtxos);
  console.log("indexingSet: ");
  console.log(indexingSet);
  const amortizedBaseFee = (protocolBaseFee + ordersCount - 1n) / ordersCount;
  const scoopPoolRedeemer: types.PoolRedeemer = { Spend: {PoolScoop: {
    signatoryIndex: 0n,
    scooperIndex: 0n,
    inputOrder: indexingSet,
  }}};
  let redeemerData = Data.to(scoopPoolRedeemer, types.PoolRedeemer);
  redeemerData = "d87a9f" + redeemerData + "ff"; // Have to do redeemer wrapper trick here
  console.log("pool redeemer: ", redeemerData);
  const orderScoopRedeemer: types.OrderRedeemer = "Scoop";
  console.log("order redeemer: ");
  console.log(Data.to(orderScoopRedeemer, types.OrderRedeemer));
  if (!targetPool.datum) {
    throw new Error("Don't have a datum for the target pool.");
  }
  console.log("old pool datum: ");
  console.log(targetPool.datum);
  const poolDatum = Data.from(targetPool.datum, types.PoolDatum);
  const poolCoinA = poolDatum.assets[0][0] + poolDatum.assets[0][1];
  const poolCoinB = poolDatum.assets[1][0] + poolDatum.assets[1][1];
  console.log("pool coins: ", poolCoinA, poolCoinB);
  const poolABL = getPoolABL(targetPool, poolDatum);
  const [newPoolABL, escrowTakes] = await updateABL(poolABL, poolDatum, orderUtxos);
  console.log("newPoolABL: ");
  console.log(newPoolABL);
  const totalProtocolFees = (amortizedBaseFee + simpleFee) * ordersCount;
  console.log(`total protocol fees: ${totalProtocolFees}`);
  const mintedLiq = newPoolABL.liq - poolABL.liq;
  console.log("mintedLiq: ");
  console.log(mintedLiq);
  const _oldRewards = poolDatum.protocolFees;
  poolDatum.protocolFees += totalProtocolFees;
  poolDatum.circulatingLp = newPoolABL.liq;
  const poolNftNameHex = computePoolNftName(fromHex(poolDatum.identifier));
  const poolLqNameHex = computePoolLqName(fromHex(poolDatum.identifier));
  const tx = await lucid.newTx();
  const toSpend = [];
  toSpend.push(changeUtxo);
  toSpend.push(targetPool);
  toSpend.push(...orderUtxos);
  toSpend.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));
  for (const e of toSpend) {
    if (Addresses.inspect(e.address).payment!.hash == scripts.poolScriptHash) {
      tx.collectFrom([e], redeemerData);
    } else if (Addresses.inspect(e.address).payment!.hash == scripts.orderScriptHash) {
      tx.collectFrom([e], Data.to(orderScoopRedeemer, types.OrderRedeemer));
    } else {
      tx.collectFrom([e]);
    }
  }

  const currentTime = emulator.now();
  console.log("new pool datum: ");
  const newPoolDatum = Data.to(poolDatum, types.PoolDatum);
  console.log(newPoolDatum);
  console.log("references: ");
  console.log(references);
  console.log("current time: ");
  console.log(currentTime);
  tx
    .validFrom(currentTime - 10000)
    .validTo(currentTime + 1000000)
    .readFrom([settings, ...references])
    // Reference utxos should carry scriptRefs for these scripts
    // .attachSpendingValidator(scripts.orderValidator)
    // .attachSpendingValidator(scripts.poolValidator)
    .attachScript(scripts.steakValidator)
    .addSigner(userAddress.payment!.hash)
    .withdraw(scripts.steakAddress.address, 0n, "00")
    .payToContract(targetPool.address, { Inline: newPoolDatum }, {
      "lovelace":
        newPoolABL.a +
        poolDatum.protocolFees,
      [poolCoinB]: newPoolABL.b,
      [toUnit(scripts.poolPolicyId, poolNftNameHex)]: 1n,
    });

  if (mintedLiq != 0n) {
    tx.attachScript(scripts.poolMint);
    const poolMintRedeemer: types.PoolMintRedeemer = {
      MintLP: {
        identifier: poolDatum.identifier,
      }
    };
    console.log("poolMintRedeemer: ");
    console.log(poolMintRedeemer);
    tx.mint({
      [toUnit(scripts.poolPolicyId, poolLqNameHex)]: mintedLiq,
    }, Data.to(poolMintRedeemer, types.PoolMintRedeemer));
  }

  console.log("pool out datum: ");
  console.log(Data.to(poolDatum, types.PoolDatum));

  // We add the escrows to the order in reverse, because in the script, prepending to the list is cheaper
  for (const e of escrowTakes) {
    const valueOut: Assets = { "lovelace": rider + (1_000_000n - amortizedBaseFee) + e.abl.a };
    if (e.abl.b > 0n) {
      valueOut[poolCoinB] = e.abl.b;
    }
    if (e.abl.liq > 0n) {
      valueOut[toUnit(scripts.poolPolicyId, poolLqNameHex)] = e.abl.liq;
    }
    tx.payTo(e.destination.address, valueOut);
  }
  const str = await tx.toString();
  console.log("building tx: " + str);
  const completed = await tx.withoutCoinSelection().commit();
  const signedTx = await completed.sign().commit();
  const signedStr = await signedTx.toString();
  console.log("signed tx: " + signedStr);
  signedTx.submit();
  await emulator.awaitTx(signedTx.tx);
  return signedTx.tx;
}

async function testScoopPool(lucid: Lucid, emulator: EmulatorWithState, scripts: Scripts, poolIdentHex: string, change: Utxo, references: Utxo[], orders: Utxo[]) {
  const [userAddress, _userPkh, _userPrivateKey] = fakeAddress(lucid.network);

  const settingsUtxos = await emulator.getUtxos(scripts.settingsAddress.address);
  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }
  const settings = settingsUtxos[0];

  const poolAddress = Addresses.credentialToAddress(emulator.network!,
    {
      type: "Script",
      hash: scripts.poolScriptHash,
    },
    {
      type: "Script",
      hash: scripts.poolStakeHash,
    }
  );

  const knownPools = await emulator.getUtxos(poolAddress);

  let targetPool = null;
  for (const knownPool of knownPools) {
    const targetAssetName = computePoolNftName(fromHex(poolIdentHex));
    const targetPolicyId = scripts.poolScriptHash;
    const targetNftUnit = targetPolicyId + targetAssetName;
    const amountOfTargetNft = knownPool.assets[targetNftUnit];
    if (amountOfTargetNft == 1n) {
      targetPool = knownPool;
    } else if (amountOfTargetNft > 1n) {
      throw new Error("Impossible: Multiple copies of pool NFT found in UTXO: " + JSON.stringify(knownPool));
    }
  }
  if (targetPool == null) {
    throw new Error("Can't find a pool UTXO containing the NFT for the ident: " + poolIdentHex);
  }
  const exUnits = await scoopPool(scripts, lucid, userAddress, settings, orders, targetPool, references, change);
  return exUnits;
}

async function testMintRberry(lucid: Lucid, emulator: EmulatorWithState) {
  const [userAddress, _userPkh, _userPrivateKey] = fakeAddress(lucid.network);
  const mintedHash = await mintRberry(lucid, userAddress);
  await emulator.awaitTx(mintedHash);
  console.log("minted rberry, hash: " + mintedHash);
  const mintedUtxos = await emulator.getUtxosByOutRef([{
    txHash: mintedHash,
    outputIndex: 0,
  }]);
  return mintedUtxos[0];
}

//async function fundUserAddress(lucid: Lucid, emulator: Emulator, scripts: Scripts) {
  // const dummy = await 
//}

async function testPostReferenceScript(lucid: Lucid, emulator: EmulatorWithState, scripts: Scripts, scriptName: string) {
  const [userAddress, _userPkh, _userPrivateKey] = fakeAddress(lucid.network);
  const change = await findChange(emulator, userAddress.address);

  const settingsUtxos = await emulator.getUtxos(scripts.settingsAddress.address);

  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }

  const settings = settingsUtxos[0];


  const postedHash = await postReferenceScript(scripts, lucid, userAddress, scriptName, change, settings);
  await emulator.awaitTx(postedHash);
  console.log("Posted reference script, hash: " + postedHash);
  const postedUtxos = await emulator.getUtxosByOutRef([{
    txHash: postedHash,
    outputIndex: 0,
  }]);
  return postedUtxos[0];
}


async function findChange(provider: Provider, userAddress: string): Promise<Utxo> {
  const startTime = Date.now();
  const changeUtxos = await provider.getUtxos(userAddress);
  const endTime = Date.now();
  console.log(`Fetched utxos from wallet, time elapsed: ${endTime - startTime}ms`);
  for (const changeUtxo of changeUtxos) {
    if (changeUtxo.datum != null && changeUtxo.datumHash != null) {
      continue;
    }
    if (changeUtxo.scriptRef != null) {
      continue;
    }
    if (changeUtxo.assets["lovelace"] >= 200_000_000n) {
      console.log("changeUtxo:");
      console.log(changeUtxo);
      return changeUtxo;
    }
  }
  console.log("changeUtxos:");
  console.log(changeUtxos);
  throw new Error("findChange: Couldn't find a UTxO without a datum or script ref in the user wallet with over 200 ADA.");
}

// deno-lint-ignore no-unused-vars
async function findSettings(provider: Provider, settingsAddress: string, settingsPolicyId: string): Promise<Utxo> {
  const settingsUtxos = await provider.getUtxos(settingsAddress);
  for (const settingsUtxo of settingsUtxos) {
    // Verify that it's a real settings
    if (settingsUtxo.assets[settingsPolicyId + fromText("settings")] != 1n) {
      console.log(`settings utxo is missing the nft (${settingsPolicyId}${fromText("settings")}):`);
      console.log(settingsUtxo.assets);
      continue;
    }
    return settingsUtxo;
  }
  throw new Error("findSettings: Couldn't find a UTxO with the settings NFT at the settings address.");
}

// deno-lint-ignore no-unused-vars
async function findOrders(provider: Provider, orderAddress: string): Promise<Utxo[]> {
  const orderUtxos = await provider.getUtxos(orderAddress);
  const result: Utxo[] = [];
  for (const orderUtxo of orderUtxos) {
    let _orderDatum: types.OrderDatum | null = null;
    try {
      _orderDatum = Data.from(orderUtxo.datum as string, types.OrderDatum);
    } catch (_e) {
      console.log("findOrders: UTxO at order address had invalid datum (skipping)");
      continue;
    }
    result.push(orderUtxo);
  }
  return result;
}

const flags = parse(Deno.args, {
  string: ["scriptsFile", "privateKey", "coinA", "coinB", "blockfrostUrl", "blockfrostProjectId"],
});
if (flags.scriptsFile == undefined) {
  throw "no scripts file";
}

const example_settings_admin =
  "6313a1d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

const example_metadata_admin =
  "7b143ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"

const example_treasury_admin =
  "17bbd2d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

const example_treasury_address =
  "6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"

const testDatum: types.SettingsDatum = {
  settingsAdmin: { Signature: { keyHash: example_settings_admin} },
  metadataAdmin: { paymentCredential: { VerificationKeyCredential: [example_metadata_admin]}, stakeCredential: null},
  treasuryAdmin: { Signature: { keyHash: example_treasury_admin} },
  treasuryAddress: { paymentCredential: { VerificationKeyCredential: [example_treasury_address]}, stakeCredential: null},
  treasuryAllowance: [1n, 10n],
  authorizedScoopers: [example_settings_admin],
  authorizedStakingKeys: [{ VerificationKeyCredential: [example_settings_admin] }],
  baseFee: 0n,
  simpleFee: 2_500_000n,
  strategyFee: 5_000_000n,
  poolCreationFee: 0n,
  extensions: Data.void()
};

const testDatumCbor = Data.to(testDatum, types.SettingsDatum);
console.log("testDatumCbor: ");
console.log(testDatumCbor);

const s = await Deno.readTextFile(flags.scriptsFile);
const scriptsJson = JSON.parse(s);

let emulator = new EmulatorWithState([], {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 999999999,
});

const lucid = new Lucid({provider: emulator});

const scripts = getScriptsAiken(scriptsJson, { transactionId: { hash: bootUtxoHash }, outputIndex: BigInt(bootUtxoIx) }, lucid.network);

const [userAddress, userPkh, _userPrivateKey] = fakeAddress(lucid.network);

// const accounts: {address: string, assets: Assets, outputData?: OutputData}[] = [
//   {
//     address: userAddress.address,
//     assets: {
//       "lovelace": 1_000_000_000_000n,
//     }
//   }
// ];
// let emulator = new Emulator(accounts, {
//   ...PROTOCOL_PARAMETERS_DEFAULT,
//   maxTxSize: 999999999999,
//   maxTxExMem: 999999999999999n,
// });


emulator.pushUtxo({
    txHash: bootUtxoHash,
    outputIndex: bootUtxoIx,
    address: userAddress.address,
    assets: { lovelace: 1_000_000_000_000n },
    datumHash: undefined,
    datum: undefined,
    scriptRef: undefined
});

await testSettingsBoot(lucid, emulator, scripts);
const _mintedUtxo = await testMintRberry(lucid, emulator);
const poolMintRef = await testPostReferenceScript(lucid, emulator, scripts, "poolMint");
const poolValidatorRef = await testPostReferenceScript(lucid, emulator, scripts, "poolValidator");
const orderValidatorRef = await testPostReferenceScript(lucid, emulator, scripts, "orderValidator");
const [_rberryMintingPolicy, rberryPolicyId]: [Script, string] = await getRberryPolicyId();
const rberry = rberryPolicyId + "." + fromText("RBERRY");
//await testMakePoolFunds(lucid, emulator, scripts, "lovelace", 1_020_000_000n, rberry, 1_000_000_000n);

const fakeChange = {
  txHash: "0000000000000000000000000000000000000000000000000000000000000000",
  outputIndex: 0,
  assets: {
    lovelace: 100_000_000_000n,
    [toUnit(rberryPolicyId, fromText("RBERRY"))]: 100_000_000_000n,
  },
  address: userAddress.address,
  datumHash: undefined,
  datum: undefined,
  scriptRef: undefined
};

emulator.pushUtxo(fakeChange);

const { poolId } =
  await testMintPool(lucid, emulator, scripts, "lovelace", rberry, poolMintRef, fakeChange);

console.log("pool id: ", poolId);

const listOrdersChange = {
  txHash: "0000000000000000000000000000000000000000000000000000000000000000",
  outputIndex: 1,
  assets: {
    lovelace: 100_000_000_000n,
    [toUnit(rberryPolicyId, fromText("RBERRY"))]: 100_000_000_000n,
  },
  address: userAddress.address,
  datumHash: undefined,
  datum: undefined,
  scriptRef: undefined
};

emulator.pushUtxo(listOrdersChange);

const { utxos: orders } =
  await testListOrder(lucid, emulator, scripts, "lovelace", rberry, listOrdersChange, poolId, 40n);

const scoopPoolChange = await findChange(emulator, userAddress.address);

const savedLedger = structuredClone(emulator);
console.log("savedLedger");
console.log(savedLedger);
console.log("ok");

const runs = new Map();

for (let i = 37; i <= 45; i++) {
  emulator = structuredClone(savedLedger);
  try {
    const exUnits = await testScoopPool(lucid, emulator, scripts, poolId, scoopPoolChange, [orderValidatorRef, poolValidatorRef], orders.slice(0, i));
    runs.set(i, exUnits);
  } catch (e) {
    console.log("Failed to scoop: ", e)
  }
}

console.log("results");
console.log(runs);
