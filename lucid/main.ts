import {
  Address,
  Assets,
  Blockfrost,
  Data,
  Constr,
  Credential,
  Script,
  ScriptHash,
  PolicyId,
  Emulator,
  fromText,
  generatePrivateKey,
  paymentCredentialOf,
  getAddressDetails,
  OutRef,
  Lucid,
  toUnit,
  TxHash,
  C,
  M,
  Utils,
  fromHex,
  toHex,
  concat,
  toPublicKey,
  UTxO,
  PROTOCOL_PARAMETERS_DEFAULT,
  Provider,
} from "../../sundae-lucid/mod.ts";
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";
import { Args, parse } from "https://deno.land/std@0.184.0/flags/mod.ts";
import { ABL, Coin, SwapFees, doSwap, doDeposit, doWithdrawal } from "./cpp.ts";
import * as random from "https://deno.land/x/random@v1.1.2/Random.js";
import * as types from "./types.ts";
import { sleep } from "https://deno.land/x/sleep/mod.ts";
import { signAsync } from "https://deno.land/x/ed25519@2.1.0/mod.ts";
import { assertEquals } from "https://deno.land/std@0.193.0/testing/asserts.ts";

const rand = new random.Random();

let bootUtxoHash = "382b27b28c70343161f9abebdab78264e0fd7271baf3bb88ca04b52e5f0067ef";
let bootUtxoIx = 1;

function settingsRedeemerUpdate() { return "d87a9fd8799fffff" };

function settingsRedeemerUpdateTreasury() { return "d87a9fd87a9fffff" };

function envelope(txBytes) {
  return `{
  "type": "Witnessed Tx BabbageEra",
  "description": "Ledger Cddl Format",
  "cborHex": "${txBytes}"
}`;
}

async function updatePoolFees() {
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);

  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Preview");
  
  const scripts = getScriptsAiken(lucid, scriptsJson);

  lucid.selectWalletFrom({
    address: address,
  });

  let poolAddress;
  if (flags.poolAddress) {
    poolAddress = flags.poolAddress;
  } else {
    console.log("poolScriptHash: " + scripts.poolScriptHash);
    console.log("poolStakeHash: " + scripts.poolStakeHash);
    poolAddress = lucid.utils.credentialToAddress(
      {
        type: "Script",
        hash: scripts.poolScriptHash,
      },
      {
        type: "Script",
        hash: scripts.poolStakeHash,
      }
    );
  }
  console.log("poolAddress: " + poolAddress);
  let knownPools = await lucid.provider.getUtxos(poolAddress);
  let targetPool = null;
  for (let knownPool of knownPools) {
    let targetAssetName = computePoolNftName(fromHex(flags.poolIdent));
    let targetPolicyId = scripts.poolScriptHash;
    let targetNftUnit = targetPolicyId + targetAssetName;
    let amountOfTargetNft = knownPool.assets[targetNftUnit];
    if (amountOfTargetNft == 1n) {
      targetPool = knownPool;
    } else if (amountOfTargetNft > 1n) {
      throw new Error("Impossible: Multiple copies of pool NFT found in UTXO: " + JSON.stringify(knownPool));
    }
  }
  if (targetPool == null) {
    throw new Error("Can't find a pool UTXO containing the NFT for the ident: " + flags.poolIdent);
  }

  const settingsUtxos = await blockfrost.getUtxos(scripts.settingsAddress);
  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }
  const settings = settingsUtxos[0];
  console.log(settings.datum);
  const settingsDatum = Data.from(settings.datum, types.SettingsDatum);

  const treasuryAddress = await addressPlutusToLucid(lucid, settingsDatum.treasuryAddress);
  console.log("treasury address: " + treasuryAddress);

  let newPoolDatum = Data.from(targetPool.datum, types.PoolDatum);
  newPoolDatum.bidFeesPer10Thousand = 50n;
  newPoolDatum.askFeesPer10Thousand = 30n;

  const change = await findChange(blockfrost, address);

  // We need to figure out what index in the inputs the pool utxo will have
  let toSpend = [];
  toSpend.push(change);
  toSpend.push(targetPool);
  toSpend.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));
  let poolInputIndex = 0n;
  for (let e of toSpend) {
    if (e.address == targetPool.address) {
      break;
    }
    poolInputIndex = poolInputIndex + 1n;
  }
  console.log("toSpend: ")
  console.log(toSpend);

  let poolManageRedeemer = Data.to({
    UpdatePoolFees: {
      poolInput: 0n, 
    }
  }, types.PoolManageRedeemer);

  console.log("poolManageRedeemer: " + poolManageRedeemer);

  let poolSpendRedeemer = Data.to({
    Manage: [],
  }, types.PoolSpendRedeemer);
  poolSpendRedeemer = "d87a9f" + poolSpendRedeemer + "ff";
  console.log("poolSpendRedeemer: " + poolSpendRedeemer);

  const refs = await Deno.readTextFile(flags.references);
  const lines = refs.split(/\r?\n/);
  const refUtxosOutRefs: OutRef[] = [];
  for (let line of lines) {
    let [hash, ix] = line.split("#");
    let ixNum = Number(ix);
    if (hash == "" || isNaN(ixNum)) {
      continue;
    }
    refUtxosOutRefs.push({
      txHash: hash,
      outputIndex: Number(ix),
    });
  }
  const references = await blockfrost.getUtxosByOutRef(refUtxosOutRefs);

  const tx = lucid.newTx();
  const currentTime = Date.now();
  tx.readFrom([...references, settings]);
  tx.collectFrom([change]);
  tx.collectFrom([targetPool], poolSpendRedeemer);
  const signers = flags.signers.split(",");
  for (s of signers) {
    tx.addSignerKey(s);
  }
  tx.attachMintingPolicy(scripts.poolManageValidator);
  tx.payToContract(
    targetPool.address,
    {
      inline: Data.to(newPoolDatum, types.PoolDatum),
    },
    targetPool.assets, 
  );
  console.log("scripts.poolManageAddress: " + scripts.poolManageAddress);
  tx.withdraw(scripts.poolManageAddress, 0n, poolManageRedeemer);
  const txStr = await tx.toString();
  console.log("tentative tx: " + txStr);
  const completed = await tx.complete({
    coinSelection: false,
  });
  const completedStr = await completed.toString();
  console.log("completed tx: " + envelope(completedStr));
  const txid = completed.toHash();
  console.log("txid: " + txid);
}

async function delegatePool() {
   if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);

  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Preview");
  
  const scripts = getScriptsAiken(lucid, scriptsJson);

  const settingsUtxos = await blockfrost.getUtxos(scripts.settingsAddress);
  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }
  const settings = settingsUtxos[0];

  if (flags.submit) {
    const sk = await Deno.readTextFile(flags.privateKeyFile);
    const skCborHex = JSON.parse(sk).cborHex;
    const skBech32 = C.PrivateKey.from_bytes(fromHex(skCborHex)).to_bech32();
    const userPublicKey = toPublicKey(skBech32);
    const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
    const userAddress = lucid.utils.credentialToAddress({
      type: "Key",
      hash: userPkh.to_hex(),
    });
    lucid.selectWalletFromPrivateKey(skBech32);
  } else {
    lucid.selectWalletFrom({
      address: address,
    });
  }

  const change = await findChange(blockfrost, address);

  const refs = await Deno.readTextFile(flags.references);
  const lines = refs.split(/\r?\n/);
  const refUtxosOutRefs: OutRef[] = [];
  for (let line of lines) {
    let [hash, ix] = line.split("#");
    let ixNum = Number(ix);
    if (hash == "" || isNaN(ixNum)) {
      continue;
    }
    refUtxosOutRefs.push({
      txHash: hash,
      outputIndex: Number(ix),
    });
  }
  const references = await blockfrost.getUtxosByOutRef(refUtxosOutRefs);

  const tx = lucid.newTx();
  const currentTime = Date.now();
  //tx.validFrom(currentTime - 1000000);
  //tx.validTo(currentTime + 1000000);
  tx.readFrom([...references, settings]);
  tx.collectFrom([change]);
  tx.delegateTo(
    scripts.poolStakeAddress,
    flags.stakePool,
    "d87980"
  );
  tx.attachMintingPolicy(scripts.poolStakeValidator);
  const signers = flags.signers.split(",");
  for (s of signers) {
    tx.addSignerKey(s);
  }
  const txStr = await tx.toString();
  console.log("tentative tx: " + txStr);
  const completed = await tx.complete({
    coinSelection: false,
  });
  const completedStr = await completed.toString();
  console.log("completed tx: " + envelope(completedStr));
  const txid = completed.toHash();
  console.log("txid: " + txid);

  if (flags.submit) {
    const signedTx = await completed.sign().complete();
    await signedTx.submit();
    console.log("submitted");
  }
}

async function withdrawPoolRewards() {
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);

  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Mainnet");
  
  const scripts = getScriptsAiken(lucid, scriptsJson);

  lucid.selectWalletFrom({
    address: address,
  });

  let poolAddress;
  if (flags.poolAddress) {
    poolAddress = flags.poolAddress;
  } else {
    console.log("poolScriptHash: " + scripts.poolScriptHash);
    console.log("poolStakeHash: " + scripts.poolStakeHash);
    poolAddress = lucid.utils.credentialToAddress(
      {
        type: "Script",
        hash: scripts.poolScriptHash,
      },
      {
        type: "Script",
        hash: scripts.poolStakeHash,
      }
    );
  }
  console.log("poolAddress: " + poolAddress);
  let knownPools = await lucid.provider.getUtxos(poolAddress);
  let targetPool = null;
  for (let knownPool of knownPools) {
    let targetAssetName = computePoolNftName(fromHex(flags.poolIdent));
    let targetPolicyId = scripts.poolScriptHash;
    let targetNftUnit = targetPolicyId + targetAssetName;
    let amountOfTargetNft = knownPool.assets[targetNftUnit];
    if (amountOfTargetNft == 1n) {
      targetPool = knownPool;
    } else if (amountOfTargetNft > 1n) {
      throw new Error("Impossible: Multiple copies of pool NFT found in UTXO: " + JSON.stringify(knownPool));
    }
  }
  if (targetPool == null) {
    throw new Error("Can't find a pool UTXO containing the NFT for the ident: " + flags.poolIdent);
  }

  const settingsUtxos = await blockfrost.getUtxos(scripts.settingsAddress);
  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }
  const settings = settingsUtxos[0];
  console.log(settings.datum);
  const settingsDatum = Data.from(settings.datum, types.SettingsDatum);

  const treasuryAddress = await addressPlutusToLucid(lucid, settingsDatum.treasuryAddress);
  console.log("treasury address: " + treasuryAddress);

  let remainingPoolFees = 5_000_000n;
  let newPoolDatum = Data.from(targetPool.datum, types.PoolDatum);
  const withdrawnAmount = newPoolDatum.protocolFees - remainingPoolFees;
  newPoolDatum.protocolFees = remainingPoolFees;

  // TODO: Only need to give a fraction of this, but we are free to give all of
  // it
  const treasuryAmount = withdrawnAmount * 92n / 100n;

  const withheld = withdrawnAmount - treasuryAmount;

  console.log("to treasury: " + treasuryAmount);
  console.log("withheld: " + withheld);

  const newPoolValue = structuredClone(targetPool.assets);
  newPoolValue.lovelace -= withdrawnAmount;
  console.log("targetPool.assets");
  console.log(targetPool.assets);
  console.log("newPoolValue");
  console.log(newPoolValue);
  
  const change = await findChange(blockfrost, address);

  // We need to figure out what index in the inputs the pool utxo will have
  let toSpend = [];
  toSpend.push(change);
  toSpend.push(targetPool);
  toSpend.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));
  let poolInputIndex = 0n;
  for (let e of toSpend) {
    if (e.address == targetPool.address) {
      break;
    }
    poolInputIndex = poolInputIndex + 1n;
  }
  console.log("toSpend: ")
  console.log(toSpend);

  const poolManageRedeemer = Data.to({
    WithdrawFees: {
      amount: withdrawnAmount,
      treasuryOutput: 1n,
      poolInput: poolInputIndex, 
    }
  }, types.PoolManageRedeemer);
  console.log("poolManageRedeemer: " + poolManageRedeemer);

  let poolSpendRedeemer = Data.to({
    Manage: [],
  }, types.PoolSpendRedeemer);
  poolSpendRedeemer = "d87a9f" + poolSpendRedeemer + "ff";
  console.log("poolSpendRedeemer: " + poolSpendRedeemer);

  const refs = await Deno.readTextFile(flags.references);
  const lines = refs.split(/\r?\n/);
  const refUtxosOutRefs: OutRef[] = [];
  for (let line of lines) {
    let [hash, ix] = line.split("#");
    let ixNum = Number(ix);
    if (hash == "" || isNaN(ixNum)) {
      continue;
    }
    refUtxosOutRefs.push({
      txHash: hash,
      outputIndex: Number(ix),
    });
  }
  const references = await blockfrost.getUtxosByOutRef(refUtxosOutRefs);

  // to withdraw fees,
  // invoke the pool script with the Manage redeemer
  // with a withdrawal against the management stake script
  // the treasury admin multisig must be satisfied,
  // we must can withdraw up to the amount of locked protocol fees but we need
  // to remember to leave some behind for the minUTXO cost
  // can take 9/10 of the amount and must send 1/10 to the treasury
  // and you gotta update the datum
  const tx = lucid.newTx();
  const currentTime = Date.now();
  //tx.validFrom(currentTime - 1000000);
  //tx.validTo(currentTime + 1000000);
  tx.readFrom([...references, settings]);
  tx.collectFrom([change]);
  tx.collectFrom([targetPool], poolSpendRedeemer);
  const signers = flags.signers.split(",");
  for (s of signers) {
    tx.addSignerKey(s);
  }
  // pay the new pool with datum and value updated for withdrawal
  tx.attachMintingPolicy(scripts.poolManageValidator);
  tx.payToContract(
    targetPool.address,
    {
      inline: Data.to(newPoolDatum, types.PoolDatum),
    },
    newPoolValue
  );
  tx.withdraw(scripts.poolManageAddress, 0n, poolManageRedeemer);
  tx.payToAddressWithData(
    treasuryAddress,
    {
      inline: "d87980" // Void
    },
    {
      "lovelace": treasuryAmount,
    },
  );
  tx.payToAddress(
    flags.withheldAddress,
    {
      "lovelace": withheld,
    },
  );
  const txStr = await tx.toString();
  console.log("tentative tx: " + txStr);
  const completed = await tx.complete({
    coinSelection: false,
  });
  const completedStr = await completed.toString();
  console.log("completed tx: " + envelope(completedStr));
  const txid = completed.toHash();
  console.log("txid: " + txid);
}

async function updateSettingsDatumTreasury() {
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);

  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Mainnet");
  
  const scripts = getScriptsAiken(lucid, scriptsJson);

  lucid.selectWalletFrom({
    address: address,
  });

  const signers = flags.signers.split(",");

  const settingsDatumBytes = await Deno.readTextFile(flags.settingsDatumFile);
  console.log(settingsDatumBytes);

  const settingsUtxos = await blockfrost.getUtxos(scripts.settingsAddress);

  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }

  const settings = settingsUtxos[0];

  //let settingsDatum = Data.from(settingsDatumBytes, types.SettingsDatum);
  //console.log(settingsDatum);
  
  let farming = true;
  let nonce = 0n;
  let completed;
  while (farming) {
    const tx = lucid.newTx();
    const currentTime = Date.now();
    //tx.validFrom(currentTime - 1000000);
    //tx.validTo(currentTime + 1000000);
    const change = await findChange(blockfrost, address);
    tx.collectFrom([change]);
    tx.collectFrom([settings], settingsRedeemerUpdateTreasury());
    tx.attachSpendingValidator(scripts.settingsValidator);
    for (s of signers) {
      tx.addSignerKey(s);
    }
    tx.payToContract(
      scripts.settingsAddress,
      {
        inline: settingsDatumBytes,//Data.to(settingsDatum, types.SettingsDatum)
      },
      settings.assets
    );
    tx.payToAddress(
      address,
      {
        "lovelace": 2_000_100n + nonce,
      }
    );
    //const txStr = await tx.toString();
    //console.log("tx: " + txStr);
    completed = await tx.complete({
      coinSelection: false,
    });
    //const completedStr = await completed.toString();
    //console.log("completed tx: " + completedStr);
    const txid = completed.toHash();
    console.log("txid: " + txid);
    nonce += 1n;
    if (txid.startsWith("0")) {
      farming = false;
    }
  }
  const completedStr = await completed.toString();
  console.log("tx: " + envelope(completedStr));
  //const signedTx = await completed.sign().complete();
  //const signedStr = await signedTx.toString();
  //console.log("signed tx for noop settings: " + signedStr);
  //return signedStr;
}


async function updateSettingsDatum() {
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);

  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Mainnet");
  
  const scripts = getScriptsAiken(lucid, scriptsJson);

  lucid.selectWalletFrom({
    address: address,
  });

  const signers = flags.signers.split(",");

  const settingsDatumBytes = await Deno.readTextFile(flags.settingsDatumFile);
  console.log(settingsDatumBytes);

  const settingsUtxos = await blockfrost.getUtxos(scripts.settingsAddress);

  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }

  const settings = settingsUtxos[0];

  //let settingsDatum = Data.from(settingsDatumBytes, types.SettingsDatum);
  //console.log(settingsDatum);
  
  let farming = true;
  let nonce = 0n;
  let completed;
  while (farming) {
    const tx = lucid.newTx();
    const currentTime = Date.now();
    //tx.validFrom(currentTime - 1000000);
    //tx.validTo(currentTime + 1000000);
    const change = await findChange(blockfrost, address);
    tx.collectFrom([change]);
    tx.collectFrom([settings], settingsRedeemerUpdate());
    tx.attachSpendingValidator(scripts.settingsValidator);
    for (s of signers) {
      tx.addSignerKey(s);
    }
    tx.payToContract(
      scripts.settingsAddress,
      {
        inline: settingsDatumBytes,//Data.to(settingsDatum, types.SettingsDatum)
      },
      settings.assets
    );
    tx.payToAddress(
      address,
      {
        "lovelace": 2_000_100n + nonce,
      }
    );
    //const txStr = await tx.toString();
    //console.log("tx: " + txStr);
    completed = await tx.complete({
      coinSelection: false,
    });
    //const completedStr = await completed.toString();
    //console.log("completed tx: " + completedStr);
    const txid = completed.toHash();
    console.log("txid: " + txid);
    nonce += 1n;
    if (txid.startsWith("0")) {
      farming = false;
    }
  }
  const completedStr = await completed.toString();
  console.log("tx: " + completedStr);
  //const signedTx = await completed.sign().complete();
  //const signedStr = await signedTx.toString();
  //console.log("signed tx for noop settings: " + signedStr);
  //return signedStr;
}


async function addScooper() {
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);

  const settingsDatumBytes = await Deno.readTextFile(flags.settingsDatumFile);

  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Mainnet");
  
  const scripts = getScriptsAiken(lucid, scriptsJson);

  lucid.selectWalletFrom({
    address: address,
  });

  const settingsUtxos = await blockfrost.getUtxos(scripts.settingsAddress);

  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }

  const settings = settingsUtxos[0];

  let settingsDatum = Data.from(settingsDatumBytes, types.SettingsDatum);

  console.log("scooperPkh is: ", flags.scooperPkh);
  console.log(settingsDatum);

  const tx = lucid.newTx();
  const currentTime = Date.now();
  tx.validFrom(currentTime - 1000000);
  tx.validTo(currentTime + 1000000);
  const change = await findChange(blockfrost, address);
  tx.collectFrom([change]);
  tx.collectFrom([settings], settingsRedeemerUpdate());
  tx.attachSpendingValidator(scripts.settingsValidator);
  tx.addSigner(address);
  tx.payToContract(
    scripts.settingsAddress,
    {
      inline: Data.to(settingsDatum, types.SettingsDatum)
    },
    settings.assets
  );
  const txStr = await tx.toString();
  console.log("tx: " + txStr);
  const completed = await tx.complete({
    coinSelection: false,
  });
  const completedStr = await completed.toString();
  console.log("completed tx: " + completedStr);
  //const signedTx = await completed.sign().complete();
  //const signedStr = await signedTx.toString();
  //console.log("signed tx for noop settings: " + signedStr);
  //return signedStr;
}

async function noopSettings(): Promise<TxHash> {
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);

  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Mainnet");
  
  const scripts = getScriptsAiken(lucid, scriptsJson);

  lucid.selectWalletFrom({
    address: address,
  });

  const settingsUtxos = await blockfrost.getUtxos(scripts.settingsAddress);

  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }

  const settings = settingsUtxos[0];

  let settingsDatum = Data.from(settings.datum, types.SettingsDatum);

  console.log(settingsDatum);

  const tx = lucid.newTx();
  const currentTime = Date.now();
  tx.validFrom(currentTime - 1000000);
  tx.validTo(currentTime + 1000000);
  const change = await findChange(blockfrost, address);
  tx.collectFrom([change]);
  tx.collectFrom([settings], settingsRedeemerUpdate());
  tx.attachSpendingValidator(scripts.settingsValidator);
  tx.addSigner(address);
  tx.payToContract(
    scripts.settingsAddress,
    {
      inline: Data.to(settingsDatum, types.SettingsDatum)
    },
    settings.assets
  );
  const txStr = await tx.toString();
  console.log("tx: " + txStr);
  const completed = await tx.complete({
    coinSelection: false,
  });
  const completedStr = await completed.toString();
  console.log("completed tx: " + completedStr);
  //const signedTx = await completed.sign().complete();
  //const signedStr = await signedTx.toString();
  //console.log("signed tx for noop settings: " + signedStr);
  //return signedStr;
}

const v3InitScoopers = [
  "570cd6294587645d26c690a72d40fede1e7a28cb3ddc78ff76655820",
  "61f1baeda28f3f83413b92a7d28d2f7b545d718f2f28f971b92b3a21",
  "251f7fb11f84f81653ee5b76a10dd29fa36ec7717aafe689490cb7e4",
  "6510a3ec0a6f273e31acc82f9f2ffb089413549a04149ea37ef8d33b",
  "9366b01d6baf040245ee07127fc8af4f04a75b91c6a97f69c7f6463a",
  "c5825983bb454dd743befc1dd65ee05934666c417503060e1d4fef47",
  "70fa8ce8dda9372aa9b7dc9f5756390e78939744c79550cc3a264b79",
  "f7b1175ea4f7980e717e19c76731e4e6ff3a2ac560dc17a6be8ec204",
  "a14cb1a14c4b5810a21103e389f4abdbdec010b766e2dc329a4e0e96",
  "40282b949abda48a573fe2757971a1369d2674ac9b6d98c1c2bdbdf7",
  "baec408a6fedd39ac0404a2f82c6e75ef06659d8596f9d0af6e01241",
  "fe9315a8d1f638a4836e9ec396d43e1f6ba88e45a7f5a5e37a77071a",
  "c6b0d1b88337b91507aa5c6496afc497f399ed8980c2054448eaab6c",
  "8ca0e08cdbc30fa0dd21833d7370d666493ecc28b136df179f97fb5d",
  "f7e1830a1f885aed62fc834c1dffcadd68a8548e88ffd0b0040b960b",
  "ee8ed5ef92d0a51c6962aac7012906d280aeb412900a7621f782c7c9",
  "1ddc54ce9d4d3a35a0ff4707636f8627cc491197ac435ba4fcf4d838",
  "cba4b71bd8cecc54c526bcd71da84f6f79e568604e574149854dbb86",
  "f52cdec15ffcc8ace593dc3e0078458ba07a8c47866ba866e4554b6d",
  "53d6b12089d642d3bfdc61d5f0f3fddfeeb56f55dcd5bd796b5c25a1",
  "dd8a02814820616b137e0fb4852fd8aab36875d849919ca68aa6cb70",
  "0b23328355b40d671d1a7ded332c697e1446ae0bb7301af2a7ed9577",
  "8be85963f17386d34bcd53b857071a01ee3c7ca543e4bd674492f78b",
  "b3ec1a4fa75b0c9291af873d3d5365b46ab22eff2bc3f12d5064561b",
  "6c8ecf30ba1a025dd324cb0598c8ff87522b324901299cf3f4f1d0b2",
  "7a7a02beabb674125d734a24817aea9505b9113540cc72f4ef5c2faf",
  "2daa1c8bb06659a89886f6b2677b8d8ccd1c2c7e9ef7ea4be88ac404",
  "6a054c69ea18300b0fcb330e07a8b56015d15527f31a361b1b1291de",
  "ee33744f202c96c2024b25b6cc177ad9537d46437cd48cc291f58009",
  "37eb116b3ff8a70e4be778b5e8d30d3b40421ffe6622f6a983f67f3f",
];

const v3InitPoolStakeHash = "4399813dad91bb78a5eb17c26ff50852bc75d3fa7b6e9ae87232ccc1";

function v3MakeInitSettingsDatum() {
  const datum: types.SettingsDatum = {
    settingsAdmin: {
      AtLeast: {
        required: 3n,
        scripts: [
          { signature: "8582e6a55ccbd7af4cabe35d6da6eaa3d543083e1ce822add9917730" },
          { signature: "7180d7ad9aaf20658d8f88c32a2e5c287425618c32c9bb82d6b6c8f8" },
          { signature: "bba4dff30f517f2859f8f295a97d3d85f26a818078f9294256fda2d8" },
          { signature: "1f68495896a7ba5132198145359311e991a1463e95ccc6f56703653d" },
          { signature: "f65e667d512b26aa98a97ac22e958e5201e7ea279d74b2e4ec5883db" },
        ],
      },
    },
    metadataAdmin: {
      paymentCredential: {
        SCredential: { bytes: "1854e9028a89496e9772a54882729d16554f8ed9af27ec6046c9a87c" },
      },
      stakeCredential: null,
    },
    treasuryAdmin: {
      AtLeast: {
        required: 3n,
        scripts: [
          { signature: "8582e6a55ccbd7af4cabe35d6da6eaa3d543083e1ce822add9917730" },
          { signature: "7180d7ad9aaf20658d8f88c32a2e5c287425618c32c9bb82d6b6c8f8" },
          { signature: "bba4dff30f517f2859f8f295a97d3d85f26a818078f9294256fda2d8" },
          { signature: "1f68495896a7ba5132198145359311e991a1463e95ccc6f56703653d" },
          { signature: "f65e667d512b26aa98a97ac22e958e5201e7ea279d74b2e4ec5883db" },
        ],
      },
    },
    treasuryAddress: {
      paymentCredential: {
        SCredential: { bytes: "c0d7aa781d14f206f1f6468f0a2d49187d1ebcb8f59c59d75d0c27a7" },
      },
      stakeCredential: null,
    },
    treasuryAllowance: [0n, 10n],
    authorizedScoopers: v3InitScoopers,
    authorizedStakingKeys: [
      {
        SCredential: { bytes: v3InitPoolStakeHash },
      },
    ],
    baseFee: 332_000n,
    simpleFee: 168_000n,
    strategyFee: 168_000n,
    poolCreationFee: 0n,
    extensions: 0n,
  };
  console.log(datum);
  return Data.to(datum, types.SettingsDatum);
}

function settingsDatum(poolStakeHash: string, userPkh: string, scooperSet: string[]): string {
  const datum: types.SettingsDatum = {
    settingsAdmin: {
      Signature: { signature: userPkh },
    },
    metadataAdmin: {
      paymentCredential: {
        VKeyCredential: { bytes: userPkh },
      },
      stakeCredential: null,
    },
    treasuryAdmin: {
      Signature: { signature: userPkh },
    },
    treasuryAddress: {
      paymentCredential: {
        VKeyCredential: { bytes: userPkh },
      },
      stakeCredential: null,
    },
    treasuryAllowance: [1n, 10n],
    authorizedScoopers: scooperSet,
    authorizedStakingKeys: [
      {
        SCredential: { bytes: poolStakeHash },
      }
    ],
    baseFee: 332_000n,
    simpleFee: 168_000n,
    strategyFee: 168_000n,
    poolCreationFee: 0n,
    extensions: 0n,
  };
  console.log(datum);
  return Data.to(datum, types.SettingsDatum);
}

function settingsMintRedeemer() { return "d87980" };

async function bootSettings(lucid: Lucid, scripts: Scripts, userPkh: string, inputs: UTxO[], scooperSet: string[], metadata: Json): string {
  let newSettingsDatum;
  if (flags.useV3InitSettings) {
    newSettingsDatum = v3MakeInitSettingsDatum();
  } else {
    newSettingsDatum = settingsDatum(scripts.poolStakeHash, userPkh, scooperSet);
  }
  const tx = await lucid.newTx()
    .collectFrom(inputs)
    .mintAssets({
      [toUnit(scripts.settingsPolicyId, fromText("settings"))]: 1n,
    }, settingsMintRedeemer())
    .attachMintingPolicy(scripts.settingsMint)
    .collectFrom(inputs || [])
    .payToContract(scripts.settingsAddress, { inline: newSettingsDatum }, {
      "lovelace": 2_000_000n,
      [toUnit(scripts.settingsPolicyId, fromText("settings"))]: 1n,
    });

  if (metadata) {
    for (const [k, v] of Object.entries(metadata)) {
      tx.attachMetadataWithConversion(k, v);
    }
  }
  let completed = await tx.complete({
    coinSelection: false,
  });

  let completeStr = await completed.toString();
  console.log("completed boot tx str: " + completeStr);
  let signed = await tx.sign().complete();
  let submittedHash = await signed.submit();
  console.log(submittedHash);
  return tx.toString();
}

async function registerStakeAddress(scriptName: string) {
   if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);

  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Preview");
  
  const scripts = getScriptsAiken(lucid, scriptsJson);

  if (flags.submit) {
    const sk = await Deno.readTextFile(flags.privateKeyFile);
    const skCborHex = JSON.parse(sk).cborHex;
    const skBech32 = C.PrivateKey.from_bytes(fromHex(skCborHex)).to_bech32();
    const userPublicKey = toPublicKey(skBech32);
    const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
    const userAddress = lucid.utils.credentialToAddress({
      type: "Key",
      hash: userPkh.to_hex(),
    });
    lucid.selectWalletFromPrivateKey(skBech32);
  } else {
    lucid.selectWalletFrom({
      address: address,
    });
  }

  const thisScript = scripts[scriptName as keyof Scripts] as Script;
  const rewardStakeRewardAddress = lucid.utils.validatorToRewardAddress(thisScript);
  const tx = await lucid.newTx()
    .registerStake(rewardStakeRewardAddress)
    .complete();
  console.log("registration tx: " + tx.toString());

  if (flags.submit) {
    const txid = tx.toHash();
    console.log(txid);
    const signedTx = await tx.sign().complete();
    await signedTx.submit();
    console.log("submitted");
  }
}

async function listOrder(lucid: Lucid, scripts: Scripts, userPkh: string, assets: CoinPair, gives: bigint, scooperFee: bigint, changeUtxo: UTxO, count: bigint, targetPoolId?: string): Promise<TxHash> {
  const rider = 2_000_000n;
  const tx = lucid.newTx();
  tx.collectFrom([changeUtxo]);
  for (let i = 0n; i < count; i++) {
    let flip = flags.random && rand.pick(true, false);
    if (flip) {
      let [tmpPolicyId, tmpTokenName] = assets[0];
      assets[0][0] = assets[1][0];
      assets[0][1] = assets[1][1];
      assets[1][0] = tmpPolicyId;
      assets[1][1] = tmpTokenName;
    }
    const newOrderDatum: types.OrderDatum = {
      poolIdent: targetPoolId,
      owner: {
        signature: userPkh,
      },
      scooperFee: scooperFee,
      destination: {
        Fixed: {
          address: {
            paymentCredential: {
              VKeyCredential: { bytes: userPkh },
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
    let value = {
      "lovelace": rider + scooperFee,
    };
    if (assets[0][0] == "") {
      value["lovelace"] += gives;
    } else {
      value[assets[0][0] + assets[0][1]] = gives;
    }
    console.log("value out: ");
    console.log(value);
    tx.payToContract(scripts.orderAddress, { inline: Data.to(newOrderDatum, types.OrderDatum) }, value);
  }
  const completed = await tx.complete({
    coinSelection: false,
  });
  const signedTx = await completed.sign().complete();
  const signedStr = await signedTx.toString();
  console.log("signed tx for listOrder: " + signedStr);
  const listedHash = await signedTx.submit();
  await emulator.awaitTx(listedHash);
  let refs = [];
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

async function cancelOrder(lucid: Lucid, scripts: Scripts, userAddress: string, userPkh: string, orderUtxo: UTxO, changeUtxo: UTxO): Promise<TxHash> {
  const rider = 2_000_000n;
  const tx = lucid.newTx();
  tx.collectFrom([changeUtxo]);
  console.log("Cancel redeemer: ");
  console.log(Data.to("Cancel", types.OrderRedeemer));
  tx.collectFrom([orderUtxo], Data.to("Cancel", types.OrderRedeemer));
  tx.attachSpendingValidator(scripts.orderValidator);
  tx.addSigner(userAddress);
  const completed = await tx.complete({
    coinSelection: false,
  });
  const signedTx = await completed.sign().complete();
  return signedTx.submit();
}

function fakeAddress(lucid: Lucid): [Address, C.Ed25519KeyHash, string] {
  const userPrivateKey = "ed25519_sk1zxsfsl8ehspny4750jeydt5she7dzstrj7za5vgxl6929kr9d33quqkgp3";
  const userPublicKey = toPublicKey(userPrivateKey);
  const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
  const userAddress = (new Utils(lucid)).credentialToAddress({
    type: "Key",
    hash: userPkh.to_hex(),
  });
  return [userAddress, userPkh, userPrivateKey];
}
interface Scripts {
  poolValidator: Script;
  poolScriptHash: ScriptHash;
  poolAddress: Address;
  settingsValidator: Script;
  settingsScriptHash: ScriptHash;
  settingsAddress: Address;
  orderValidator: Script;
  orderScriptHash: ScriptHash;
  orderAddress: Address;
  steakValidator: Script;
  steakScriptHash: ScriptHash;
  steakAddress: Address;
  poolMint: Script;
  poolPolicyId: PolicyId;
  poolStakeHash: ScriptHash;
  poolStakeAddress: Address;
  poolStakeValidator: Script;
  poolManageHash: ScriptHash;
  poolManageAddress: Address;
  poolManageValidator: Script;
};

function bytesToScript(bytes: string) {
  return { type: "PlutusV2", script: bytes };
}

function getScriptsAiken(lucid: Lucid, json: any): Scripts {
  let validator = json["validators"];
  let out: any = {};
  for (let v of validator) {
    if (v.title == "order.spend") {
      out.orderValidator = bytesToScript(v.compiledCode);
      out.orderScriptHash = lucid.utils.validatorToScriptHash(out.orderValidator);
      out.orderAddress = lucid.utils.validatorToAddress(out.orderValidator);
    }
    if (v.title == "settings.spend") {
      out.settingsValidator = bytesToScript(v.compiledCode);
      out.settingsScriptHash = lucid.utils.validatorToScriptHash(out.settingsValidator);
      out.settingsAddress = lucid.utils.validatorToAddress(out.settingsValidator);
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
      out.settingsMint = bytesToScript(v.compiledCode);
      out.settingsPolicyId = lucid.utils.mintingPolicyToId(out.settingsMint);
    }
    if (v.title == "pool_stake.stake") {
      out.poolStakeValidator = bytesToScript(v.compiledCode);
      out.poolStakeHash = lucid.utils.validatorToScriptHash(out.poolStakeValidator);
      out.poolStakeAddress = lucid.utils.validatorToRewardAddress(out.poolStakeValidator);
    }
    if (v.title == "pool.manage") {
      out.poolManageValidator = bytesToScript(v.compiledCode);
      out.poolManageHash = lucid.utils.mintingPolicyToId(out.poolManageValidator);
      out.poolManageAddress = lucid.utils.validatorToRewardAddress(out.poolManageValidator);
    }
  }
  return out;
}

async function testSettingsBoot(lucid: Lucid, emulator: Emulator, scripts: Scripts) {
  const dummy = await Lucid.new(undefined, "Custom");
  const [userAddress, userPkh, userPrivateKey] = fakeAddress(dummy);

  const bootUtxo = (await emulator.getUtxosByOutRef([{
    txHash: bootUtxoHash,
    outputIndex: bootUtxoIx,
  }]))[0];

  console.log(bootUtxo);

  lucid.selectWalletFromPrivateKey(userPrivateKey);
  const bootedHash = await bootSettings(lucid, scripts, userPkh.to_hex(), [], [userPkh.to_hex()]);
  console.log("bootedHash: " + bootedHash);
  await emulator.awaitTx(bootedHash);
  return bootedHash;
}

async function dummy() {
  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Preview");

  if (flags.submit) {
    const sk = await Deno.readTextFile(flags.privateKeyFile);
    const skCborHex = JSON.parse(sk).cborHex;
    const skBech32 = C.PrivateKey.from_bytes(fromHex(skCborHex)).to_bech32();
    const userPublicKey = toPublicKey(skBech32);
    const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
    const userAddress = lucid.utils.credentialToAddress({
      type: "Key",
      hash: userPkh.to_hex(),
    });
    lucid.selectWalletFromPrivateKey(skBech32);
  } else {
    lucid.selectWalletFrom({
      address: address,
    });
  }

  let metadata;
  if (flags.metadata) {
    let metadataStr = await Deno.readTextFile(flags.metadata);
    metadata = JSON.parse(metadataStr);
    console.log("metadata");
    console.log(metadata);
  }

  const change = await findChange(lucid.provider, address);

  const tx = lucid.newTx();
  tx.collectFrom([change]);
  if (metadata) {
    for (const [k, v] of Object.entries(metadata)) {
      tx.attachMetadataWithConversion(k, v);
    }
  }
  const completed = await tx.complete({
    coinSelection: false,
  });
  const completedStr = await completed.toString();
  const signedTx = await completed.sign().complete();
  console.log(completedStr);
  const txHash = await signedTx.submit();
  console.log("submitted: " + txHash);

}

async function realSettingsBoot() {
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);

  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Mainnet");
  
  const scripts = getScriptsAiken(lucid, scriptsJson);

  if (flags.submit) {
    const sk = await Deno.readTextFile(flags.privateKeyFile);
    const skCborHex = JSON.parse(sk).cborHex;
    const skBech32 = C.PrivateKey.from_bytes(fromHex(skCborHex)).to_bech32();
    const userPublicKey = toPublicKey(skBech32);
    const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
    const userAddress = lucid.utils.credentialToAddress({
      type: "Key",
      hash: userPkh.to_hex(),
    });
    lucid.selectWalletFromPrivateKey(skBech32);
  } else {
    lucid.selectWalletFrom({
      address: address,
    });
  }

  let walletUtxos = await lucid.wallet.getUtxos();
  let toSpend = undefined;
  for (let utxo of walletUtxos) {
    if (utxo.txHash == bootUtxoHash && utxo.outputIndex == bootUtxoIx) {
      toSpend = utxo;
      break;
    }
  }

  if (toSpend == undefined) {
    throw new Error("the boot utxo was not found in the wallet");
  }

  let metadata;
  if (flags.metadata) {
    let metadataStr = await Deno.readTextFile(flags.metadata);
    metadata = JSON.parse(metadataStr);
    console.log("metadata");
    console.log(metadata);
  }
  const txBytes = await bootSettings(lucid, scripts, userPkh, [toSpend], [flags.scooperPkh], metadata);
  console.log("boot tx: " + txBytes);
}

async function doCancelOrder(scripts: Scripts, privateKeyFile: string, order: string, change: string) {
  const sk = await Deno.readTextFile(privateKeyFile);
  const skCborHex = JSON.parse(sk).cborHex;
  const skBech32 = C.PrivateKey.from_bytes(fromHex(skCborHex)).to_bech32();
  const userPublicKey = toPublicKey(skBech32);
  const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
  const userAddress = (new Utils(dummy)).credentialToAddress({
    type: "Key",
    hash: userPkh.to_hex(),
  });

  console.log("private key: " + skBech32);
  console.log("public key: " + userPublicKey);
  console.log("public key hash: " + userPkh.to_hex());
  console.log("address: " + userAddress);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId);
  const lucid = await Lucid.new(blockfrost, "Preview");

  lucid.selectWalletFromPrivateKey(skBech32);

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
    userPkh.to_hex(),
    orderUtxo,
    changeUtxo
  );
  console.log("canceledHash: " + canceledHash);
}

async function testListOrder(lucid: Lucid, emulator: Emulator, scripts: Scripts, coinA: string, coinB: string, change: UTxO, poolIdent: string, orderCount: bigint) {
  let assets: CoinPair = [
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
    userPkh.to_hex(),
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

function computePoolId(utxo: UTxO) {
  const poolInputTxHash = fromHex(utxo.txHash);
  const numberSign = new Uint8Array([0x23]);
  const poolInputTxIx = new Uint8Array([utxo.outputIndex]); // ident encoding for output index 1
  let poolInputRef = new Uint8Array([]);
  poolInputRef = concat(poolInputRef, poolInputTxHash);
  poolInputRef = concat(poolInputRef, numberSign);
  poolInputRef = concat(poolInputRef, poolInputTxIx);
  return C.hash_blake2b256(poolInputRef).slice(4); // Truncate first four bytes
}

function computePoolNftName(poolId: Uint8Array) {
  const prefix = new Uint8Array([0x00, 0x0d, 0xe1, 0x40]);
  return toHex(concat(prefix, poolId));
}

function computePoolLqName(poolId: Uint8Array) {
  const prefix = new Uint8Array([0x00, 0x14, 0xdf, 0x10]);
  return toHex(concat(prefix, poolId));
}

function computePoolRefName(poolId: Uint8Array) {
  const prefix = new Uint8Array([0x00, 0x06, 0x43, 0xb0]);
  return toHex(concat(prefix, poolId));
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

async function getRberryPolicyId(): Promise<[Script, string]> {
  const dummy = await Lucid.new(undefined, "Custom");
  const rberryMintingPolicy: Script = dummy.utils.nativeScriptFromJson({
    type: "all",
    scripts: [],
  });
  return [rberryMintingPolicy, dummy.utils.mintingPolicyToId(rberryMintingPolicy)];
}

async function postReferenceScript(scripts: Scripts, lucid: Lucid, userAddress: Address, scriptName: string, changeUtxo: UTxO, settings: UTxO): TxHash {
  if (scriptName in scripts) {
    let tx;
    let retry = true;
    let nonce = 0n;
    while (retry) {
      console.log(`nonce=${nonce}`);
      tx = await lucid.newTx()
        .collectFrom([changeUtxo])
        .payToAddressWithData(flags.destinationAddress || userAddress, {
          scriptRef: scripts[scriptName as keyof Scripts] as Script,
        }, {
          "lovelace": 2_000_000n,
        })
        .payToAddress(userAddress, { "lovelace": 2_000_000n + nonce })
        .complete({
          coinSelection: false,
        });
      const hash = tx.toHash();
      if (hash.startsWith("f")) {
        break;
      }
      nonce += 1n;
    }
    console.log("post reference script: ", tx.toString());
    let signed = await tx.sign().complete();
    await signed.submit();
    return tx.toString();
  } else {
    throw new Error("script does not exist: " + scriptName);
  }
}

async function mintRberry(scripts: Scripts, lucid: Lucid, userAddress: Address): Promise<TxHash> {
  const [rberryMintingPolicy, rberryPolicyId]: [Script, string] = await getRberryPolicyId();
  const tx = await lucid.newTx()
    .mintAssets({
      [toUnit(rberryPolicyId, fromText("RBERRY"))]: 10_000_000_000n,
    })
    .attachMintingPolicy(rberryMintingPolicy)
    .payToAddress(userAddress, {
      "lovelace": 2_000_000n,
      [toUnit(rberryPolicyId, fromText("RBERRY"))]: 10_000_000_000n,
    })
    .complete();
  console.log("mintRberry: ", tx.toString());
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
}

async function evaporatePool() {
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);

  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Preview");
  
  const scripts = getScriptsAiken(lucid, scriptsJson);

  let settingsUtxos = await lucid.provider.getUtxos(scripts.settingsAddress);
  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }
  const settings = settingsUtxos[0];
  const settingsDatum = Data.from(settings.datum, types.SettingsDatum);

  const refs = await Deno.readTextFile(flags.references);
  const lines = refs.split(/\r?\n/);
  const refUtxosOutRefs: OutRef[] = [];
  for (let line of lines) {
    let [hash, ix] = line.split("#");
    let ixNum = Number(ix);
    if (hash == "" || isNaN(ixNum)) {
      continue;
    }
    refUtxosOutRefs.push({
      txHash: hash,
      outputIndex: Number(ix),
    });
  }
  const references = await blockfrost.getUtxosByOutRef(refUtxosOutRefs);
  console.log(references);

  if (flags.submit) {
    const sk = await Deno.readTextFile(flags.privateKeyFile);
    const skCborHex = JSON.parse(sk).cborHex;
    const skBech32 = C.PrivateKey.from_bytes(fromHex(skCborHex)).to_bech32();
    const userPublicKey = toPublicKey(skBech32);
    const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
    const userAddress = lucid.utils.credentialToAddress({
      type: "Key",
      hash: userPkh.to_hex(),
    });
    lucid.selectWalletFromPrivateKey(skBech32);
  } else {
    lucid.selectWalletFrom({
      address: address,
    });
  }

  const poolAddress = (new Utils(lucid)).credentialToAddress(
    {
      type: "Script",
      hash: scripts.poolScriptHash,
    },
    {
      type: "Script",
      hash: scripts.poolStakeHash,
    }
  );

  let knownPools = await lucid.provider.getUtxos(poolAddress);
  let targetPool = null;
  for (let knownPool of knownPools) {
    let targetAssetName = computePoolNftName(fromHex(flags.poolIdent));
    let targetPolicyId = scripts.poolScriptHash;
    let targetNftUnit = targetPolicyId + targetAssetName;
    let amountOfTargetNft = knownPool.assets[targetNftUnit];
    if (amountOfTargetNft == 1n) {
      targetPool = knownPool;
    } else if (amountOfTargetNft > 1n) {
      throw new Error("Impossible: Multiple copies of pool NFT found in UTXO: " + JSON.stringify(knownPool));
    }
  }
  if (targetPool == null) {
    throw new Error("Can't find a pool UTXO containing the NFT for the ident: " + flags.poolIdent);
  }

  let poolDatum = Data.from(targetPool.datum, types.PoolDatum);

  const change = await findChange(lucid.provider, address);

  let toSpend = [];
  toSpend.push(change);
  toSpend.push(targetPool);
  toSpend.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));
  let poolInputIndex = 0n;
  for (let e of toSpend) {
    if (e.address == targetPool.address) {
      break;
    }
    poolInputIndex = poolInputIndex + 1n;
  }
  console.log("toSpend: ")
  console.log(toSpend);

  const poolMintRedeemer = {
    BurnPool: {
      poolIdent: flags.poolIdent,
    },
  };
  const poolMintRedeemerBytes = Data.to(poolMintRedeemer, types.PoolMintRedeemer);
  const poolSpendRedeemer = {
    Manage: []
  };
  let poolSpendRedeemerBytes = Data.to(poolSpendRedeemer, types.PoolSpendRedeemer);
  poolSpendRedeemerBytes = "d87a9f" + poolSpendRedeemerBytes + "ff";
  const poolManageRedeemer = {
    WithdrawFees: {
      poolInput: poolInputIndex,
      treasuryOutput: 0n,
      amount: poolDatum.protocolFees,
    }
  };
  console.log("poolDatum.protocolFees: " + poolDatum.protocolFees);
  const treasuryAddress = await addressPlutusToLucid(lucid, settingsDatum.treasuryAddress);
  const poolManageRedeemerBytes = Data.to(poolManageRedeemer, types.PoolManageRedeemer);
  const poolNftNameHex = computePoolNftName(fromHex(flags.poolIdent));
  console.log("baby1");
  const tx = await lucid.newTx()
    .mintAssets({
      [toUnit(scripts.poolPolicyId, poolNftNameHex)]: -1n,
    }, poolMintRedeemerBytes)
    .readFrom([...references, settings])
    .collectFrom([change])
    .collectFrom([targetPool], poolSpendRedeemerBytes)
    .attachMintingPolicy(scripts.poolManageValidator)
    .withdraw(scripts.poolManageAddress, 0n, poolManageRedeemerBytes)
    .payToAddressWithData(treasuryAddress, {
      inline: "d87980" // Void
    }, {
      "lovelace": poolDatum.protocolFees,
    });
  const signers = flags.signers.split(",");
  for (s of signers) {
    tx.addSignerKey(s);
  }
  const txStr = await tx.toString();
  console.log("txStr: " + txStr);
  const completed = await tx.complete({
    coinSelection: false,
  });
  if (flags.submit) {
    const signedTx = await completed.sign().complete();
    return signedTx.submit();
  } else {
    throw new Error("florp");
  }
}

async function mintPool(scripts: Scripts, lucid: Lucid, userAddress: Address, settings: UTxO, references: UTxO[], assets: CoinPair, seed: UTxO, amountA: bigint, amountB: bigint, fees: bigint[], marketOpen?: bigint): Promise<TxHash> {
  const poolId = computePoolId(seed);
  const liq = initialLiquidity(amountA, amountB);
  const newPoolDatum: types.PoolDatum = {
    identifier: toHex(poolId),
    assets: assets,
    circulatingLp: liq,
    bidFeesPer10Thousand: 30n,
    askFeesPer10Thousand: 50n,
    feeManager: {
      Signature: {
        signature: flags.feeManagerPkh, 
      }
    },
    marketOpen: marketOpen || 0n,
    protocolFees: 3_000_000n,
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
  let poolValue = {
    [toUnit(scripts.poolPolicyId, poolNftNameHex)]: 1n,
    [toUnit(assets[1][0], assets[1][1])]: amountB,
  };
  if (assets[0][0] == "") {
    poolValue["lovelace"] = amountA + 3_000_000n;
  } else {
    poolValue["lovelace"] = 3_000_000n;
    poolValue[toUnit(assets[0][0], assets[0][1])] = amountA;
  }

  const poolMintRedeemerBytes = Data.to(poolMintRedeemer, types.PoolMintRedeemer);
  const poolDatumBytes = Data.to(newPoolDatum, types.PoolDatum);

  // bad pool address
  //const poolAddress = (new Utils(lucid)).credentialToAddress(
  //  {
  //    type: "Script",
  //    hash: scripts.poolScriptHash,
  //  }
  //);

  const poolAddress = (new Utils(lucid)).credentialToAddress(
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
    .mintAssets({
      [toUnit(scripts.poolPolicyId, poolNftNameHex)]: 1n,
      [toUnit(scripts.poolPolicyId, poolRefNameHex)]: 1n,
      [toUnit(scripts.poolPolicyId, poolLqNameHex)]: liq,
    }, poolMintRedeemerBytes)
    .readFrom([...references, settings])
    .collectFrom([seed])
    .payToContract(poolAddress, { inline: poolDatumBytes }, poolValue)
    .payToAddress(userAddress, {
      "lovelace": 2_000_000n,
      [toUnit(scripts.poolPolicyId, poolLqNameHex)]: liq,
    })
    .payToAddressWithData(
      flags.metadataAddress,
      { inline: "d87980" },
      {
        "lovelace": 2_000_000n,
        [toUnit(scripts.poolPolicyId, poolRefNameHex)]: 1n,
      }
    );

  const str = await tx.toString();
  console.log("building tx: " + str);
  const completed = await tx.complete({
    coinSelection: false,
  });
  const signedTx = await completed.sign().complete();
  const signedStr = await signedTx.toString()
  console.log("signed tx: " + signedStr);
  const hash = await signedTx.submit();
  console.log("hash: " + hash);
  return {
    poolMintedHash: hash,
    poolId: toHex(poolId),
  };
}

function assetFromString(s: string): Asset {
  if (s == "lovelace") {
    return ["", ""];
  }
  let chunks = s.split(".");
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

async function testMintPool(lucid: Lucid, emulator: Emulator, scripts: Scripts, coinA: string, coinB: string, refUtxo: UTxO, seed: UTxO) {
  const dummy = await Lucid.new(undefined, "Custom");
  const [userAddress, userPkh, userPrivateKey] = fakeAddress(dummy);

  let assets: CoinPair = [
    assetFromString(coinA),
    assetFromString(coinB),
  ];

  const settingsUtxos = await emulator.getUtxos(scripts.settingsAddress);

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

async function mainnetMintPool() {
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);

  const address = flags.address;
  const userPkh = paymentCredentialOf(address).hash;

  console.log("address: " + address);

  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Preview");
  
  const scripts = getScriptsAiken(lucid, scriptsJson);

  let settingsUtxos = await lucid.provider.getUtxos(scripts.settingsAddress);
  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }
  const settings = settingsUtxos[0];

  const refs = await Deno.readTextFile(flags.references);
  const lines = refs.split(/\r?\n/);
  const refUtxosOutRefs: OutRef[] = [];
  for (let line of lines) {
    let [hash, ix] = line.split("#");
    let ixNum = Number(ix);
    if (hash == "" || isNaN(ixNum)) {
      continue;
    }
    refUtxosOutRefs.push({
      txHash: hash,
      outputIndex: Number(ix),
    });
  }
  const references = await blockfrost.getUtxosByOutRef(refUtxosOutRefs);
  console.log(references);

  const [seedRefHash, seedRefIx] = flags.seed.split("#");
  const seedUtxos = await blockfrost.getUtxosByOutRef([{
    txHash: seedRefHash,
    outputIndex: Number(seedRefIx),
  }]);
  if (seedUtxos.length == 0) {
    throw new Error("Couldn't find seed utxo");
  }
  const seed = seedUtxos[0];
  console.log(seed);

  let assets: CoinPair = [
    assetFromString(flags.coinA),
    assetFromString(flags.coinB),
  ];

  if (flags.submit) {
    const sk = await Deno.readTextFile(flags.privateKeyFile);
    const skCborHex = JSON.parse(sk).cborHex;
    const skBech32 = C.PrivateKey.from_bytes(fromHex(skCborHex)).to_bech32();
    const userPublicKey = toPublicKey(skBech32);
    const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
    const userAddress = lucid.utils.credentialToAddress({
      type: "Key",
      hash: userPkh.to_hex(),
    });
    lucid.selectWalletFromPrivateKey(skBech32);
  } else {
    lucid.selectWalletFrom({
      address: address,
    });
  }

  const minted = await mintPool(scripts, lucid, address, settings, references, assets, seed, BigInt(flags.hasA), BigInt(flags.hasB), 30n);
  console.log("minted");
  console.log(minted);
}

function computeIndexingSet(scripts: Scripts, changeUtxo: UTxO, targetPool: UTxO, orderUtxos: UTxO[]): [bigint, null, bigint][] {
  let toSpend = [];
  toSpend.push(changeUtxo);
  toSpend.push(targetPool);
  toSpend.push(...orderUtxos);
  toSpend.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));
  let i = 0n;
  let indexingSet = [];
  for (let i = 0n; i < toSpend.length; i++) {
    let e = toSpend[Number(i)];
    if (e.address == scripts.orderAddress) {
      indexingSet.push([i, null, 0n]);
    }
  }
  return indexingSet;
}

function getPoolABL(targetPool: UTxO, datum: types.PoolDatum): ABL {
  let poolCoinA = datum.assets[0][0] + datum.assets[0][1];
  let poolCoinB = datum.assets[1][0] + datum.assets[1][1];
  if (datum.assets[0][0] == "") {
    poolCoinA = "lovelace";
  }

  let poolAmountA = targetPool.assets[poolCoinA];
  if (poolCoinA == "lovelace") {
    poolAmountA -= datum.protocolFees;
  }

  let poolAmountB = targetPool.assets[poolCoinB];

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
  destination: Address,
};

async function addressPlutusToLucid(lucid: Lucid, addr: any): Promise<Address> {
  let paymentCred: Credential | null = null;
  if (addr.paymentCredential.VKeyCredential) {
    paymentCred = { type: "Key", hash: addr.paymentCredential.VKeyCredential.bytes };
  } else if (addr.paymentCredential.SCredential) {
    paymentCred = { type: "Script", hash: addr.paymentCredential.SCredential.bytes };
  } else {
    throw new Error("couldn't convert order datum address for lucid");
  }

  let stakingCred: Credential | null = null;
  if (addr.stakeCredential == null) {
    // That's ok 
  } else if (addr.stakeCredential.VKeyCredential) {
    stakingCred = { type: "Key", hash: addr.stakeCredential.VKeyCredential.bytes };
  } else if (addr.stakeCredential.SCredential) {
    stakingCred = { type: "Script", hash: addr.stakeCredential.SCredential.bytes };
  } else {
    throw new Error("couldn't convert order datum address for lucid: stake cred invalid");
  }
  console.log("debug: ");
  console.log(paymentCred);
  console.log(stakingCred);
  if (stakingCred) {
    return lucid.utils.credentialToAddress(paymentCred, stakingCred);
  } else {
    return lucid.utils.credentialToAddress(paymentCred);
  }
}

async function executeOrder(lucid: Lucid, poolABL: ABL, poolDatum: types.PoolDatum, order: UTxO): Promise<[ABL, EscrowTakes]> {
  console.log("poolABL is: ");
  console.log(poolABL);
  let poolCoinA = poolDatum.assets[0][0] + poolDatum.assets[0][1];
  let poolCoinB = poolDatum.assets[1][0] + poolDatum.assets[1][1];
  if (!order.datum) {
    throw new Error("executeOrder: order utxo had no datum");
  }
  let orderDatum: types.OrderDatum = Data.from(order.datum, types.OrderDatum);
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
  const dest = await addressPlutusToLucid(lucid, orderDatum.destination.Fixed.address);
  return [poolABL, {
    abl: res,
    destination: dest,
  }];
}

async function updateABL(lucid: Lucid, poolABL: ABL, poolDatum: types.PoolDatum, orders: UTxO[]): Promise<[ABL, EscrowTakes[]]> {
  orders.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));
  let takes: EscrowTakes[] = [];
  let currentPoolABL: ABL = {
    a: poolABL.a,
    b: poolABL.b,
    liq: poolABL.liq,
  };
  let take = null;
  for (let o of orders) {
    [currentPoolABL, take] = await executeOrder(lucid, currentPoolABL, poolDatum, o);
    takes.push(take);
  }
  return [currentPoolABL, takes];
}

async function scoopPool(scripts: Scripts, lucid: Lucid, userAddress: Address, settings: UTxO, orderUtxos: UTxO[], targetPool: UTxO, references: UTxO[], changeUtxo: UTxO): Promise<TxHash> {
  console.log("settings: ");
  console.log(settings);

  console.log("targetPool: ");
  console.log(targetPool);

  const settingsDatum = Data.from(settings.datum, types.SettingsDatum);
  const protocolBaseFee = settingsDatum.baseFee;
  const simpleFee = settingsDatum.simpleFee;
  const ordersCount: bigint = BigInt(orderUtxos.length);
  const rider = 2_000_000n;
  const indexingSet = computeIndexingSet(scripts, changeUtxo, targetPool, orderUtxos);
  console.log("indexingSet: ");
  console.log(indexingSet);
  const amortizedBaseFee = (protocolBaseFee + ordersCount - 1n) / ordersCount;
  const scoopPoolRedeemer: types.PoolRedeemer = {
    signatoryIndex: 0n,
    scooperIndex: 0n,
    inputOrder: indexingSet,
  };
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
  let poolDatum = Data.from(targetPool.datum, types.PoolDatum);
  const poolCoinA = poolDatum.assets[0][0] + poolDatum.assets[0][1];
  const poolCoinB = poolDatum.assets[1][0] + poolDatum.assets[1][1];
  console.log("pool coins: ", poolCoinA, poolCoinB);
  const poolABL = getPoolABL(targetPool, poolDatum);
  const [newPoolABL, escrowTakes] = await updateABL(lucid, poolABL, poolDatum, orderUtxos);
  console.log("newPoolABL: ");
  console.log(newPoolABL);
  const totalProtocolFees = (amortizedBaseFee + simpleFee) * ordersCount;
  console.log(`total protocol fees: ${totalProtocolFees}`);
  const mintedLiq = newPoolABL.liq - poolABL.liq;
  console.log("mintedLiq: ");
  console.log(mintedLiq);
  const oldRewards = poolDatum.protocolFees;
  poolDatum.protocolFees += totalProtocolFees;
  poolDatum.circulatingLp = newPoolABL.liq;
  const poolNftNameHex = computePoolNftName(fromHex(poolDatum.identifier));
  const poolLqNameHex = computePoolLqName(fromHex(poolDatum.identifier));
  let tx = await lucid.newTx();
  let toSpend = [];
  toSpend.push(changeUtxo);
  toSpend.push(targetPool);
  toSpend.push(...orderUtxos);
  toSpend.sort((a, b) => a.txHash == b.txHash ? a.outputIndex - b.outputIndex : (a.txHash < b.txHash ? -1 : 1));
  for (let e of toSpend) {
    if (getAddressDetails(e.address).paymentCredential.hash == scripts.poolScriptHash) {
      tx.collectFrom([e], redeemerData);
    } else if (getAddressDetails(e.address).paymentCredential.hash == scripts.orderScriptHash) {
      tx.collectFrom([e], Data.to(orderScoopRedeemer, types.OrderRedeemer));
    } else {
      tx.collectFrom([e]);
    }
  }

  const currentTime = emulator.now();
  console.log("new pool datum: ");
  let newPoolDatum = Data.to(poolDatum, types.PoolDatum);
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
    .attachSpendingValidator(scripts.steakValidator)
    .addSigner(userAddress)
    .withdraw(scripts.steakAddress, 0n, "00")

    .payToContract(targetPool.address, { inline: newPoolDatum }, {
      "lovelace":
        newPoolABL.a +
        poolDatum.protocolFees,
      [poolCoinB]: newPoolABL.b,
      [toUnit(scripts.poolPolicyId, poolNftNameHex)]: 1n,
    });

  if (mintedLiq != 0n) {
    tx.attachMintingPolicy(scripts.poolMint);
    const poolMintRedeemer: types.PoolMintRedeemer = {
      MintLP: {
        identifier: poolDatum.identifier,
      }
    };
    console.log("poolMintRedeemer: ");
    console.log(poolMintRedeemer);
    tx.mintAssets({
      [toUnit(scripts.poolPolicyId, poolLqNameHex)]: mintedLiq,
    }, Data.to(poolMintRedeemer, types.PoolMintRedeemer));
  }

  console.log("pool out datum: ");
  console.log(Data.to(poolDatum, types.PoolDatum));

  // We add the escrows to the order in reverse, because in the script, prepending to the list is cheaper
  for (let e of escrowTakes) {
    let valueOut: Assets = { "lovelace": rider + (1_000_000n - amortizedBaseFee) + e.abl.a };
    if (e.abl.b > 0n) {
      valueOut[poolCoinB] = e.abl.b;
    }
    if (e.abl.liq > 0n) {
      valueOut[toUnit(scripts.poolPolicyId, poolLqNameHex)] = e.abl.liq;
    }
    tx.payToAddress(e.destination, valueOut);
  }
  const str = await tx.toString();
  console.log("building tx: " + str);
  const completed = await tx.complete({
    coinSelection: false, // We don't want extra inputs screwing up the indexing set
    //nativeUplc: false, // "Lucid breaks with stake scripts"?
  });
  const signedTx = await completed.sign().complete();
  const exUnits = completed.exUnits;
  const signedStr = await signedTx.toString();
  console.log("signed tx: " + signedStr);
  const scoopedHash = signedTx.submit();
  await emulator.awaitTx(scoopedHash);
  return exUnits;
}

async function testScoopPool(lucid: Lucid, emulator: Emulator, scripts: Scripts, poolIdentHex: string, change: UTxO, references: UTxO[], orders: UTxO[]) {
  const dummy = await Lucid.new(undefined, "Custom");
  const [userAddress, userPkh, userPrivateKey] = fakeAddress(dummy);

  let settingsUtxos = await emulator.getUtxos(scripts.settingsAddress);
  if (settingsUtxos.length == 0) {
    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
  }
  if (settingsUtxos.length > 1) {
    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
  }
  const settings = settingsUtxos[0];

  const poolAddress = (new Utils(lucid)).credentialToAddress(
    {
      type: "Script",
      hash: scripts.poolScriptHash,
    },
    {
      type: "Script",
      hash: scripts.poolStakeHash,
    }
  );

  let knownPools = await emulator.getUtxos(poolAddress);

  let targetPool = null;
  for (let knownPool of knownPools) {
    let targetAssetName = computePoolNftName(fromHex(poolIdentHex));
    let targetPolicyId = scripts.poolScriptHash;
    let targetNftUnit = targetPolicyId + targetAssetName;
    let amountOfTargetNft = knownPool.assets[targetNftUnit];
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

async function testMintRberry(lucid: Lucid, emulator: Emulator, scripts: Scripts) {
  const dummy = await Lucid.new(undefined, "Custom");
  const [userAddress, userPkh, userPrivateKey] = fakeAddress(dummy);
  const mintedHash = await mintRberry(scripts, lucid, userAddress);
  await emulator.awaitTx(mintedHash);
  console.log("minted rberry, hash: " + mintedHash);
  const mintedUtxos = await emulator.getUtxosByOutRef([{
    txHash: mintedHash,
    outputIndex: 0,
  }]);
  return mintedUtxos[0];
}

async function fundUserAddress(lucid: Lucid, emulator: Emulator, scripts: Scripts) {
 // const dummy = await 
}

async function testPostReferenceScript(lucid: Lucid, emulator: Emulator, scripts: Scripts, scriptName: string) {
  const dummy = await Lucid.new(undefined, "Custom");
  const [userAddress, userPkh, userPrivateKey] = fakeAddress(dummy);
  const change = await findChange(emulator, userAddress);

  const settingsUtxos = await emulator.getUtxos(scripts.settingsAddress);

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

async function buildSSE() {
  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Preview");

  const sk = await Deno.readTextFile(flags.privateKey);
  const skCborHex = JSON.parse(sk).cborHex;
  const skHex = skCborHex.slice(4);
  const skBech32 = C.PrivateKey.from_bytes(fromHex(skCborHex)).to_bech32();
  const userPublicKey = toPublicKey(skBech32);
  const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
  const userAddress = lucid.utils.credentialToAddress({
    type: "Key",
    hash: userPkh.to_hex(),
  });

  console.log("pkh: ", userPkh.to_hex());
  console.log("address: ", userAddress);

  const utxo = flags.utxo;
  //const validAfter = flags.validAfter;
  //const validUntil = flags.validUntil;

  let assets: CoinPair = [
    assetFromString(flags.coinA),
    assetFromString(flags.coinB),
  ];

  const [orderHash, orderIx] = utxo.split("#");
  const matches = await lucid.provider.getUtxosByOutRef([{
    txHash: orderHash,
    outputIndex: Number(orderIx),
  }]);
  if (matches.length == 0) {
    throw new Error(`Couldn't find utxo: ${orderHash}#${orderIx}`);
  }

  // TODO: utxo should be an order so we can check the funds on it

  const strategyExecution: StrategyExecution = {
    txRef: {
      transactionId: {
        hash: orderHash,
      },
      outputIndex: BigInt(orderIx),
    },
    validityRange: {
      lowerBound: {
        boundType: { NegativeInfinity: [] },
        isInclusive: true,
      },
      upperBound: {
        boundType: { PositiveInfinity: [] },
        isInclusive: true,
      },
    },
    details: {
      Swap: {
        offer: [assets[0][0], assets[0][1], BigInt(flags.gives)],
        minReceived: [assets[1][0], assets[1][1], 0n],
      },
    },
    extensions: "NoExtension",
  };

  const strategyExecutionBytes = Data.to(strategyExecution, types.StrategyExecution);

  // No longer need to do this!!!
  // const orderDetailsBytes = Data.to(strategyExecution.details, types.Details);

  const { address: { hex: hexAddress } } =
    lucid.utils.getAddressDetails(userAddress);

  const signature = await signAsync(strategyExecutionBytes, skHex);
  
  const signedStrategyExecution: SignedStrategyExecution = {
    strategy: strategyExecution,
    signature: toHex(signature),
  };

  const sseBytes = Data.to(signedStrategyExecution, types.SignedStrategyExecution);

  console.log(`built a strategy execution: ${strategyExecutionBytes}`);
  console.log(`signature: ${toHex(signature)}`);
  console.log(`sse: ${sseBytes}`);
}

async function doPostReferenceScript(scriptName: string) {
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }
  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);
  const blockfrost = new Blockfrost(flags.blockfrostUrl as string, flags.blockfrostProjectId as string);
  const lucid = await Lucid.new(blockfrost, "Mainnet");
  const scripts = getScriptsAiken(lucid, scriptsJson);

  const address = flags.address;

if (flags.submit) {
  const sk = await Deno.readTextFile(flags.privateKeyFile);
  const skCborHex = JSON.parse(sk).cborHex;
  const skBech32 = C.PrivateKey.from_bytes(fromHex(skCborHex)).to_bech32();
  const userPublicKey = toPublicKey(skBech32);
  const userPkh = C.PublicKey.from_bech32(userPublicKey).hash();
  const userAddress = lucid.utils.credentialToAddress({
    type: "Key",
    hash: userPkh.to_hex(),
  });
  lucid.selectWalletFromPrivateKey(skBech32);
}

else {
    lucid.selectWalletFrom({
      address: address,
    });
  }

  const change = await findChange(lucid.provider, address);

  console.log("scripts.settingsAddress: " + scripts.settingsAddress)
  const settingsUtxos = await lucid.provider.getUtxos(scripts.settingsAddress);

//  if (settingsUtxos.length == 0) {
//    throw new Error("Couldn't find any settings utxos: " + scripts.settingsAddress);
//  }
//  if (settingsUtxos.length > 1) {
//    throw new Error("Multiple utxos at the settings address, I don't know which one to choose");
//  }
//
//  const settings = settingsUtxos[0];
//
  let settings = undefined;
  const postedHash = await postReferenceScript(scripts, lucid, address, scriptName, change, settings);
  await lucid.provider.awaitTx(postedHash);
  console.log("Posted reference script, hash: " + postedHash);
  const postedUtxos = await emulator.getUtxosByOutRef([{
    txHash: postedHash,
    outputIndex: 0,
  }]);
  return postedUtxos[0];
}

async function findChange(provider: Provider, userAddress: string): Promise<UTxO> {
  let startTime = Date.now();
  let changeUtxos = await provider.getUtxos(userAddress);
  let endTime = Date.now();
  //console.log(`Fetched utxos from wallet, time elapsed: ${endTime - startTime}ms`);
  for (let changeUtxo of changeUtxos) {
    //console.log(changeUtxo);
    if (changeUtxo.datum != null && changeUtxo.datumHash != null) {
      continue;
    }
    if (changeUtxo.scriptRef != null) {
      continue;
    }
    if (Object.keys(changeUtxo.assets).length > 1) {
      continue; // Don't want native assets
    }
    if (changeUtxo.assets["lovelace"] >= 40_000_000n) {
      //console.log("changeUtxo:");
      //console.log(changeUtxo);
      return changeUtxo;
    }
  }
  throw new Error("findChange: Couldn't find a UTxO without a datum or script ref in the user wallet with over 200 ADA.");
}

async function findSettings(provider: Provider, settingsAddress: string, settingsPolicyId: string): Promise<UTxO> {
  let settingsUtxos = await provider.getUtxos(settingsAddress);
  for (let settingsUtxo of settingsUtxos) {
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

async function findOrders(provider: Provider, orderAddress: string): Promise<UTxO[]> {
  let orderUtxos = await provider.getUtxos(orderAddress);
  let result: UTxO[] = [];
  for (let orderUtxo of orderUtxos) {
    let orderDatum: types.OrderDatum | null = null;
    try {
      orderDatum = Data.from(orderUtxo.datum as string, types.OrderDatum);
    } catch (e) {
      console.log("findOrders: UTxO at order address had invalid datum (skipping)");
      continue;
    }
    result.push(orderUtxo);
  }
  return result;
}

async function previewRecordOrderDebug() {
  const accounts: any[] = [];
  let emulator = new Emulator(accounts, {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxTxSize: 999999999999,
    maxTxExMem: 999999999999999n,
  });
  let lucid = await Lucid.new(emulator);

  const userAddress = "addr_test1vqp4mmnx647vyutfwugav0yvxhl6pdkyg69x4xqzfl4vwwck92a9t";

  let orderSpendScriptRefBytes = await Deno.readTextFile(flags.orderSpend);
  let orderSpendScriptRef = {
    type: "PlutusV2",
    script: orderSpendScriptRefBytes
  };
  let poolSpendScriptRefBytes = await Deno.readTextFile(flags.poolSpend);
  let poolSpendScriptRef = {
    type: "PlutusV2",
    script: poolSpendScriptRefBytes
  };

  let rberry = toUnit(
    "99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e15",
    fromText("RBERRY")
  );

  let sberry = toUnit(
    "99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e15",
    fromText("SBERRY")
  );

  const poolnft = toUnit(
    "44a1eb2d9f58add4eb1932bd0048e6a1947e85e3fe4f32956a110414",
    "000de140bcc8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef767"
  );

  // This is not in the original tx but im adding it here to try to get lucid to
  // stop complaining about collateral
  emulator.ledger["c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b441"] = {
    utxo: {
      txHash: "c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b44",
      outputIndex: 1n,
      address: userAddress, 
      assets: { lovelace: 100_000_000n },
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };
  emulator.ledger["c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b442"] = {
    utxo: {
      txHash: "c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b44",
      outputIndex: 2n,
      address: userAddress,
      assets: { lovelace: 100_000_000n },
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };

  emulator.ledger["b251fe5510e5237736ac2093e6c6001dd2aaa883d0c03cd8c466a0b3b63e90f62"] = {
    utxo: {
      txHash: "b251fe5510e5237736ac2093e6c6001dd2aaa883d0c03cd8c466a0b3b63e90f6",
      outputIndex: 2n,
      address: userAddress,
      assets: { lovelace: 4_542_717_456n },
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };

  // POOL
  emulator.ledger["2a97101e262ae73c82f1815e96c91282b159279d7c42b298baa4023153562ace0"] = {
    utxo: {
      txHash: "2a97101e262ae73c82f1815e96c91282b159279d7c42b298baa4023153562ace",
      outputIndex: 0n,
      address: "addr_test1xpz2r6ednav2m48tryet6qzgu6segl59u0ly7v54dggsg9xvy7vq4p2hl6wm9jdvpgn80ax3xpkm7yrgnxphtrct3klq005j2r",
      assets: {
        lovelace: 3_500_000n,
        [rberry]: 100_001_000n,
        [sberry]: 99_999_006n,
        [poolnft]: 1n,
      },
      datumHash: undefined,
      datum: "d8799f581cbcc8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef7679f9f581c99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e1546524245525259ff9f581c99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e1546534245525259ffff1a05f5e1001832181ed8799fd8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bffff001a003567e0ff",
      scriptRef: undefined
    },
    spent: false
  };


  const originalOrderDatum = "d8799fd8799f581cbcc8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef767ffd8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bff1a0010c8e0d8799fd8799fd87a9f581c75b05727d2c714297863d1c251190a82812f05008fc619af2cc9369dffd87a80ffd87980ffd87e9f9f581c63d77b5d97ce0b0bc4e70cb4ce885813d9db9901b0a4c3570a69612c466f7261636c65ffffd87980ff";
  const fixedOrderDatum = 
    "d8799fd8799f581cbcc8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef767ffd8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bff1a0010c8e0d8799fd8799fd87a9f581c75b05727d2c714297863d1c251190a82812f05008fc619af2cc9369dffd87a80ffd87980ffd87e9f9f581c63d77b5d97ce0b0bc4e70cb4ce885813d9db9901b0a4c3570a69612c466f7261636c65ffffd8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bffff";

  // ORDER
  emulator.ledger["70c17c4e12f2ca53c887faf887f49943f1bfcab1746916e5a6feebf4799a40d90"] = {
    utxo: {
      txHash: "70c17c4e12f2ca53c887faf887f49943f1bfcab1746916e5a6feebf4799a40d9",
      outputIndex: 0n,
      address: "addr_test1wr866xg5kkvarzll69xjh0tfvqvu9zvuhht2qve9ehmgp0qfgf3wc",
      assets: {
        lovelace: 3_100_000n,
      },
      datumHash: undefined,
      datum: fixedOrderDatum,
      scriptRef: undefined
    },
    spent: false
  };

  emulator.ledger["92ec2274938de291d3837b7facf9eddfaed57cd6ff97e26af57cb7a9978e38870"] = {
    utxo: {
      txHash: "92ec2274938de291d3837b7facf9eddfaed57cd6ff97e26af57cb7a9978e3887",
      outputIndex: 0n,
      address: "addr_test1vzkjjdguttg6lpwqwkup0ldh6hg2pvspen6tsvpcy8psn7gnv7r5n",
      assets: {
        lovelace: 11_546_490n, 
      },
      datumHash: undefined,
      datum: undefined,
      scriptRef: orderSpendScriptRef
    },
    spent: false
  };
  emulator.ledger["8036a88a61427262aba964a42d0b9924739ffc3214de9a07c54b5a09af7f0d7d0"] = {
    utxo: {
      txHash: "8036a88a61427262aba964a42d0b9924739ffc3214de9a07c54b5a09af7f0d7d",
      outputIndex: 0n,
      address: "addr_test1vzkjjdguttg6lpwqwkup0ldh6hg2pvspen6tsvpcy8psn7gnv7r5n",
      assets: {
        lovelace: 68_692_780n, 
      },
      datumHash: undefined,
      datum: undefined,
      scriptRef: poolSpendScriptRef
    },
    spent: false
  };
  emulator.ledger["3a2d9f8573018e62929efbefc5b975aed6a84b3a3812eb301a296a41e11ef2640"] = {
    utxo: {
      txHash: "3a2d9f8573018e62929efbefc5b975aed6a84b3a3812eb301a296a41e11ef264",
      outputIndex: 0n,
      address: "addr_test1wzz76rrsvrxdguqfylvtvrcpvz479v7rq3r0cz56eqakkasu3f7n0",
      assets: {
        lovelace: 2_137_760n,
        [toUnit("85ed0c7060ccd4700927d8b60f0160abe2b3c30446fc0a9ac83b6b76", fromText("settings"))]: 1n,
      },
      datumHash: undefined,
      datum: "d8799fd8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bffd8799fd8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bffd87a80ffd8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bffd8799fd8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bffd87a80ff9f010affd8799f9f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bffff9fd87a9f581ccc27980a8557fe9db2c9ac0a2677f4d1306dbf10689983758f0b8dbeffff1a000510e01a000290401a000290400000ff",
      scriptRef: undefined
    },
    spent: false
  };

  const change =
    emulator.ledger["b251fe5510e5237736ac2093e6c6001dd2aaa883d0c03cd8c466a0b3b63e90f62"].utxo;
  const order = 
    emulator.ledger["70c17c4e12f2ca53c887faf887f49943f1bfcab1746916e5a6feebf4799a40d90"].utxo;
  const pool = 
    emulator.ledger["2a97101e262ae73c82f1815e96c91282b159279d7c42b298baa4023153562ace0"].utxo;

  const references =
    [
      emulator.ledger["8036a88a61427262aba964a42d0b9924739ffc3214de9a07c54b5a09af7f0d7d0"].utxo,
      emulator.ledger["92ec2274938de291d3837b7facf9eddfaed57cd6ff97e26af57cb7a9978e38870"].utxo
    ];

  const settings =
    emulator.ledger["3a2d9f8573018e62929efbefc5b975aed6a84b3a3812eb301a296a41e11ef2640"].utxo;

  const newPoolDatum =
    "d8799f581cbcc8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef7679f9f581c99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e1546524245525259ff9f581c99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e1546534245525259ffff1a05f5e1001832181ed8799fd8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bffff001a003d0900ff";

  const badNewOracleDatum_WrongFormat = "d8799f00d8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bffd8799fd8799fd87a9f1b0000018f59d7eff8ffd87980ffd8799fd87a9f1b0000018f59f67478ffd87980ffff581cbce8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef7679f581c99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e15465242455252591a05f5e4e8ff9f581c99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e15465342455252591a05f5dd1eff9f581c44a1eb2d9f58add4eb1932bd0048e6a1947e85e3fe4f32956a11041458200014df10bcc8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef7671a05f5e100ff00ff";
  const badNewOracleDatum_WrongPoolIdent = "d8799fd8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bffd8799fd8799fd87a9f1b0000018f59d7eff8ffd87980ffd8799fd87a9f1b0000018f59f67478ffd87980ffff581cbce8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef7679f581c99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e15465242455252591a05f5e4e8ff9f581c99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e15465342455252591a05f5dd1eff9f581c44a1eb2d9f58add4eb1932bd0048e6a1947e85e3fe4f32956a11041458200014df10bcc8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef7671a05f5e100ffff";
  const newOracleDatum = "d8799fd8799f581c035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73bffd8799fd8799fd87a9f1b0000018f59d7eff8ffd87980ffd8799fd87a9f1b0000018f59f67478ffd87980ffff581cbcc8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef7679f581c99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e15465242455252591a05f5e4e8ff9f581c99b071ce8580d6a3a11b4902145adb8bfd0d2a03935af8cf66403e15465342455252591a05f5dd1eff9f581c44a1eb2d9f58add4eb1932bd0048e6a1947e85e3fe4f32956a11041458200014df10bcc8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef7671a05f5e100ffff";

  const poolAddr = "addr_test1xpz2r6ednav2m48tryet6qzgu6segl59u0ly7v54dggsg9xvy7vq4p2hl6wm9jdvpgn80ax3xpkm7yrgnxphtrct3klq005j2r";

  const orderDestination = "addr_test1wp3aw76ajl8qkz7yuuxtfn5gtqfankueqxc2fs6hpf5kztqz244zc";
  const stakeAddress = "stake_test17r9rqul4mupvvpqy5826pfzpduxs9zxuax023pa9a0kp3fc28mvvx";

  const stakeValidator = {
    type: "PlutusV2",
    script: "5901420100003323232323232322322253330053253330063370e900218039baa300130083754004264a66600e66e1d2000300837540022646600200264a66601266e1d2002300a3754002297adef6c6013756601c60166ea8004c8cc004004dd5980218059baa300e300b375400644a66601a0022980103d87a80001323232533300d3371e0166eb8c03800c4cdd2a4000660226e980052f5c026600a00a0046eacc038008c044008c03c004894ccc030004528099299980519b873371c6eb8c02cc03c00920024806852889980180180098078008b1929998050008a6103d87a800013374a9000198059806000a5eb80dd618059806180618041baa300b3008375400429408c02cc03000452613656375c002ae6955ceaab9e5573eae815d0aba24c011e581c44a1eb2d9f58add4eb1932bd0048e6a1947e85e3fe4f32956a1104140001"
  };

  const oracleValidator = {
    type: "PlutusV2",
    script: "5915fe010000332323232323232232225323232323232323233300d3001300e375401226464a66601ea66601e600660206ea80304c8c8c8c8c94ccc050c020c054dd5000899191919191919299980d9807980e1baa001132533301c3375e6008603c6ea8c010c078dd5000980b998101ba901a4bd70099299980e98091998009bab300b301f375400403600c264a66603c6028603e6ea80044c8c8c8c8c8c8c8c8c8c8c8cdc42400060546ea8c0acc8ccc004004dd6180898161baa3012302c375404c97bdb1810100000103d87a8000222533302f00210011333003003303200232323232533303030240011337606ea000cccc0c0009300103d87a80004c0103d87980001533303030250011325333031302530323754002264a666064604c60666ea80044c8c8c94ccc0d4c0a4c0d8dd500089919191919299981d19baf3022303c3754604460786ea8038c0d4cc0f8dd481325eb804c94ccc0ecc0bcc0f0dd500089919299981e99baf302b303f375400460846086608660866086608660866086607e6ea8c094c0fcdd501c8a99981e99b8f375c6048607e6ea80080b454ccc0f4cdd79812981f9baa0020051533303d3375e6e9c05cc108c10cc10cc10cc0fcdd50010a99981e99baf374e02c60846086608660866086607e6ea800854ccc0f4cdd79ba70153006303f3754004266ec0dd419b8001048008ccc0f403d30103d87a80004c0103d879800016161616161632533303d3031303e37540042646464646464646464646464a666098609e00426464646464931919191919299982a982c0010a4c2c6eb4c158004c158008dd7182a000982a0031bae3052005323232323253330543057002149858dd6982a800982a8011bae30530013053007375c60a200c6464646464a6660a660ac0042930b1bad30540013054002375c60a400260a40106eb8c14001d4ccc128c0f8c12cdd50050991919192999828982a0010991924c602600460240062c60a400260a400460a000260986ea802858c0a402c58dd6182680098268011bac304b001304b0023758609200260920046eb8c11c004c11c008c114004c114008c10c004c0fcdd50010b1299981e9818981f1baa00113232323253330443047002132498c94ccc108c0d800454ccc114c110dd50020a4c2c2a666084606e00226464a66608e60940042930b1bad3048001304437540082a66608460700022a66608a60886ea80105261616304237540062c64a66608860860022a666082606c6084002294454ccc104c0d4c1080045280b0b1baa304500130450023043001303f37540022c6080607a6ea800458cc01cdd59804181e1baa3022303c375406c01c2c6032002600260746ea800c8c0f4c0f8c0f8c0f8c0f8c0f8004c94ccc0dcc0acc0e0dd500109919191919191919191919192999823182480109919191924c64a66608e607600226464a666098609e00426493192999825181f00089919299982798290010a4c2c6eb8c140004c130dd50010a999825181f80089919299982798290010a4c2c6eb8c140004c130dd50010b18251baa00116304d0013049375400e2a66608e6078002264646464a66609c60a200426464931919191919299982a182b8010a4c2c6eb4c154004c154008dd7182980098298019bae3051002323232323253330533056002149858dd6982a000982a0011bae30520013052004375c60a00062c6eb0c13c004c13c008dd6182680098249baa00715333047303d00113232533304c304f002132498c8c8c8c8c8c8c8c94ccc150c15c00852616375a60aa00260aa0046eb8c14c004c14c00cdd7182880119191919192999829982b0010a4c2c6eb4c150004c150008dd7182900098290019bae3050002375860980046eb0c12800458c94ccc130c13cc13c0044cdd81827000982718278008b1bac304d0013049375400e2a66608e608000226464a666098609e0042649319191919192999828982a0010a4c2c6eb4c148004c148008dd7182800098280011bae304e001163758609a00260926ea801c54ccc11cc0fc0044c8c94ccc130c13c0084c926323232323232323253330543057002149858dd6982a800982a8011bae30530013053003375c60a20046464646464a6660a660ac0042930b1bad30540013054002375c60a400260a40066eb8c140008dd618260011bac304a0011632533304c304f304f001133760609c002609c609e0022c6eb0c134004c124dd50038a999823981f000899192999826182780109924c646eb8c130008dd718250008b19299982618279827800899bb0304e001304e304f001163758609a00260926ea801c58c11cdd5003192999823181d000899191919299982698280010991924c64a66609860800022a66609e609c6ea800c526161533304c30410011323253330513054002149858dd7182900098271baa0031533304c30420011323253330513054002149858c148004c138dd50018b18261baa002533304a303e304b3754006264646464a6660a260a80042646493192999828182200089919299982a982c00109924c64a6660a6608e00226464a6660b060b600426493180f8008b182c800982a9baa0021533305330480011323232323232533305c305f002149858dd6982e800982e8011bad305b001305b002375a60b200260aa6ea800858c14cdd50008b182b00098291baa00315333050304500115333053305237540062930b0b18281baa002301800316305200130520023050001304c37540062c2c609c002609c004609800260906ea802054ccc118c0ec00454ccc124c120dd50040a4c2c2c608c6ea801cc090028c94ccc110c0e00044c8c94ccc124c13000852616375c6094002608c6ea803054ccc110c0e400454ccc11cc118dd50060a4c2c2c60886ea802c58c11c004c11c008c114004c114008c10c004c10c008dd698208009820801181f800981f801181e800981c9baa002162325333038302c00113232533303d3040002149858dd7181f000981d1baa00215333038302d00113232533303d3040002149858dd7181f000981d1baa00216303837540026074606e6ea800458cc004dd59801181b1baa301c303637540606044606c6ea8c0e4c0d8dd5001911919299981b98158008a6103d87a800015333037302c001132323300100100622533303d00114c0103d87a80001323232533303d3371e00c6eb8c0f800c4c0dccc1040052f5c026600a00a004607c0046082004607e0026eb8c0f0c0e4dd5001098189981d981e181c9baa0024bd70181b9baa001301c303737540024607060726072607260726072607260726072607260720022c66646002002444a66606e004298103d87a8000132325333036302a003130303303a0024bd70099980280280099b8000348004c0ec00cc0e4008dd6180c98199baa30193033375405a6eb4c0d8c0ccdd50008b19991800800911299981b0010a60103d87a800013232533303530290031302f33039375000497ae01333005005001337000069000981d0019bad303800201f00314bded8c010100000103d87980003330133756603a60626ea800c06d2201066f7261636c650033710900018179baa3030003375a605c0046062004660586ea4098cc0b0dd4809198161816981700325eb80cc0acc0a0008cc0acc0a4008cc0acdd41998059bab3015302937540186eb8c0a0008dd7181480125eb80cc0a8c09c008cc0a8c0a0008cc0a8dd41998051bab3014302837540166eb8c09c008dd7181400125eb80c8cdd81815800981598160009bac302700232337606054002605460560026eb0c094004c8cdd81814800981498150009bac3028001302830243754008a666042602a60446ea800c4c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c94ccc0d0c0dc0084c8c9263253330333027001132325333038303b002132498c05400458c0e4004c0d4dd50038a99981998140008a99981b181a9baa00714985858c0ccdd5003191919191bae3037003375c606a004646eb8c0d800cdd7181a0011919bb03038001303830390013758606801c6466ec0c0dc004c0dcc0e0004dd618190068b1bad30350013035002375a60660026066004606200260620046eb4c0bc004c0bc008dd6981680098168011bad302b001302b002325333028302b302b0011337606054002605460560022c6eb0c0a4004c0a4008dd7181380098119baa003163001001223253330223016001132325333027302a002149858dd7181400098121baa002153330223017001132325333027302a002132498cc05c0048cc01801800458dd6181400098121baa002153330223018001132325333027302a002132498cc05c0048cc01801800458dd6181400098121baa00215333022301b0011323232325333029302c002132498cc0640048cc02002000458dd6181500098150011bad3028001302437540042a666044603400226464a66604e60540042930b1bad3028001302437540042a666044603200226464a66604e60540042930b1bad3028001302437540042a66604466e1d200c001132325333027302a002149858dd7181400098121baa0021630223754002604660406ea800458c010c07cdd50010b111192999810180a98109baa0011480004dd6981298111baa0013253330203015302137540022980103d87a8000132330010013756604c60466ea8008894ccc094004530103d87a8000132323253330253371e00e6eb8c09800c4c07ccc0a4dd4000a5eb804cc014014008dd698130011814801181380099198008008021129998120008a6103d87a8000132323253330243371e00e6eb8c09400c4c078cc0a0dd3000a5eb804cc014014008dd59812801181400118130008b1810180e9baa0011632533301e00114c103d87a8000130153301f30200014bd701bac3001301c3754600460386ea80588c07cc080c0800048c078004cc009220104000de14000007330014881040014df100000622337140040026eb8c064c058dd50008b1800980a9baa00f2301830190013758602c602e0046eb8c054004c044dd50060a5114984d958c94ccc03cc00c0044c8c8c8c94ccc058c0640084c926330060012375a0022c6eb0c05c004c05c008dd7180a80098089baa00c1533300f300400115333012301137540182930b0b18079baa00b22323300100100322533301400114984c8cc00c00cc060008c00cc0580044cc8894ccc040c8c8c94ccc04cc020c050dd500089919299980a9999911119198008008029119299980e180800089919198008008041129998110008a5013253330203371e6eb8c09400801052889980180180098128009bae3021301e37540042a66603860220022660106eb0c084c078dd50011198020020008a99980e1809000899198008009bac3022301f375400644a66604200229404c94ccc07ccc018018c09000852889980180180098120008a99980e180a80089919b89375a6044002646660020026eb0c08cc09000920002225333023002100113330030033026002533302033007007302500213370000290010800980f1baa0021533301c301400113232533301e3013301f3754002264a66603e64a66604660440022a666040602a6042002294454ccc080c050c0840045280b0b1baa300f30213754601e60426ea80204cdc4800801899b88001003375a604660406ea80045281806180f9baa300d301f375400c6eb4c084c078dd50010a99980e180980089919299980f1809980f9baa001132533301f3253330233022001153330203015302100114a22a6660406028604200229405858dd5180798109baa300e30213754010266e2400c0044cdc40018009bad3023302037540022940c030c07cdd51806180f9baa006375a6042603c6ea80084c8c8cc004004018894ccc088004528099299981019baf0043021302500214a2266006006002604a002602c66040602e660406042603c6ea80092f5c097ae0301c37540026008602e6ea8048dd6180d180d980d980d980d980d980d980d980d980b9baa30043017375400c60346036603660366036603660366036602e6ea8c010c05cdd50031bab301a301b301b301b301b301b301b301737546008602e6ea80184cc004dd6180d180d980d980b9baa30043017375400c4601464a66602e601860306ea8004520001375a603860326ea8004c94ccc05cc030c060dd50008a6103d87a8000132330010013756603a60346ea8008894ccc070004530103d87a80001323232533301c3371e911066f7261636c6500375c603a0062602c660406ea00052f5c026600a00a0046eb4c074008c080008c078004c8cc004004dd59803980c9baa00222533301b00114c103d87a80001323232533301b3371e0106eb8c07000c4c054cc07cdd3000a5eb804cc014014008dd5980e001180f801180e8008a5022323300100100322533301b00114a2264a6660326008603c0042660060060022940c078004dd7180c180a9baa00116300130143754600260286ea8c94ccc04cc020c050dd5000899299980a1804180a9baa00113004301637546032602c6ea800458cc88c8cc00400400c894ccc0680045300103d87a80001323253330193375e601060366ea80080144c04ccc0740092f5c0266008008002603c00460380026eb0c008c054dd51801180a9baa0043018301537540022c600460286ea800c8c05c0048c058c05c0045261365632323232533301130053012375401c2646464646464646464646464a666040604600426464646464931919191919299981498160010a4c2c6eb4c0a8004c0a8008dd7181400098140031bae302600532323232325333028302b002149858dd6981480098148011bae30270013027007375c604a00c6464646464a66604e60540042930b1bad30280013028002375c604c002604c0106eb8c09001d4ccc078c048c07cdd5005099191919299981298140010991924c602a00460280062c604c002604c004604800260406ea802858c03402c58dd6181080098108011bac301f001301f0023758603a002603a0046eb8c06c004c06c008c064004c064008c05c004c04cdd50070b180080091192999809180300089919299980b980d0010a4c2c6eb8c060004c050dd50010a999809180380089919299980b980d00109924c6600e00246600c00c0022c6eb0c060004c050dd50010a999809180400089919299980b980d00109924c6600e00246600c00c0022c6eb0c060004c050dd50010a9998091805800899191919299980c980e00109924c660120024660100100022c6eb0c068004c068008dd6980c000980a1baa00215333012300a001132325333017301a002149858dd6980c000980a1baa002153330123009001132325333017301a002149858dd6980c000980a1baa002153330123370e900600089919299980b980d0010a4c2c6eb8c060004c050dd50010b18091baa0012533300f300330103754002264646464a66602c60320042649319299980a18040008a99980b980b1baa00414985854ccc050c0240044c8c94ccc064c07000852616375a6034002602c6ea801054ccc050c02800454ccc05cc058dd50020a4c2c2c60286ea800c58c94ccc058c05400454ccc04cc020c0500045288a9998099803980a0008a5016163754602e002602e004602a00260226ea80045888c8cc00400400c894ccc050004526132330030033018002300330160013012300f37540126e1d2000370e90011b8748010dc3a40146e1d2008370e90031ba548000dd2a40046eb80055cd2ab9d5573caae7d5d02ba157449811e581c44a1eb2d9f58add4eb1932bd0048e6a1947e85e3fe4f32956a1104140001",
  };
  
  console.log("oh baby");

  lucid.selectWalletFrom({
    address: userAddress,
  });
  
  const currentTime = emulator.now();
  console.log(`currentTime: ${currentTime}`);
  const tx = lucid.newTx();

  const goodOracleRedeemer = "d8799f581cbcc8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef7679f01ffff";
  const badOracleRedeemer = "d8799f581cbcc8b7c0512de3524b1760b5ae4b456b0ee99374279abeafe8eef7679f09ffff";

  tx
    .validFrom(currentTime + 48543275)
    .validTo(currentTime + 48545275)
    .readFrom([settings, ...references])
    .attachSpendingValidator(stakeValidator)
    .attachSpendingValidator(oracleValidator)
    .addSignerKey("035dee66d57cc271697711d63c8c35ffa0b6c4468a6a98024feac73b")
    .mintAssets({
      [toUnit("63d77b5d97ce0b0bc4e70cb4ce885813d9db9901b0a4c3570a69612c", fromText("oracle"))]: 1n,
    }, goodOracleRedeemer)
    .attachMintingPolicy(oracleValidator)
    .withdraw(stakeAddress, 0n, "00")
    .collectFrom([change])
    .collectFrom([pool], "d87a9fd8799f00009f9f01d87a8000ffffffff")
    .collectFrom([order], "d87980")
    .payToContract(poolAddr, { inline: newPoolDatum }, {
      "lovelace": 4_000_000n,
      [rberry]: 100_001_000n,
      [sberry]: 99_999_006n,
      [poolnft]: 1n
    })
    .payToContract(orderDestination, {
      inline: badNewOracleDatum_WrongFormat,
    },{
      "lovelace": 2_600_000n,
      [toUnit("63d77b5d97ce0b0bc4e70cb4ce885813d9db9901b0a4c3570a69612c", fromText("oracle"))]: 1n,
    })

  const str = await tx.toString();
  console.log("building tx: " + str);
  const completed = await tx.complete({
    coinSelection: false,
  });
  const completedStr = await completed.toString();
  console.log("completed: " + completedStr);
  //const signedTx = await completed.sign().complete();
  //const exUnits = completed.exUnits;
  //const signedStr = await signedTx.toString();
  //console.log("signed tx: " + signedStr);
  //const scoopedHash = signedTx.submit();
  //await emulator.awaitTx(scoopedHash);
  //return exUnits;

}

async function protov3ScoopDebugStrategy() {
  const accounts: any[] = [];
  let emulator = new Emulator(accounts, {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxTxSize: 999999999999,
    maxTxExMem: 999999999999999n,
  });
  let lucid = await Lucid.new(emulator);

  const userAddress = "addr1v92m7sgckq0pc72xgldexa0lepe7gdwhxuq8k2kmcjxdh2skxa0m4";

  let orderSpendScriptRefBytes = await Deno.readTextFile(flags.orderSpend);
  let orderSpendScriptRef = {
    type: "PlutusV2",
    script: orderSpendScriptRefBytes
  };
  let poolSpendScriptRefBytes = await Deno.readTextFile(flags.poolSpend);
  let poolSpendScriptRef = {
    type: "PlutusV2",
    script: poolSpendScriptRefBytes
  };

  console.log("oh baby 1");

  let rberry = toUnit(
    "99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e8",
    fromText("RBERRY")
  );

  let sberry = toUnit(
    "99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e8",
    fromText("SBERRY")
  );

  const poolnft = toUnit(
    "04123d867240ebbf6703317449fa06079ce747f09706773683cf01db",
    "000de1405e7df7e74c56adf93537657d744256a0fce543916837df34d9a98e41"
  );

  // This is not in the original tx but im adding it here to try to get lucid to
  // stop complaining about collateral
  emulator.ledger["c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b441"] = {
    utxo: {
      txHash: "c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b44",
      outputIndex: 1n,
      address: "addr1v92m7sgckq0pc72xgldexa0lepe7gdwhxuq8k2kmcjxdh2skxa0m4",
      assets: { lovelace: 100_000_000n },
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };
  emulator.ledger["c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b442"] = {
    utxo: {
      txHash: "c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b44",
      outputIndex: 2n,
      address: "addr1v92m7sgckq0pc72xgldexa0lepe7gdwhxuq8k2kmcjxdh2skxa0m4",
      assets: { lovelace: 100_000_000n },
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };

  emulator.ledger["f401bc107c6e69b94a5a6e2eb395ffb797f976d5dbc7c20da9d3d3c547a9f0512"] = {
    utxo: {
      txHash: "f401bc107c6e69b94a5a6e2eb395ffb797f976d5dbc7c20da9d3d3c547a9f051",
      outputIndex: 2n,
      address: "addr1v92m7sgckq0pc72xgldexa0lepe7gdwhxuq8k2kmcjxdh2skxa0m4",
      assets: { lovelace: 34_686_595n },
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };
  emulator.ledger["35b925b56c729d6099978c721df5c622bac03029ded2800781e636e347de714e0"] = {
    utxo: {
      txHash: "35b925b56c729d6099978c721df5c622bac03029ded2800781e636e347de714e",
      outputIndex: 0n,
      address: "addr1wyr32h227pudmkg42dq9cx6k4scrqpqj0v786rrux3emyccd5em8f",
      assets: {
        lovelace: 3_100_000n,
        [rberry]: 1000n,
        [sberry]: 1000n,
      },
      datumHash: undefined,
      datum: "d8799fd8799f581c5e7df7e74c56adf93537657d744256a0fce543916837df34d9a98e41ffd8799f581c55bf4118b01e1c794647db9375ffc873e435d737007b2adbc48cdbaaff1a0010c8e0d8799fd8799fd8799f581c55bf4118b01e1c794647db9375ffc873e435d737007b2adbc48cdbaaffd87a80ffd87980ffd8799fd8799f5820fafcb4fb6ced8f0a123b00cf466ed9a11849bd97f7942b03c5c97ed60ce6f69fffffd87980ff",
      scriptRef: undefined
    },
    spent: false
  };
  emulator.ledger["f401bc107c6e69b94a5a6e2eb395ffb797f976d5dbc7c20da9d3d3c547a9f0510"] = {
    utxo: {
      txHash: "f401bc107c6e69b94a5a6e2eb395ffb797f976d5dbc7c20da9d3d3c547a9f051",
      outputIndex: 0n,
      address: "addr1xyzpy0vxwfqwh0m8qvchgj06qcreee687ztsvaeks08srkcs9ehhm4hh4nlvdtaw0pt0gwtrwfgrcnvyjj74kuqvsknsqcqes2",
      assets: {
        lovelace: 5_500_000n,
        [rberry]: 1_000_000n,
        [sberry]: 1_000_000n,
        [poolnft]: 1n,
      },
      datumHash: undefined,
      datum: "d8799f581c5e7df7e74c56adf93537657d744256a0fce543916837df34d9a98e419f9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e846524245525259ff9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e846534245525259ffff1903e69f181e181eff9f18321832ffd8799fd8799f581c8582e6a55ccbd7af4cabe35d6da6eaa3d543083e1ce822add9917730ffff00001a0053ec60ff",
      scriptRef: undefined
    },
    spent: false
  };
  emulator.ledger["f7aeb7c5e0a1fd2907d4808185cb01f63d2e2824da22a9836bb1c66a54b7f8a40"] = {
    utxo: {
      txHash: "f7aeb7c5e0a1fd2907d4808185cb01f63d2e2824da22a9836bb1c66a54b7f8a4",
      outputIndex: 0n,
      address: "addr1qyd2dlgxzed75q6grpcplkjygjda3setj0v5w2zfuvw76fg6ntjap8v99w6ms2cpujt44q49ldj2jezfq03akshmzx5sxfd0yj",
      assets: {
        lovelace: 11_667_170n
      },
      datumHash: undefined,
      datum: undefined,
      scriptRef: orderSpendScriptRef
    },
    spent: false
  };
  emulator.ledger["b49203cf5272fe3d4f24d1dae3a4d3f3debb5d8b873de542ee3f6435d39328130"] = {
    utxo: {
      txHash: "b49203cf5272fe3d4f24d1dae3a4d3f3debb5d8b873de542ee3f6435d3932813",
      outputIndex: 0n,
      address: "addr1qyd2dlgxzed75q6grpcplkjygjda3setj0v5w2zfuvw76fg6ntjap8v99w6ms2cpujt44q49ldj2jezfq03akshmzx5sxfd0yj",
      assets: {
        lovelace: 70_408_160n
      },
      datumHash: undefined,
      datum: undefined,
      scriptRef: poolSpendScriptRef
    },
    spent: false
  };
  emulator.ledger["0cab57f56e00ec587de1d16258ea313e39854b4e9bd304df45d872e5b0ddda390"] = {
    utxo: {
      txHash: "0cab57f56e00ec587de1d16258ea313e39854b4e9bd304df45d872e5b0ddda39",
      outputIndex: 0n,
      address: "addr1w9680rk7hkue4e0zkayyh47rxqpg9gzx445mpha3twge75sku2mg0",
      assets: {
        lovelace: 7_266_660n,
        [toUnit("74778edebdb99ae5e2b7484bd7c3300282a046ad69b0dfb15b919f52", fromText("settings"))]: 1n,
      },
      datumHash: undefined,
      datum: "d8799fd87c9f029fd8799f581c8582e6a55ccbd7af4cabe35d6da6eaa3d543083e1ce822add9917730ffd8799f581c7180d7ad9aaf20658d8f88c32a2e5c287425618c32c9bb82d6b6c8f8ffd8799f581cbba4dff30f517f2859f8f295a97d3d85f26a818078f9294256fda2d8ffd8799f581c1f68495896a7ba5132198145359311e991a1463e95ccc6f56703653dffd8799f581cf65e667d512b26aa98a97ac22e958e5201e7ea279d74b2e4ec5883dbffffffd8799fd87a9f581c1854e9028a89496e9772a54882729d16554f8ed9af27ec6046c9a87cffd87a80ffd87c9f029fd8799f581c8582e6a55ccbd7af4cabe35d6da6eaa3d543083e1ce822add9917730ffd8799f581c7180d7ad9aaf20658d8f88c32a2e5c287425618c32c9bb82d6b6c8f8ffd8799f581cbba4dff30f517f2859f8f295a97d3d85f26a818078f9294256fda2d8ffd8799f581c1f68495896a7ba5132198145359311e991a1463e95ccc6f56703653dffd8799f581cf65e667d512b26aa98a97ac22e958e5201e7ea279d74b2e4ec5883dbffffffd8799fd87a9f581cc0d7aa781d14f206f1f6468f0a2d49187d1ebcb8f59c59d75d0c27a7ffd87a80ff9f0b1864ffd8799f9f581c570cd6294587645d26c690a72d40fede1e7a28cb3ddc78ff76655820581c61f1baeda28f3f83413b92a7d28d2f7b545d718f2f28f971b92b3a21581c251f7fb11f84f81653ee5b76a10dd29fa36ec7717aafe689490cb7e4581c6510a3ec0a6f273e31acc82f9f2ffb089413549a04149ea37ef8d33b581c9366b01d6baf040245ee07127fc8af4f04a75b91c6a97f69c7f6463a581cc5825983bb454dd743befc1dd65ee05934666c417503060e1d4fef47581c70fa8ce8dda9372aa9b7dc9f5756390e78939744c79550cc3a264b79581cf7b1175ea4f7980e717e19c76731e4e6ff3a2ac560dc17a6be8ec204581ca14cb1a14c4b5810a21103e389f4abdbdec010b766e2dc329a4e0e96581c40282b949abda48a573fe2757971a1369d2674ac9b6d98c1c2bdbdf7581cbaec408a6fedd39ac0404a2f82c6e75ef06659d8596f9d0af6e01241581cfe9315a8d1f638a4836e9ec396d43e1f6ba88e45a7f5a5e37a77071a581cc6b0d1b88337b91507aa5c6496afc497f399ed8980c2054448eaab6c581c8ca0e08cdbc30fa0dd21833d7370d666493ecc28b136df179f97fb5d581cf7e1830a1f885aed62fc834c1dffcadd68a8548e88ffd0b0040b960b581cee8ed5ef92d0a51c6962aac7012906d280aeb412900a7621f782c7c9581c1ddc54ce9d4d3a35a0ff4707636f8627cc491197ac435ba4fcf4d838581ccba4b71bd8cecc54c526bcd71da84f6f79e568604e574149854dbb86581cf52cdec15ffcc8ace593dc3e0078458ba07a8c47866ba866e4554b6d581c53d6b12089d642d3bfdc61d5f0f3fddfeeb56f55dcd5bd796b5c25a1581cdd8a02814820616b137e0fb4852fd8aab36875d849919ca68aa6cb70581c0b23328355b40d671d1a7ded332c697e1446ae0bb7301af2a7ed9577581c8be85963f17386d34bcd53b857071a01ee3c7ca543e4bd674492f78b581cb3ec1a4fa75b0c9291af873d3d5365b46ab22eff2bc3f12d5064561b581c6c8ecf30ba1a025dd324cb0598c8ff87522b324901299cf3f4f1d0b2581c7a7a02beabb674125d734a24817aea9505b9113540cc72f4ef5c2faf581c2daa1c8bb06659a89886f6b2677b8d8ccd1c2c7e9ef7ea4be88ac404581c6a054c69ea18300b0fcb330e07a8b56015d15527f31a361b1b1291de581cee33744f202c96c2024b25b6cc177ad9537d46437cd48cc291f58009581c37eb116b3ff8a70e4be778b5e8d30d3b40421ffe6622f6a983f67f3f581c55bf4118b01e1c794647db9375ffc873e435d737007b2adbc48cdbaaffff9fd87a9f581c102e6f7dd6f7acfec6afae7856f4396372503c4d8494bd5b700c85a7ffff1a000510e01a000290401a0002904000d87980ff",
      scriptRef: undefined
    },
    spent: false
  };

  const change =
    emulator.ledger["f401bc107c6e69b94a5a6e2eb395ffb797f976d5dbc7c20da9d3d3c547a9f0512"].utxo;
  const order = 
    emulator.ledger["35b925b56c729d6099978c721df5c622bac03029ded2800781e636e347de714e0"].utxo;
  const pool = 
    emulator.ledger["f401bc107c6e69b94a5a6e2eb395ffb797f976d5dbc7c20da9d3d3c547a9f0510"].utxo;

  const references =
    [
      emulator.ledger["b49203cf5272fe3d4f24d1dae3a4d3f3debb5d8b873de542ee3f6435d39328130"].utxo,
      emulator.ledger["f7aeb7c5e0a1fd2907d4808185cb01f63d2e2824da22a9836bb1c66a54b7f8a40"].utxo
    ];

  const settings =
    emulator.ledger["0cab57f56e00ec587de1d16258ea313e39854b4e9bd304df45d872e5b0ddda390"].utxo;

  const newPoolDatum =
    "d8799f581c5e7df7e74c56adf93537657d744256a0fce543916837df34d9a98e419f9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e846524245525259ff9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e846534245525259ffff1903e69f181e181eff9f18321832ffd8799fd8799f581c8582e6a55ccbd7af4cabe35d6da6eaa3d543083e1ce822add9917730ffff00001a005b8d80ff";

  const poolAddr = "addr1xyzpy0vxwfqwh0m8qvchgj06qcreee687ztsvaeks08srkcs9ehhm4hh4nlvdtaw0pt0gwtrwfgrcnvyjj74kuqvsknsqcqes2";

  // Network id 1=mainnet
  const orderDestination = "addr1v92m7sgckq0pc72xgldexa0lepe7gdwhxuq8k2kmcjxdh2skxa0m4";
  // Network id 0=testnet
  //const orderDestination = "addr1qpnh34ra6rm5wc8nsjseekknvxy6dv0neyqu7n5z7ayr6wcp8plfds3j3vct3gwp287u4wk4jtr4632d2gmdm96gp4jqgecn8p";

  // Network id 1=mainnet
  const stakeAddress = "stake17yzcx4hnmk4xsz27ney9rgr9m7nhfzkhfyr2q0msjkvw4pgpvj8dq";
  // Network id 0=testnet
  //const stakeAddress = "stake17qzcx4hnmk4xsz27ney9rgr9m7nhfzkhfyr2q0msjkvw4pgp7zkd8";

  const stakeValidator = {
    type: "PlutusV2",
    script: "5901420100003323232323232322322253330053253330063370e900218039baa300130083754004264a66600e66e1d2000300837540022646600200264a66601266e1d2002300a3754002297adef6c6013756601c60166ea8004c8cc004004dd5980218059baa300e300b375400644a66601a0022980103d87a80001323232533300d3371e0166eb8c03800c4cdd2a4000660226e980052f5c026600a00a0046eacc038008c044008c03c004894ccc030004528099299980519b873371c6eb8c02cc03c00920024806852889980180180098078008b1929998050008a6103d87a800013374a9000198059806000a5eb80dd618059806180618041baa300b3008375400429408c02cc03000452613656375c002ae6955ceaab9e5573eae815d0aba24c011e581c04123d867240ebbf6703317449fa06079ce747f09706773683cf01db0001"
  };
  
  console.log("oh baby");

  lucid.selectWalletFrom({
    address: userAddress,
  });
  
  const currentTime = emulator.now();
  console.log(`currentTime: ${currentTime}`);
  const tx = lucid.newTx();

  const goodSSE = "d87a9fd8799f00181e9f9f00d8799fd8799fd8799fd8799fd8799f582035b925b56c729d6099978c721df5c622bac03029ded2800781e636e347de714eff00ffd8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffffd87a9f9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e8465242455252591903e8ff9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e84653424552525900ffffd87980ffd8799f58402497034910d085fd67aa6130e20ac12164027371da704a1ea22d9314584aefb8c79f3c5bf935b2b2ce0c4682a5d1cdd6b068f5a04818f1d58bfc728e38ed0205ffffff00ffffffff";
  const evilSSE = "d87a9fd8799f00181e9f9f00d8799fd8799fd8799fd8799fd8799f582035b925b56c729d6099978c721df5c622bac03029ded2800781e636e347de714eff00ffd8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffffd87a9f9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e8465242455252591903e8ff9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e84653424552525900ffffd87980ffd87a80ffff00ffffffff";

  tx
    .validFrom(currentTime + 47509375000)
    .validTo(currentTime + 47511375000)
    .readFrom([settings, ...references])
    .attachSpendingValidator(stakeValidator)
    .addSigner("addr1v92m7sgckq0pc72xgldexa0lepe7gdwhxuq8k2kmcjxdh2skxa0m4")
    .withdraw(stakeAddress, 0n, "00")
    .collectFrom([change])
    .collectFrom([pool], goodSSE)
    .collectFrom([order], "d87980")
    .payToContract(poolAddr, { inline: newPoolDatum }, {
      "lovelace": 6_000_000n,
      [rberry]: 1_001_000n,
      [sberry]: 999_004n,
      [poolnft]: 1n
    })
    .payToAddress(orderDestination, {
      "lovelace": 2_600_000n,
      [sberry]: 1_996n, 
    })
    //.payToAddress(userAddress, {
    //  "lovelace": 99_759_203n
    //});

  const str = await tx.toString();
  console.log("building tx: " + str);
  const completed = await tx.complete({
    coinSelection: false,
  });
  const completedStr = await completed.toString();
  console.log("completed: " + completedStr);
  //const signedTx = await completed.sign().complete();
  //const exUnits = completed.exUnits;
  //const signedStr = await signedTx.toString();
  //console.log("signed tx: " + signedStr);
  //const scoopedHash = signedTx.submit();
  //await emulator.awaitTx(scoopedHash);
  //return exUnits;
}



async function protov3ScoopDebug() {
  const accounts: any[] = [];
  let emulator = new Emulator(accounts, {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxTxSize: 999999999999,
    maxTxExMem: 999999999999999n,
  });
  let lucid = await Lucid.new(emulator);

  const userAddress = "addr1v92m7sgckq0pc72xgldexa0lepe7gdwhxuq8k2kmcjxdh2skxa0m4";

  let orderSpendScriptRefBytes = await Deno.readTextFile(flags.orderSpend);
  let orderSpendScriptRef = {
    type: "PlutusV2",
    script: orderSpendScriptRefBytes
  };
  let poolSpendScriptRefBytes = await Deno.readTextFile(flags.poolSpend);
  let poolSpendScriptRef = {
    type: "PlutusV2",
    script: poolSpendScriptRefBytes
  };

  console.log("oh baby 1");

  let rberry = toUnit(
    "99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e8",
    fromText("RBERRY")
  );

  let sberry = toUnit(
    "99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e8",
    fromText("SBERRY")
  );

  const poolnft = toUnit(
    "04123d867240ebbf6703317449fa06079ce747f09706773683cf01db",
    "000de140a37b1fb65d69bfd68e26ef1bab3c83e0479ab73c9c52d702c7d3f923"
  );

  // This is not in the original tx but im adding it here to try to get lucid to
  // stop complaining about collateral
  emulator.ledger["c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b441"] = {
    utxo: {
      txHash: "c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b44",
      outputIndex: 1n,
      address: "addr1v92m7sgckq0pc72xgldexa0lepe7gdwhxuq8k2kmcjxdh2skxa0m4",
      assets: { lovelace: 100_000_000n },
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };
  emulator.ledger["c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b442"] = {
    utxo: {
      txHash: "c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b44",
      outputIndex: 2n,
      address: "addr1v92m7sgckq0pc72xgldexa0lepe7gdwhxuq8k2kmcjxdh2skxa0m4",
      assets: { lovelace: 100_000_000n },
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };

  emulator.ledger["c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b440"] = {
    utxo: {
      txHash: "c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b44",
      outputIndex: 0n,
      address: "addr1v92m7sgckq0pc72xgldexa0lepe7gdwhxuq8k2kmcjxdh2skxa0m4",
      assets: { lovelace: 100_000_000n },
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };
  emulator.ledger["6175708f51362c9e8b860b0b2851b43eb5ed670aed1b5e3b0d96d9b7ab411b9b0"] = {
    utxo: {
      txHash: "6175708f51362c9e8b860b0b2851b43eb5ed670aed1b5e3b0d96d9b7ab411b9b",
      outputIndex: 0n,
      address: "addr1zyr32h227pudmkg42dq9cx6k4scrqpqj0v786rrux3emyccp8plfds3j3vct3gwp287u4wk4jtr4632d2gmdm96gp4jqt39l6k",
      assets: {
        lovelace: 3_000_000n,
        [rberry]: 4_000_000n,
      },
      datumHash: undefined,
      datum: "d8799fd8799f581ca37b1fb65d69bfd68e26ef1bab3c83e0479ab73c9c52d702c7d3f923ffd8799f581c01387e96c2328b30b8a1c151fdcabad592c75d454d5236dd97480d64ff1a000f4240d8799fd8799fd8799f581c6778d47dd0f74760f384a19cdad36189a6b1f3c901cf4e82f7483d3bffd8799fd8799fd8799f581c01387e96c2328b30b8a1c151fdcabad592c75d454d5236dd97480d64ffffffffd87980ffd87a9f9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e8465242455252591a003d0900ff9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e8465342455252591a003b343dffff43d87980ff",
      scriptRef: undefined
    },
    spent: false
  };
  emulator.ledger["b6c88753597872cf7ad5e1d2ff2ca02a6a31b3cc63980829e01f40e1abeb047a0"] = {
    utxo: {
      txHash: "b6c88753597872cf7ad5e1d2ff2ca02a6a31b3cc63980829e01f40e1abeb047a",
      outputIndex: 0n,
      address: "addr1xyzpy0vxwfqwh0m8qvchgj06qcreee687ztsvaeks08srk6cg0uvjyu9nr727qnsc2ljadfkldrtqdp3ehnyyavpz9uq58yqg9",
      assets: {
        lovelace: 3_000_000n,
        [rberry]: 5_000_000_000_000n,
        [sberry]: 5_000_000_000_000n,
        [poolnft]: 1n,
      },
      datumHash: undefined,
      datum: "d8799f581ca37b1fb65d69bfd68e26ef1bab3c83e0479ab73c9c52d702c7d3f9239f9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e846524245525259ff9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e846534245525259ffff1b0000048c273950009f181e181eff9f181e181effd87a801b0000018ef32f22161b0000018ef32f22161a002dc6c0ff",
      scriptRef: undefined
    },
    spent: false
  };
  emulator.ledger["f7aeb7c5e0a1fd2907d4808185cb01f63d2e2824da22a9836bb1c66a54b7f8a40"] = {
    utxo: {
      txHash: "f7aeb7c5e0a1fd2907d4808185cb01f63d2e2824da22a9836bb1c66a54b7f8a4",
      outputIndex: 0n,
      address: "addr1qyd2dlgxzed75q6grpcplkjygjda3setj0v5w2zfuvw76fg6ntjap8v99w6ms2cpujt44q49ldj2jezfq03akshmzx5sxfd0yj",
      assets: {
        lovelace: 11_667_170n
      },
      datumHash: undefined,
      datum: undefined,
      scriptRef: orderSpendScriptRef
    },
    spent: false
  };
  emulator.ledger["b49203cf5272fe3d4f24d1dae3a4d3f3debb5d8b873de542ee3f6435d39328130"] = {
    utxo: {
      txHash: "b49203cf5272fe3d4f24d1dae3a4d3f3debb5d8b873de542ee3f6435d3932813",
      outputIndex: 0n,
      address: "addr1qyd2dlgxzed75q6grpcplkjygjda3setj0v5w2zfuvw76fg6ntjap8v99w6ms2cpujt44q49ldj2jezfq03akshmzx5sxfd0yj",
      assets: {
        lovelace: 70_408_160n
      },
      datumHash: undefined,
      datum: undefined,
      scriptRef: poolSpendScriptRef
    },
    spent: false
  };
  emulator.ledger["25d830ea4bec1a6ac24cf35ee7b37a6ae3f4a903bf021660cd27b4bdf8b230b60"] = {
    utxo: {
      txHash: "25d830ea4bec1a6ac24cf35ee7b37a6ae3f4a903bf021660cd27b4bdf8b230b6",
      outputIndex: 0n,
      address: "addr1w9680rk7hkue4e0zkayyh47rxqpg9gzx445mpha3twge75sku2mg0",
      assets: {
        lovelace: 2_271_370n,
        [toUnit("74778edebdb99ae5e2b7484bd7c3300282a046ad69b0dfb15b919f52", fromText("settings"))]: 1n,
      },
      datumHash: undefined,
      datum: "d8799fd8799f581c6778d47dd0f74760f384a19cdad36189a6b1f3c901cf4e82f7483d3bffd8799fd8799f581c6778d47dd0f74760f384a19cdad36189a6b1f3c901cf4e82f7483d3bffd87a80ffd8799f581c6778d47dd0f74760f384a19cdad36189a6b1f3c901cf4e82f7483d3bffd8799fd8799f581c6778d47dd0f74760f384a19cdad36189a6b1f3c901cf4e82f7483d3bffd87a80ff9f010affd8799f9f581c41ccc9327949dc71be86efbb58eab0e2a2d34a4bde61d21727ecf356581c55bf4118b01e1c794647db9375ffc873e435d737007b2adbc48cdbaaffff9fd87a9f581c5843f8c9138598fcaf0270c2bf2eb536fb46b03431cde64275811178ffff1a000510e01a000290401a000290400000ff",
      scriptRef: undefined
    },
    spent: false
  };

  const change =
    emulator.ledger["c3bb601268244b710cc507170eb4f67ce625ebaa23d1ee80382e2e9fe6d32b440"].utxo;
  const order = 
    emulator.ledger["6175708f51362c9e8b860b0b2851b43eb5ed670aed1b5e3b0d96d9b7ab411b9b0"].utxo;
  const pool = 
    emulator.ledger["b6c88753597872cf7ad5e1d2ff2ca02a6a31b3cc63980829e01f40e1abeb047a0"].utxo;

  const references =
    [
      emulator.ledger["b49203cf5272fe3d4f24d1dae3a4d3f3debb5d8b873de542ee3f6435d39328130"].utxo,
      emulator.ledger["f7aeb7c5e0a1fd2907d4808185cb01f63d2e2824da22a9836bb1c66a54b7f8a40"].utxo
    ];

  const settings =
    emulator.ledger["25d830ea4bec1a6ac24cf35ee7b37a6ae3f4a903bf021660cd27b4bdf8b230b60"].utxo;

  const newPoolDatum =
    "d8799f581ca37b1fb65d69bfd68e26ef1bab3c83e0479ab73c9c52d702c7d3f9239f9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e846524245525259ff9f581c99f64e2f566c87003cb11cc9edb56b8109947925d3930103d0d198e846534245525259ffff1b0000048c273950009f181e181eff9f181e181effd87a801b0000018ef32f22161b0000018ef32f22161a003567e0ff";

  const poolAddr = "addr1xyzpy0vxwfqwh0m8qvchgj06qcreee687ztsvaeks08srk6cg0uvjyu9nr727qnsc2ljadfkldrtqdp3ehnyyavpz9uq58yqg9";

  // Network id 1=mainnet
  const orderDestination = "addr1q9nh34ra6rm5wc8nsjseekknvxy6dv0neyqu7n5z7ayr6wcp8plfds3j3vct3gwp287u4wk4jtr4632d2gmdm96gp4jqe0354u";
  // Network id 0=testnet
  //const orderDestination = "addr1qpnh34ra6rm5wc8nsjseekknvxy6dv0neyqu7n5z7ayr6wcp8plfds3j3vct3gwp287u4wk4jtr4632d2gmdm96gp4jqgecn8p";

  // Network id 1=mainnet
  const stakeAddress = "stake17yzcx4hnmk4xsz27ney9rgr9m7nhfzkhfyr2q0msjkvw4pgpvj8dq";
  // Network id 0=testnet
  //const stakeAddress = "stake17qzcx4hnmk4xsz27ney9rgr9m7nhfzkhfyr2q0msjkvw4pgp7zkd8";

  const stakeValidator = {
    type: "PlutusV2",
    script: "5901420100003323232323232322322253330053253330063370e900218039baa300130083754004264a66600e66e1d2000300837540022646600200264a66601266e1d2002300a3754002297adef6c6013756601c60166ea8004c8cc004004dd5980218059baa300e300b375400644a66601a0022980103d87a80001323232533300d3371e0166eb8c03800c4cdd2a4000660226e980052f5c026600a00a0046eacc038008c044008c03c004894ccc030004528099299980519b873371c6eb8c02cc03c00920024806852889980180180098078008b1929998050008a6103d87a800013374a9000198059806000a5eb80dd618059806180618041baa300b3008375400429408c02cc03000452613656375c002ae6955ceaab9e5573eae815d0aba24c011e581c04123d867240ebbf6703317449fa06079ce747f09706773683cf01db0001"
  };
  
  console.log("oh baby");

  lucid.selectWalletFrom({
    address: userAddress,
  });
  
  const currentTime = emulator.now();
  console.log(`currentTime: ${currentTime}`);
  const tx = lucid.newTx();
  tx
    .validFrom(currentTime + 47509375000)
    .validTo(currentTime + 47511375000)
    .readFrom([settings, ...references])
    .attachSpendingValidator(stakeValidator)
    .addSigner("addr1v92m7sgckq0pc72xgldexa0lepe7gdwhxuq8k2kmcjxdh2skxa0m4")
    .withdraw(stakeAddress, 0n, "00")
    .collectFrom([change])
    .collectFrom([pool], "d87a9fd8799f00019f9f00d87a8000ffffffff")
    .collectFrom([order], "d87980")
    .payToContract(poolAddr, { inline: newPoolDatum }, {
      "lovelace": 3_500_000n,
      [rberry]: 5_000_004_000_000n,
      [sberry]: 4_999_996_012_004n,
      [poolnft]: 1n
    })
    .payToAddress(orderDestination, {
      "lovelace": 2_500_000n,
      [sberry]: 3_987_996n
    })
    //.payToAddress(userAddress, {
    //  "lovelace": 99_759_203n
    //});

  const str = await tx.toString();
  console.log("building tx: " + str);
  const completed = await tx.complete({
    coinSelection: false,
  });
  const completedStr = await completed.toString();
  console.log("completed: " + completedStr);
  //const signedTx = await completed.sign().complete();
  //const exUnits = completed.exUnits;
  //const signedStr = await signedTx.toString();
  //console.log("signed tx: " + signedStr);
  //const scoopedHash = signedTx.submit();
  //await emulator.awaitTx(scoopedHash);
  //return exUnits;

    
}

async function benchmark(flags: any) {
  if (flags.scriptsFile == undefined) {
    throw "no scripts file";
  }

  let s = await Deno.readTextFile(flags.scriptsFile);
  let scriptsJson = JSON.parse(s);
  const dummy = await Lucid.new(undefined, "Custom");
  const scripts = getScriptsAiken(dummy, scriptsJson);
  const [userAddress, userPkh, userPrivateKey] = fakeAddress(dummy);

  const accounts: any[] = [
    {
      address: userAddress,
      assets: {
        "lovelace": 1_000_000_000_000n,
      }
    }
  ];
  let emulator = new Emulator(accounts, {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxTxSize: 999999999999,
    maxTxExMem: 999999999999999n,
  });
  let lucid = await Lucid.new(emulator);

  emulator.ledger[bootUtxoHash + bootUtxoIx] = {
    utxo: {
      txHash: bootUtxoHash,
      outputIndex: bootUtxoIx,
      address: userAddress,
      assets: { lovelace: 1_000_000_000n },
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };

  await testSettingsBoot(lucid, emulator, scripts);
  const mintedUtxo = await testMintRberry(lucid, emulator, scripts);
  const poolMintRef = await testPostReferenceScript(lucid, emulator, scripts, "poolMint");
  const poolValidatorRef = await testPostReferenceScript(lucid, emulator, scripts, "poolValidator");
  const orderValidatorRef = await testPostReferenceScript(lucid, emulator, scripts, "orderValidator");
  const [rberryMintingPolicy, rberryPolicyId]: [Script, string] = await getRberryPolicyId();
  const rberry = rberryPolicyId + "." + fromText("RBERRY");
  //await testMakePoolFunds(lucid, emulator, scripts, "lovelace", 1_020_000_000n, rberry, 1_000_000_000n);

  emulator.ledger["00000000000000000000000000000000000000000000000000000000000000000"] = {
    utxo: {
      txHash: "0000000000000000000000000000000000000000000000000000000000000000",
      outputIndex: 0,
      assets: {
        lovelace: 100_000_000_000n,
        [toUnit(rberryPolicyId, fromText("RBERRY"))]: 100_000_000_000n,
      },
      address: userAddress,
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };

  const fakeChange = emulator.ledger["00000000000000000000000000000000000000000000000000000000000000000"].utxo;

  const { poolMintedHash, poolId } =
    await testMintPool(lucid, emulator, scripts, "lovelace", rberry, poolMintRef, fakeChange);

  console.log("pool id: ", poolId);

  emulator.ledger["00000000000000000000000000000000000000000000000000000000000000001"] = {
    utxo: {
      txHash: "0000000000000000000000000000000000000000000000000000000000000000",
      outputIndex: 1,
      assets: {
        lovelace: 100_000_000_000n,
        [toUnit(rberryPolicyId, fromText("RBERRY"))]: 100_000_000_000n,
      },
      address: userAddress,
      datumHash: undefined,
      datum: undefined,
      scriptRef: undefined
    },
    spent: false
  };

  const listOrdersChange = emulator.ledger["00000000000000000000000000000000000000000000000000000000000000001"].utxo;

  const { listedHash, utxos: orders } =
    await testListOrder(lucid, emulator, scripts, "lovelace", rberry, listOrdersChange, poolId, 40n);

  const scoopPoolChange = await findChange(emulator, userAddress);

  const savedLedger = structuredClone(emulator.ledger);
  console.log("savedLedger");
  console.log(savedLedger);
  console.log("ok");

  const runs = new Map();

  for (let i = 37; i <= 45; i++) {
    emulator.ledger = structuredClone(savedLedger);
    try {
      const exUnits = await testScoopPool(lucid, emulator, scripts, poolId, scoopPoolChange, [orderValidatorRef, poolValidatorRef], orders.slice(0, i));
      runs.set(i, exUnits);
    } catch (e) {
      console.log("Failed to scoop: ", e)
    }
  }

  console.log("results");
  console.log(runs);
}

const flags = parse(Deno.args, {
  string: ["scriptsFile", "privateKey", "coinA", "coinB", "blockfrostUrl", "blockfrostProjectId", "scooperPkh"],
});

if (flags.runBenchmark) {
  await benchmark(flags);
} else if (flags.bootSettings) {
  await realSettingsBoot();
} else if (flags.postReferenceScript) {
  await doPostReferenceScript(flags.scriptName);
} else if (flags.registerStakeAddress) {
  await registerStakeAddress(flags.scriptName);
} else if (flags.buildSSE) {
  await buildSSE();
} else if (flags.noopSettings) {
  await noopSettings();
} else if (flags.addScooper) {
  await addScooper();
} else if (flags.updateSettingsDatum) {
  await updateSettingsDatum();
} else if (flags.updateSettingsDatumTreasury) {
  await updateSettingsDatumTreasury();
} else if (flags.debug) {
  await protov3ScoopDebug();
} else if (flags.debugStrategy) {
  await protov3ScoopDebugStrategy();
} else if (flags.withdrawPoolRewards) {
  await withdrawPoolRewards();
} else if (flags.mintPool) {
  await mainnetMintPool();
} else if (flags.updatePoolFees) {
  await updatePoolFees();
} else if (flags.evaporatePool) {
  await evaporatePool();
} else if (flags.delegate) {
  await delegatePool();
} else if (flags.debugOracle) {
  await previewRecordOrderDebug();
} else if (flags.dummy) {
  await dummy();
}

// Corresponds to strategy_verify_signature in contracts tests
Deno.test("test buildSSE", async () => {
  const strategyExecution: StrategyExecution = {
    txRef: {
      transactionId: {
        hash: "5d4fefff26960b203687dd0e33bdcb9b4c79a10ed87326d31238d4a9659d7ffa",
      },
      outputIndex: 0n,
    },
    validityRange: {
      lowerBound: {
        boundType: { NegativeInfinity: [] },
        isInclusive: true,
      },
      upperBound: {
        boundType: { PositiveInfinity: [] },
        isInclusive: true,
      },
    },
    details: {
      Swap: {
        offer: ["", "", 1_000_000n],
        minReceived: [
          "824faecc3a7312f5cad0397dd9ffb96e15dd4cf6a3cf16e23d7352b3",
          "4d6f6f6d6f6f204d696c6b",
          0n
        ],
      },
    },
    extensions: "NoExtension",
  };
  const strategyExecutionBytes = Data.to(strategyExecution, types.StrategyExecution);
  console.log(strategyExecutionBytes);
  const signature = await signAsync(
    strategyExecutionBytes,
    "e4f721e42cba4b76203614ff0d945466dd58458c08e3fd0de36f40f4c709ade1",
  );
  assertEquals(
    toHex(signature),
    "d22ea1fb150eb58cedae1fe6d7f83fcfc2f102fb19dc0b3e99cd9efdc69d865a5a6cb98691aca3ec5d4523c7bb0eb684d8ba2d918b9de5411cdd9f56bbac1307",
  );
});
