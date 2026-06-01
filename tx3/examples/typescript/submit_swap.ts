import { Client, type Profile, type SubmitSwapParams } from "../../gen/typescript/sundae-v3/protocol";
import { Party } from "tx3-sdk";
import type { ClientOptions } from "tx3-sdk/trp";

const profile: Profile = "preview";

const options: ClientOptions = {
  endpoint: "https://cardano-preview.trp-m1.demeter.run",
  headers: {
    "dmtr-api-key": process.env.DEMETER_TRP_API_KEY ?? "",
  },
};

const userAddress = process.env.SUNDAE_USER_ADDRESS ?? "<wallet address>";

const params: SubmitSwapParams = {
  poolIdent: "35a34996f515c5a28c8df9eada81f03f4f2756d92e7f73cde1f4e593",
  ownerKeyHash: "<stake key hash>",
  destinationPaymentKeyHash: "<payment key hash>",
  destinationStakeKeyHash: "<stake key hash>",
  orderAda: 3_000_000,
  maxProtocolFee: 600_000,
  offerPolicy: "d8906ca5c7ba124a0407a32dab37b2c82b13b3dcd9111e42940dcea4",
  offerName: "0014df105553444d",
  offerAmount: 1_000_000,
  minReceivedPolicy: "",
  minReceivedName: "",
  minReceivedAmount: 1,
};

async function main() {
  if (!options.headers?.["dmtr-api-key"]) {
    throw new Error("missing DEMETER_TRP_API_KEY");
  }

  if (!process.env.SUNDAE_USER_ADDRESS) {
    throw new Error("missing SUNDAE_USER_ADDRESS");
  }

  const client = new Client(options, profile).withUser(Party.address(userAddress));

  // The preview/mainnet profiles already carry the `orderscript` party value.
  // Call `.withOrderscript(...)` only if you want to override it.

  const builder = client.submitSwap(params);
  const resolved = await builder.resolve();

  console.log("resolve ok");
  console.log("tx hash:", resolved.hash);
  console.log("tx hex:", resolved.txHex);
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
