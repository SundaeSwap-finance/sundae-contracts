import { Data } from "https://deno.land/x/lucid@0.20.5/mod.ts";

export const examplePkh: string = "6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513";

// Incomplete, but sufficient for our use case
export const MultiSigScriptSchema = Data.Enum(
  { Signature: { keyHash: Data.Bytes() } },
  { AllOf: { scripts: Data.Array(Data.Bytes()) } },
);
export type MultiSigScript = typeof MultiSigScriptSchema;
export const MultiSigScript = MultiSigScriptSchema as unknown as MultiSigScript;

export const SingletonValueSchema = Data.Tuple([
  Data.Bytes(),
  Data.Bytes(),
  Data.Integer(),
]);
export type SingletonValue = typeof SingletonValueSchema;
export const SingletonValue = SingletonValueSchema as unknown as SingletonValue;

// TODO: Implement
export const StrategySchema = Data.Object({
  dummy: Data.Integer(),
});
export type Strategy = typeof StrategySchema;
export const Strategy = StrategySchema as unknown as Strategy;

export const SwapSchema = Data.Object({
  offer: SingletonValue,
  minReceived: SingletonValue,
});
export type Swap = typeof SwapSchema;
export const Swap = SwapSchema as unknown as Swap;

export const DepositSchema = Data.Object({
  assets: Data.Tuple([SingletonValue, SingletonValue]),
});
export type Deposit = typeof DepositSchema;
export const Deposit = DepositSchema as unknown as Deposit;

export const WithdrawalSchema = Data.Object({
  amount: SingletonValue,
});
export type Withdrawal = typeof WithdrawalSchema;
export const Withdrawal = WithdrawalSchema as unknown as Withdrawal;

export const DonationSchema = Data.Object({
  assets: Data.Tuple([SingletonValue, SingletonValue]),
});
export type Donation = typeof DonationSchema;
export const Donation = DonationSchema as unknown as Donation;

export const OrderSchema = Data.Enum(
  { Strategy: Strategy },
  { Swap: Swap },
  { Deposit: Deposit },
  { Withdrawal: Withdrawal },
  { Donation: Donation },
);
export type Order = typeof OrderSchema;
export const Order = OrderSchema as unknown as Order;

export const CredentialSchema = Data.Enum(
  { VerificationKeyCredential: [Data.Bytes()] },
  { ScriptCredential: [Data.Bytes()] },
);
export type Credential = typeof CredentialSchema;
export const Credential = CredentialSchema as unknown as Credential;

export const AddressSchema = Data.Object({
  paymentCredential: Credential,
  stakeCredential: Data.Nullable(Credential),
});
export type Address = typeof AddressSchema;
export const Address = AddressSchema as unknown as Address;

export const SettingsDatumSchema = Data.Object({
  settingsAdmin: MultiSigScript,
  metadataAdmin: Address,
  treasuryAdmin: MultiSigScript,
  treasuryAddress: Address,
  treasuryAllowance: Data.Tuple([Data.Integer(), Data.Integer()]),
  authorizedScoopers: Data.Nullable(Data.Array(Data.Bytes())),
  authorizedStakingKeys: Data.Array(Credential),
  baseFee: Data.Integer(),
  simpleFee: Data.Integer(),
  strategyFee: Data.Integer(),
  poolCreationFee: Data.Integer(),
  extensions: Data.Any(),
});
export type SettingsDatum = typeof SettingsDatumSchema;
export const SettingsDatum = SettingsDatumSchema as unknown as SettingsDatum;

export const exampleSettingsDatum: SettingsDatum = {
  settingsAdmin: {
    Signature: { keyHash: examplePkh},
  },
  metadataAdmin: {
    paymentCredential: {
      VerificationKeyCredential: [examplePkh],
    },
    stakeCredential: null,
  },
  treasuryAdmin: {
    Signature: { keyHash: examplePkh},
  },
  treasuryAddress: {
    paymentCredential: {
      VerificationKeyCredential: [examplePkh],
    },
    stakeCredential: null,
  },
  treasuryAllowance: [1n, 10n],
  authorizedScoopers: [examplePkh],
  authorizedStakingKeys: [],
  baseFee: 1000000n,
  simpleFee: 100000n,
  strategyFee: 200000n,
  poolCreationFee: 1000000n,
  extensions: Data.void(),
};

export const DatumSchema = Data.Enum(
  "NoDatum",
  "Foo",
  //Data.Object({ DatumHash: Data.Bytes() }),
  //Data.Object({ InlineDatum: Data.Any() }),
);
export type Datum = typeof DatumSchema;
export const Datum = DatumSchema as unknown as Datum;

export const DestinationSchema = Data.Enum(
  {
    Fixed: Data.Object({
      address: Address,
      datum: Datum,
    }),
  },
  {
    Self: Data.Tuple([]),
  },
);
export type Destination = typeof DestinationSchema;
export const Destination = DestinationSchema as unknown as Destination;

export const ExtensionSchema = Data.Enum(
  "NoExtension",
  "Foo",
);
export type Extension = typeof ExtensionSchema;
export const Extension = ExtensionSchema as unknown as Extension;

export const IdentSchema = Data.Bytes();
export type Ident = typeof IdentSchema;
export const Ident = IdentSchema as unknown as Ident;

export const OrderDatumSchema = Data.Object({
  poolIdent: Data.Nullable(Ident),
  owner: MultiSigScript,
  scooperFee: Data.Integer(),
  destination: Destination,
  order: Order,
  extension: Extension,
});
export type OrderDatum = typeof OrderDatumSchema;
export const OrderDatum = OrderDatumSchema as unknown as OrderDatum;

export const AssetClassSchema = Data.Tuple([Data.Bytes(), Data.Bytes()]);
export type AssetClass = typeof AssetClassSchema;
export const AssetClass = AssetClassSchema as unknown as AssetClass;

export const PoolDatumSchema = Data.Object({
  identifier: Ident,
  assets: Data.Tuple([AssetClass, AssetClass]),
  circulatingLp: Data.Integer(),
  bidFeesPer10Thousand: Data.Tuple([Data.Integer(), Data.Integer()]),
  askFeesPer10Thousand: Data.Tuple([Data.Integer(), Data.Integer()]),
  feeManager: Data.Nullable(MultiSigScript),
  marketOpen: Data.Integer(),
  protocolFees: Data.Integer(),
});
export type PoolDatum = typeof PoolDatumSchema;
export const PoolDatum = PoolDatumSchema as unknown as PoolDatum;

export const TransactionIdSchema = Data.Object({
  hash: Data.Bytes(),
});
export type TransactionId = typeof TransactionIdSchema;
export const TransactionId = TransactionIdSchema as unknown as TransactionId;

export const OutputReferenceSchema = Data.Object({
  transactionId: TransactionId,
  outputIndex: Data.Integer(),
});
export type OutputReference = typeof OutputReferenceSchema;
export const OutputReference = OutputReferenceSchema as unknown as OutputReference;

export const IntervalBoundTypeSchema = Data.Enum(
  { NegativeInfinity: "NegativeInfinity" },
  { Finite: Data.Object ({ value: Data.Integer() }) },
  { PositiveInfinity: "PositiveInfinity" }
);
export type IntervalBoundType = typeof IntervalBoundTypeSchema;
export const IntervalBoundType = IntervalBoundTypeSchema as unknown as IntervalBoundType;

export const ValidityRangeSchema = Data.Object({
  lowerBound: Data.Object({
    boundType: IntervalBoundType,
    isInclusive: Data.Boolean(),
  }),
  upperBound: Data.Object({
    boundType: IntervalBoundType,
    isInclusive: Data.Boolean(),
  }),
});
export type ValidityRange = typeof ValidityRangeSchema;
export const ValidityRange = ValidityRangeSchema as unknown as ValidityRange;

export const StrategyExecutionSchema = Data.Object({
  txRef: OutputReference,
  validityRange: ValidityRange,
  details: Order,
});
export type StrategyExecution = typeof StrategyExecutionSchema;
export const StrategyExecution = StrategyExecutionSchema as unknown as StrategyExecution;

export const SignedStrategyExecutionSchema = Data.Object({
  strategy: StrategyExecution,
  signature: Data.Bytes(),
});
export type SignedStrategyExecution = typeof SignedStrategyExecutionSchema;
export const SignedStrategyExecution = SignedStrategyExecutionSchema as unknown as SignedStrategyExecution;

export const InputOrderItemSchema = Data.Tuple([
  Data.Integer(),
  Data.Nullable(SignedStrategyExecution),
  Data.Integer(),
]);
export type InputOrderItem = typeof InputOrderItemSchema;
export const InputOrderItem = InputOrderItemSchema as unknown as InputOrderItem;

export const PoolSpendRedeemerSchema = Data.Enum(
  {
    PoolScoop: Data.Object({
      signatoryIndex: Data.Integer(),
      scooperIndex: Data.Integer(),
      inputOrder: Data.Array(InputOrderItem),
    }),
  },
  {
    Manage: Data.Tuple([]),
  },
);
export type PoolSpendRedeemer = typeof PoolSpendRedeemerSchema;
export const PoolSpendRedeemer = PoolSpendRedeemerSchema as unknown as PoolSpendRedeemer;

export const PoolRedeemerSchema = Data.Enum(
  { Spend: PoolSpendRedeemer },
);
export type PoolRedeemer = typeof PoolRedeemerSchema;
export const PoolRedeemer = PoolRedeemerSchema as unknown as PoolRedeemer;

export const examplePoolRedeemer = {
  Spend: {
      signatoryIndex: 0n,
      scooperIndex: 0n,
      inputOrder: [
        [ 1n, null, 0n ],
      ],
  },
};

export const OrderRedeemerSchema = Data.Enum(
  "Scoop",
  "Cancel",
);
export type OrderRedeemer = typeof OrderRedeemerSchema;
export const OrderRedeemer = OrderRedeemerSchema as unknown as OrderRedeemer;

export const PoolMintRedeemerSchema = Data.Enum(
  { MintLP: { identifier: Data.Bytes() } },
  {
    CreatePool: {
      assets: Data.Tuple([
        AssetClass,
        AssetClass
      ]),
      poolOutput: Data.Integer(),
      metadataOutput: Data.Integer(),
    },
  },
  {
    BurnPool: {
      identifier: Data.Bytes(),
    }
  }
);
export type PoolMintRedeemer = typeof PoolMintRedeemerSchema;
export const PoolMintRedeemer = PoolMintRedeemerSchema as unknown as PoolMintRedeemer;

export const exampleSingletonValue: SingletonValue = ["99", "aa", 100n];

export const exampleSwap: Swap = {
  offer: ["", "", 100n],
  minReceived: ["33", "44", 100n],
};

export const exampleCredential: Credential = {
  VerificationKeyCredential: ["00"],
};

export const exampleOrderDatum: OrderDatum = {
  poolIdent: null,
  owner: {
    Signature: { keyHash: examplePkh},
  },
  scooperFee: 2_500_000n,
  destination: {
    Fixed: {address: {
      paymentCredential: {
        VerificationKeyCredential: [examplePkh],
      },
      stakeCredential: null,
    },
    datum: "NoDatum",
  }},
  order: {
    Swap: {
      offer: ["", "", 10_000_000n],
      minReceived: ["aa", "aa", 0n],
    }
  },
  extension: "NoExtension",
};

export const examplePoolDatum: PoolDatum = {
  identifier: "",
  assets: [
    ["",""],
    ["9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77","53554e444145"],
  ],
  circulatingLp: 1_000_000_000n,
  bidFeesPer10Thousand: [3n, 3n],
  askFeesPer10Thousand: [3n, 3n],
  marketOpen: 100n,
  feeManager: null,
  protocolFees: 1_000_000n,
};
