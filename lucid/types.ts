import { Data } from "../../sundae-lucid/mod.ts";

export const examplePkh: string = "6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513";

// Incomplete, but sufficient for our use case
export const MultiSigScriptSchema = Data.Object({
  signature: Data.Bytes(),
});

export const SingletonValueSchema = Data.Tuple([
  Data.Bytes(),
  Data.Bytes(),
  Data.Integer(),
]);
export type SingletonValue = Data.Static<typeof SingletonValueSchema>;
export const SingletonValue = SingletonValueSchema as unknown as SingletonValue;

// TODO: Implement
export const StrategySchema = Data.Object({
  DUMMY: Data.Integer(),
});

export const SwapSchema = Data.Object({
  offer: SingletonValueSchema,
  minReceived: SingletonValueSchema,
});
export type Swap = Data.Static<typeof SwapSchema>;
export const Swap = SwapSchema as unknown as Swap;

export const DepositSchema = Data.Object({
  assets: Data.Tuple([SingletonValueSchema, SingletonValueSchema]),
});

export const WithdrawalSchema = Data.Object({
  amount: SingletonValueSchema,
});

export const DonationSchema = Data.Object({
  assets: Data.Tuple([SingletonValueSchema, SingletonValueSchema]),
});

export const OrderSchema = Data.Enum([
  Data.Object({ Strategy: StrategySchema }),
  Data.Object({ Swap: SwapSchema }),
  Data.Object({ Deposit: DepositSchema }),
  Data.Object({ Withdrawal: WithdrawalSchema }),
  Data.Object({ Donation: DonationSchema }),
]);
export const CredentialSchema = Data.Enum([
  Data.Object({ VKeyCredential: Data.Object({ bytes: Data.Bytes(), }), }),
  Data.Object({ SCredential: Data.Object({ bytes: Data.Bytes(), }), }),
]);
export type Credential = Data.Static<typeof CredentialSchema>;
export const Credential = CredentialSchema as unknown as Credential;

export const AddressSchema = Data.Object({
  paymentCredential: CredentialSchema,
  stakeCredential: Data.Nullable(CredentialSchema),
});

export const SettingsDatumSchema = Data.Object({
  settingsAdmin: MultiSigScriptSchema,
  metadataAdmin: AddressSchema,
  treasuryAdmin: MultiSigScriptSchema,
  treasuryAddress: AddressSchema,
  treasuryAllowance: Data.Array(Data.Integer()),
  authorizedScoopers: Data.Nullable(Data.Array(Data.Bytes())),
  authorizedStakingKeys: Data.Array(CredentialSchema),
  baseFee: Data.Integer(),
  simpleFee: Data.Integer(),
  strategyFee: Data.Integer(),
  poolCreationFee: Data.Integer(),
  extensions: Data.Integer(),
});
export type SettingsDatum = Data.Static<typeof SettingsDatumSchema>;
export const SettingsDatum = SettingsDatumSchema as unknown as SettingsDatum;

export const exampleSettingsDatum: SettingsDatum = {
  settingsAdmin: {
    signature: examplePkh,
  },
  metadataAdmin: {
    paymentCredential: {
      VKeyCredential: { bytes: examplePkh },
    },
    stakeCredential: null,
  },
  treasuryAdmin: {
    signature: examplePkh,
  },
  treasuryAddress: {
    paymentCredential: {
      VKeyCredential: { bytes: examplePkh },
    },
    stakeCredential: null,
  },
  treasuryAllowance: [1n, 10n],
  authorizedScoopers: [examplePkh],
  authorizedStakingKeys: [],
  baseFee: 1000000n,
  simpleFee: 100000n,
  strategyFee: 200000n,
  extensions: 0n,
};

export const DatumSchema = Data.Enum([
  Data.Literal("NoDatum"),
  Data.Literal("Foo"),
  //Data.Object({ DatumHash: Data.Bytes() }),
  //Data.Object({ InlineDatum: Data.Any() }),
]);

export const DestinationSchema = Data.Object({
  address: AddressSchema,
  datum: DatumSchema,
});

export const ExtensionSchema = Data.Enum([
  Data.Literal("NoExtension"),
  Data.Literal("Foo"),
]);

export const IdentSchema = Data.Bytes();

export const OrderDatumSchema = Data.Object({
  poolIdent: Data.Nullable(IdentSchema),
  owner: MultiSigScriptSchema,
  scooperFee: Data.Integer(),
  destination: DestinationSchema,
  order: OrderSchema,
  extension: ExtensionSchema,
});
export type OrderDatum = Data.Static<typeof OrderDatumSchema>;
export const OrderDatum = OrderDatumSchema as unknown as OrderDatum;

export const AssetClassSchema = Data.Tuple([Data.Bytes(), Data.Bytes()]);

export const PoolDatumSchema = Data.Object({
  identifier: IdentSchema,
  assets: Data.Tuple([AssetClassSchema, AssetClassSchema]),
  circulatingLp: Data.Integer(),
  feesPer10Thousand: Data.Tuple([Data.Integer(), Data.Integer()]),
  marketOpen: Data.Integer(),
  feeFinalized: Data.Integer(),
  protocolFees: Data.Integer(),
});
export type PoolDatum = Data.Static<typeof PoolDatumSchema>;
export const PoolDatum = PoolDatumSchema as unknown as PoolDatum;

export const OutputReferenceSchema = Data.Object({
  transactionId: Data.Bytes(),
  outputIndex: Data.Integer(),
});

export const IntervalBoundTypeSchema = Data.Enum([
  Data.Object({ NegativeInfinity: Data.Literal("NegativeInfinity") }),
  Data.Object({ Finite: Data.Object ({ value: Data.Integer() }) }),
  Data.Object({ PositiveInfinity: Data.Literal("PositiveInfinity") })
]);

export const ValidityRangeSchema = Data.Object({
  lowerBound: Data.Object({
    boundType: IntervalBoundTypeSchema,
    isInclusive: Data.Boolean(),
  }),
  upperBound: Data.Object({
    boundType: IntervalBoundTypeSchema,
    isInclusive: Data.Boolean(),
  }),
});

export const StrategyExecutionSchema = Data.Object({
  txRef: OutputReferenceSchema,
  validityRange: ValidityRangeSchema,
  details: OrderSchema,
});

export type StrategyExecution = Data.Static<typeof StrategyExecutionSchema>;
export const StrategyExecution = StrategyExecutionSchema as unknown as StrategyExecution;

export const SignedStrategyExecutionSchema = Data.Object({
  strategy: StrategyExecutionSchema,
  signature: Data.Bytes(),
});

export type SignedStrategyExecution = Data.Static<typeof SignedStrategyExecutionSchema>;
export const SignedStrategyExecution = SignedStrategyExecutionSchema as unknown as SignedStrategyExecution;

export const InputOrderItemSchema = Data.Tuple([
  Data.Integer(),
  Data.Nullable(SignedStrategyExecutionSchema),
  Data.Integer(),
]);

export const PoolSpendRedeemerSchema = Data.Enum([
  Data.Object({
    PoolScoop: Data.Object({
      signatoryIndex: Data.Integer(),
      scooperIndex: Data.Integer(),
      inputOrder: Data.Array(InputOrderItemSchema),
    }),
  }),
  Data.Object({
    WithdrawFees: Data.Object({
      amount: Data.Integer(),
      treasuryOutput: Data.Integer(),
    }),
  }),
]);

export const PoolRedeemerSchema = Data.Enum([
  Data.Object({ Spend: PoolSpendRedeemerSchema }),
]);

export type PoolRedeemer = Data.Static<typeof PoolRedeemerSchema>;
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

export const OrderRedeemerSchema = Data.Enum([
  Data.Literal("Scoop"),
  Data.Literal("Cancel"),
]);
export type OrderRedeemer = Data.Static<typeof OrderRedeemerSchema>;
export const OrderRedeemer = OrderRedeemerSchema as unknown as OrderRedeemer;

export const PoolMintRedeemerSchema = Data.Enum([
  Data.Object({ MintLP: Data.Object({ identifier: Data.Bytes() }) }),
  Data.Object({
    CreatePool: Data.Object({
      assets: Data.Tuple([
        AssetClassSchema,
        AssetClassSchema,
      ]),
      poolOutput: Data.Integer(),
      metadataOutput: Data.Integer(),
    }),
  }),
]);
export type PoolMintRedeemer = Data.Static<typeof PoolMintRedeemerSchema>;
export const PoolMintRedeemer = PoolMintRedeemerSchema as unknown as PoolMintRedeemer;

export const exampleSingletonValue: SingletonValue = ["99", "aa", 100n];

export const exampleSwap: Swap = {
  offer: ["", "", 100n],
  minReceived: ["33", "44", 100n],
};

export const exampleCredential: Credential = {
  VKeyCredential: { bytes: "00" },
};

export const exampleOrderDatum: OrderDatum = {
  poolIdent: null,
  owner: {
    signature: examplePkh,
  },
  scooperFee: 2_500_000n,
  destination: {
    address: {
      paymentCredential: {
        VKeyCredential: { bytes: examplePkh },
      },
      stakeCredential: null,
    },
    datum: "NoDatum",
  },
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
  feesPer10Thousand: [3n, 3n],
  marketOpen: 100n,
  feeFinalized: 100n,
  protocolFees: 1_000_000n,
};
