use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/transaction.{InlineDatum, Output}
use shared.{AssetClass, Ident}
use sundae/multisig
use types/order.{SignedStrategyExecution}

/// The current state of a AMM liquidity pool at a UTXO.
pub type ConditionPoolDatum {
  /// the unique identifier of the pool. Produced by hashing one of the input UTXOs used to produce the pool
  /// to ensure uniqueness.
  identifier: Ident,
  /// The two asset IDs that this pool can exchange, in alphabetical order
  /// Used to validate that the assets being swapped are indeed the intended assets
  assets: (AssetClass, AssetClass),
  /// The total number of LP tokens in circulation
  /// Maintains the following two invariants on each deposit or withdrawal:
  /// - circulating_lp is always equal to the number of LP tokens that have been minted and are in circulation
  /// - A users LP tokens (or burned LP tokens), as a percentage of the circulating LP tokens, represent the percentage of assets they just deposited or withdrew.
  circulating_lp: Int,
  // An optional multisig condition under which the protocol fees can be updated
  fee_manager: Option<multisig.MultisigScript>,
  /// The UNIX millisecond timestamp at which trading against the pool should be allowed
  /// TODO: deposits and arguably withdrawals should be processed before the market open
  market_open: Int,
  /// The amount of ADA on the UTXO that is set aside by collecting protocol fees
  /// This should be increased on each scoop to represent collecting fees; deducted from the reserve amount (if one of the tokens in the pair is ADA)
  /// to calculate the swap amounts, and decreased when some amount is withdrawn.
  /// Note that this also allows us to conveniently sidestep minUTXO woes, because the creator of the pool can set the initial protocol fees to whatever minUTXO is needed
  /// and withdrawals never have to be for the full amount.
  /// TODO: should we add a field to the settings object to set a minimum initial protocol_fees on pool mint?
  protocol_fees: Int,
  /// The condition script that needs to be present in the withdrawals
  condition: ScriptHash,
  /// If the condition needs extra data it can be stored in this field
  condition_datum: Data,
}

/// A pool UTXO can be spent for two purposes:
pub type ConditionPoolRedeemer {
  /// Execute a scoop, a batch of transactions all processed at once
  ConditionPoolScoop {
    /// The index in the transaction extra_signatories that represents the "scooper" signature
    /// This lets us quickly skip to the scooper if there are multiple signatures on the transaciton for some reason
    /// And this is safe to do, because at the end of the day the value pointed at has to be in the list of authorized scoopers anyway
    signatory_index: Int,
    /// The index in the list of authorized scoopers on the settings datum of the scooper processing the order
    /// This is so that we can quickly skip to the correct scooper in the list of authorized scoopers
    /// And this is safe to do, because at the end of the day, the pointed at scooper must have signed the transaction
    scooper_index: Int,
    /// The order to process the transaction inputs in, and optionally the signed strategy execution to execute for strategy orders
    /// This is because the cardano ledger re-orders transaction inputs when building the script context; so this could end up processing
    /// users orders out of order, and open the protocol to sandwich attacks and line-cutting.
    /// Much of the complexity of the protocol comes from ensuring this list is processed both efficiently and safely.
    input_order: List<(Int, Option<SignedStrategyExecution>, Int)>,
    /// A withdrawal only scoop does should always be allowed, no matter the condition
    withdrawal_only: Bool,
  }
  /// Withdraw the earned protocol fees into the treasury, or update the pool
  /// fees
  ConditionManage
}

/// We use the pool mint script for two different purposes
pub type ConditionPoolMintRedeemer {
  /// to mint LP tokens for the user for a specific pool (by identifier)
  ConditionMintLP { identifier: Ident }
  /// to mint the actual pool itself
  ConditionCreatePool {
    /// The assets to create the pool with;
    /// provided so we can enforce that they are paid to the pool output, and the pool datum is constructed correctly
    assets: (AssetClass, AssetClass),
    /// The index in the outputs that corresponds to the pool output
    /// This is done so we can efficiently skip to the correct output
    /// This is safe because we validate that the datum, value, and pool token are paid into this UTXO
    pool_output: Int,
    /// The index in the outputs that the corresponding CIP-68 metadata token is paid to
    /// Note that the tokens are structured as such:
    ///  - A CIP-68 (222) token, indicating an NFT, held by the pool script address
    ///  - A CIP-68 (333) token, indicating a fungible token, paid anywhere the user wants; this is the LP token
    ///  - A CIP-68 (111) token; Whichever UTXO holds this token can set a datum that determines on-chain metadata like decimals, name, and image
    /// That token is given to a "metadata administrator" who can set the appropriate metadata, and update it if needed.
    /// This is done so we can efficiently skip to the correct metadata output
    /// This is safe because we validate that the token is paid to the metadata admin
    metadata_output: Int,
  }
  /// to burn the pool NFT (when permitted by the spending validator)
  ConditionBurnPool { identifier: Ident }
}

/// Manage settings about a pool (used against the pool manage script)
pub type ConditionManageRedeemer {
  // Withdraw some subset of the fees in the pool, paid into the treasury
  ConditionWithdrawFees { amount: Int, treasury_output: Int, pool_input: Int }
  // Update the percentage fee the pool charges
  ConditionUpdateCondition { pool_input: Int }
}

pub fn find_pool_output(outputs: List<Output>) -> (Output, ConditionPoolDatum) {
  // Find the pool output; we can assume the pool output is the first output, because:
  // - The ledger doesn't reorder outputs, just inputs
  // - We check that the address is correct, so if the first output was to a different contract, we would fail
  // - We check that the datum is the correct type, meaning we can't construct an invalid pool output
  // - Later, we check that the pool output has the correct value, meaning it *must* contain the pool token, so we can't pay to the pool script multiple times
  expect Some(pool_output) = list.head(outputs)
  expect InlineDatum(output_datum) = pool_output.datum
  expect output_datum: ConditionPoolDatum = output_datum
  (pool_output, output_datum)
}
