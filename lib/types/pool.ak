use aiken/crypto.{VerificationKey}
use shared.{AssetClass, Ident}
use sundae/multisig
use types/order.{SignedStrategyExecution}

/// The current state of a AMM liquidity pool at a UTXO.
pub type PoolDatum {
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
  /// The basis points to charge on each trade for the liquidity providers
  /// First element is for bid (A -> B), second is for ask (B -> A)
  /// For example, a 1% fee would be represented as 100 (out of 10,000), and a 0.3% fee would be represented as 30
  lp_fee_basis_points: (Int, Int),
  /// The percentage cut of the LP fees the protocol collects, in basis points
  /// First element is for bid (A -> B), second is for ask (B -> A)
  protocol_fee_basis_points: (Int, Int),
  /// An optional multisig condition under which the pool fees can be updated
  fee_manager: Option<multisig.MultisigScript>,
  /// An optional multisig condition under which the staking settings can be updated
  /// If set, this admin can change the stake address attached to the pool, and can update itself
  staking_admin: Option<multisig.MultisigScript>,
  /// Extra scoopers that are authorized to scoop this pool
  /// in addition to those in the settings UTxO
  extra_scoopers: List<VerificationKey>,
  /// The UNIX millisecond timestamp at which trading against the pool should be allowed
  market_open: Int,
  /// The amount of fees the protocol has collected from trading activity
  /// First element is ADA collected from raw scooper fees
  /// Second element is asset A (which may also be ADA) collected as a percentage of LP fees
  /// Third element is asset B (which can never be ADA) collected as a percentage of LP fees
  protocol_fees: (Int, Int, Int),
}

/// A pool UTXO can be spent for two purposes:
pub type PoolRedeemer {
  /// Execute a scoop, a batch of transactions all processed at once
  PoolScoop {
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
    ///
    /// Note that the last integer in the throuple is meaningless; it's used to work around a small quirk of plutus / aiken at the time we wrote the contracts.
    /// Lists of pairs get serialized as cbor maps, which canonically are unordered! so we needed to insert a dummy value so that they get serialized as lists of triples appropriately.
    /// Today, Aiken now has the `Pairs` abstraction to distinguish, and this wouldn't be needed.
    input_order: List<(Int, Option<SignedStrategyExecution>, Int)>,
  }
  /// Withdraw the earned protocol fees into the treasury, or update the pool
  /// fees
  Manage
}

/// We use the pool mint script for two different purposes
pub type PoolMintRedeemer {
  /// to mint LP tokens for the user for a specific pool (by identifier)
  MintLP { identifier: Ident }
  /// to mint the actual pool itself
  CreatePool {
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
  BurnPool { identifier: Ident }
}

/// Manage settings about a pool (used against the pool manage script)
pub type ManageRedeemer {
  /// Withdraw some subset of the fees in the pool, paid into the treasury
  /// amount is (ada_amount, asset_a_amount, asset_b_amount)
  WithdrawFees { amount: (Int, Int, Int), treasury_output: Int, pool_input: Int }
  /// Update the percentage fee the pool charges (requires fee_manager)
  UpdatePoolFees { pool_input: Int }
  /// Update the staking credential on the pool, or rotate the staking admin (requires staking_admin)
  UpdateStaking { pool_input: Int }
}
