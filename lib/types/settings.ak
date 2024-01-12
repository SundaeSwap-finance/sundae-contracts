use aiken/transaction/credential.{Address, VerificationKey}
use sundae/multisig.{MultisigScript}
use shared.{Rational}
use aiken/transaction.{
  InlineDatum, Input, Output,
}
use aiken/builtin
use aiken/transaction/value.{AssetName, PolicyId}
use aiken/dict

/// Various protocol-wide settings, mostly used as reference inputs to other transactions
pub type SettingsDatum {
  /// The conditions under which the critical settings can be updated
  /// For example, this may be a multisig script, or have additional script conditions using a staking withdrawal address
  /// The settings the settings_admin is allowed to update are:
  /// - settings_admin, to allow rotating their own keys
  /// - metadata_admin, to allow setting a new metadata admin at the will of the DAO
  /// - treasury_admin, to allow setting the treasury admin, at the will of the DAO
  /// - authorized_scoopers, to allow adding and removing scoopers in accordance with the will of the DAO
  /// - base_fee, simple_fee, strategy_fee, to set the monetary policy
  /// - extensions, to allow configuring arbitrary non-binding settings data
  settings_admin: MultisigScript,

  /// The address to which the CIP-68 (100) tokens are minted with a void datum, to allow administration of the LP tokens image
  metadata_admin: Address, // Who or what governs the evolution of CIP-68 token metadata

  /// The conditions under which the treasury admin can act
  /// For example, this may be a multisig script, or have additional script conditions using a stake withdrawal address
  /// The treasury admin is allowed to withdraw funds from a pool into the treasury, and can set the following settings
  /// - treasury_address, to allow setting the destination of the DAO treasury withdrawals; TODO: should this instead be the settings admin, for separation of powers?
  /// - authorized_staking_keys, to establish a policy of which pools each liquidity pool can be delegated to (each pool can be changed at the time of withdrawal)
  treasury_admin: MultisigScript, // Who can withdraw to the treasury and control delegation

  /// The address to which the treasury funds must be withdrawn, such as a governance DAO; TODO: should this be a destination or something similar? Should it enforce a null datum?
  treasury_address: Address, // Where to withdraw treasury funds

  /// An allowed withholding that the treasury admin can keep to pay administrative fees
  /// Asking for a DAO vote for each bit of administrative overhead would quickly deter and overwhelm people from participating in governance
  /// So, the DAO can instead set a specific percentage of operating funds that go to operating costs
  /// The treasury administrator is responsible for keeping this percentage, and then paying for things like the scoopers, server infrastructure, etc.
  treasury_allowance: Rational, // When withdrawing, what percentage can be released for administrative purposes?
  
  /// The list of actors who are allowed to execute scoops
  /// If set to None, *anyone* is allowed to execute a scoop
  /// Note: this would forfeit first-come-first-serve and MEV protection that Sundae
  /// currently enjoys, but this at least provides the DAO with flexibility to make that choice.
  authorized_scoopers: Option<List<VerificationKey>>,
  
  /// The list of authorized staking keys that can be attached to a pool; It's a list, so that if the protocol TVL exceeds the saturation point for a single pool,
  /// or the DAO directs the treasury administrator to split the pool delegation across multiple pools, the treasury administrator can do so.
  /// Also note, that if any one pool ever exceeds the delegation percentage, it can't be split further. This is a problem we intend to solve with a future version of the protocol
  authorized_staking_keys: List<VerificationKey>,

  /// The base fee, in lovelace, to be collected each time a batch happens; can be shared among all participants in the batch.
  base_fee: Int,
  /// The additional fee, in lovelace, to charge each "simple" order, such as a swap, deposit, withdraw, etc.
  simple_fee: Int,
  /// The additional fee, in lovelace, to charge each "strategy" execution;
  /// This is configurable separately because the DAO may decide to collect more from automated strategies
  /// or give automated strategies a discount, to incentivize more volume since it's automated.
  strategy_fee: Int,

  /// Any arbitrary "extension" data that can be added to the datum; This isn't used by any existing scripts, but may be used to:
  /// - publish data for convenient and canonical access by off-chain actors
  /// - expose data to the script conditions in the multisig scripts of the two administrator roles
  /// - expose additional settings for more advanced pool and order types to operate off of
  // TODO: Perhaps we should split this into separate fields, with different permissions to update it from each administrator
  // for example, maybe there is a "settings_extensions", "treasury_extensions", etc.
  extensions: Data,
}

/// The settings redeemer can be spent for two different purposes
pub type SettingsRedeemer {
  /// To make an update by the settings administrator
  SettingsAdminUpdate
  /// To make an update by the treasury administrator
  TreasuryAdminUpdate
}

/// The name of the token that authenticates the settings UTXO
pub const settings_nft_name: AssetName = "settings"

/// Scan over the list of reference inputs to find the settings datum, and ensure it's the correct one
/// Note that this makes the assumption that the settings datum is the first reference input, for performance
/// This means that when storing reference scripts on-chain, there needs to be a small amount of "farming" to select
/// UTXOs that are higher than the settings datum input. Awkward, but GOTTA GO FAST!
pub fn find_settings_datum(reference_inputs: List<Input>, settings_policy_id: PolicyId) -> SettingsDatum {
  // Assume the settings input is at the head of the list.
  let settings_output = builtin.head_list(reference_inputs).output
  /// Convert the value on this UTXO to a list we can easily scan over
  let settings_value_list = dict.to_list(value.to_dict(settings_output.value))
  /// Since it will always only have ADA and the settings NFT, we make assumptions about this structure
  /// TODO: make sure the settings spend scripts enforce this condition
  expect [_, (found_policy_id, settings_dict)] = settings_value_list
  /// Check that the policy ID on that singular token matches the expected settings policy ID
  /// This ensures that we have the real settings UTXO, and not one that someone just paid ADA into with an arbitrary datum
  expect found_policy_id == settings_policy_id
  /// Check that the asset name and quantity exactly match what we expect
  /// TODO: is this neccesary? if we only allow minting a single settings token, it can't be anything else
  expect dict.to_list(settings_dict) == [(settings_nft_name, 1)]
  /// Extract the datum; note, this makes the assumption that the settings datum will always be inline
  /// but this is a safe assumption to make because we don't expect to compose the spending of the settings datum
  /// with other scripts
  expect InlineDatum(settings_datum) = settings_output.datum
  /// Cast it to the right type, to ensure it's not some other random datum!
  expect settings_datum: SettingsDatum = settings_datum
  settings_datum
}
