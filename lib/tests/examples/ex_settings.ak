use aiken/bytearray
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/cbor
use sundae/multisig
use aiken/transaction/value
use types/settings.{SettingsDatum}
use aiken/transaction.{Input, Output, InlineDatum}
use tests/examples/ex_shared.{script_address, mk_output_reference}

pub fn mk_valid_settings_datum(
  scoopers: List<ByteArray>,
) -> SettingsDatum {
  SettingsDatum {
    settings_admin: multisig.Signature(
      #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
    ),
    metadata_admin: Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    ),
    treasury_admin: multisig.Signature(
      #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
    ),
    treasury_address: Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    ),
    treasury_allowance: (1, 10),
    authorized_scoopers: scoopers,
    authorized_staking_keys: [
      #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
    ],
    base_fee: 0,
    simple_fee: 2_500_000,
    strategy_fee: 5_000_000,
    extensions: Void,
  }
}

pub fn mk_valid_settings_input(
  scoopers: List<ByteArray>,
  ix: Int,
) -> Input {
  let hash_of_settings_script =
    #"00000000000000000000000000000000000000000000000000000000"
  let settings_nft_name = "settings"
  let settings_address = script_address(hash_of_settings_script)
  let settings_datum = mk_valid_settings_datum(scoopers)
    
  let settings_input =
    Input {
      output_reference: mk_output_reference(ix),
      output: Output {
        address: settings_address,
        value: value.from_lovelace(2_000_000)
          |> value.add(hash_of_settings_script, settings_nft_name, 1),
        datum: InlineDatum(settings_datum),
        reference_script: None,
      },
    }
  settings_input
}

test example_settings_datum() {
  trace bytearray.to_hex(
    cbor.serialise(mk_valid_settings_datum([
      #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
    ])),
  )
  True
}