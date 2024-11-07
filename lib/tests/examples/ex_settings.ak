use aiken/cbor
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output}
use sundae/multisig
use tests/examples/ex_shared.{mk_output_reference,
  print_example, script_address}
use types/settings.{SettingsDatum}

pub const example_settings_admin =
  #"6313a1d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

pub const example_metadata_admin =
  #"7b143ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"

pub const example_treasury_admin =
  #"17bbd2d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

pub const example_treasury_address =
  #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"

pub fn mk_valid_settings_datum(scoopers: List<ByteArray>) -> SettingsDatum {
  SettingsDatum {
    settings_admin: multisig.Signature(example_settings_admin),
    metadata_admin: Address(VerificationKey(example_metadata_admin), None),
    treasury_admin: multisig.Signature(example_treasury_admin),
    treasury_address: Address(VerificationKey(example_treasury_address), None),
    treasury_allowance: (1, 10),
    authorized_scoopers: Some(scoopers),
    authorized_staking_keys: [VerificationKey(example_settings_admin)],
    base_fee: 0,
    simple_fee: 2_500_000,
    strategy_fee: 5_000_000,
    pool_creation_fee: 0,
    extensions: Void,
  }
}

pub fn mk_valid_settings_input(scoopers: List<ByteArray>, ix: Int) -> Input {
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
        value: assets.from_lovelace(2_000_000)
          |> assets.add(hash_of_settings_script, settings_nft_name, 1),
        datum: InlineDatum(settings_datum),
        reference_script: None,
      },
    }
  settings_input
}

test example_settings_datum() {
  print_example(
    cbor.serialise(mk_valid_settings_datum([example_settings_admin])),
  )
}

test example_big_settings_datum() {
  // 30 scoopers at least
  print_example(
    mk_valid_settings_datum(
      [
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02400",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02401",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02402",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02403",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02404",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02405",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02406",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02407",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02408",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02409",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02410",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02411",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02412",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02413",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02414",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02415",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02416",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02417",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02418",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02419",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02421",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02422",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02423",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02424",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02425",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02426",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02427",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02428",
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa02429",
      ],
    ),
  )
}

test example_mainnet_boot_settings_datum() {
  let admin =
    multisig.AtLeast(
      3,
      [
        multisig.Signature(
          #"8582e6a55ccbd7af4cabe35d6da6eaa3d543083e1ce822add9917730",
        ),
        multisig.Signature(
          #"7180d7ad9aaf20658d8f88c32a2e5c287425618c32c9bb82d6b6c8f8",
        ),
        multisig.Signature(
          #"bba4dff30f517f2859f8f295a97d3d85f26a818078f9294256fda2d8",
        ),
        multisig.Signature(
          #"1f68495896a7ba5132198145359311e991a1463e95ccc6f56703653d",
        ),
        multisig.Signature(
          #"f65e667d512b26aa98a97ac22e958e5201e7ea279d74b2e4ec5883db",
        ),
      ],
    )
  print_example(
    SettingsDatum {
      settings_admin: admin,
      metadata_admin: Address(
        Script(#"1854e9028a89496e9772a54882729d16554f8ed9af27ec6046c9a87c"),
        None,
      ),
      treasury_admin: admin,
      treasury_address: Address(
        Script(#"c0d7aa781d14f206f1f6468f0a2d49187d1ebcb8f59c59d75d0c27a7"),
        None,
      ),
      treasury_allowance: (0, 10),
      authorized_scoopers: Some(
        [
          // AAA
          #"570cd6294587645d26c690a72d40fede1e7a28cb3ddc78ff76655820",
          // ATLAS
          #"61f1baeda28f3f83413b92a7d28d2f7b545d718f2f28f971b92b3a21",
          // AZUR
          #"251f7fb11f84f81653ee5b76a10dd29fa36ec7717aafe689490cb7e4",
          // ATADA
          #"6510a3ec0a6f273e31acc82f9f2ffb089413549a04149ea37ef8d33b",
          // AHLNET
          #"9366b01d6baf040245ee07127fc8af4f04a75b91c6a97f69c7f6463a",
          // BLADE
          #"c5825983bb454dd743befc1dd65ee05934666c417503060e1d4fef47",
          // BLOCK
          #"70fa8ce8dda9372aa9b7dc9f5756390e78939744c79550cc3a264b79",
          // BLOOM
          #"f7b1175ea4f7980e717e19c76731e4e6ff3a2ac560dc17a6be8ec204",
          // BCSH
          #"a14cb1a14c4b5810a21103e389f4abdbdec010b766e2dc329a4e0e96",
          // CCV
          #"40282b949abda48a573fe2757971a1369d2674ac9b6d98c1c2bdbdf7",
          // CRDNS
          #"baec408a6fedd39ac0404a2f82c6e75ef06659d8596f9d0af6e01241",
          // DIGI
          #"fe9315a8d1f638a4836e9ec396d43e1f6ba88e45a7f5a5e37a77071a",
          // DSHIN
          #"c6b0d1b88337b91507aa5c6496afc497f399ed8980c2054448eaab6c",
          // KIWI
          #"8ca0e08cdbc30fa0dd21833d7370d666493ecc28b136df179f97fb5d",
          // TERA
          #"f7e1830a1f885aed62fc834c1dffcadd68a8548e88ffd0b0040b960b",
          // XSP
          #"ee8ed5ef92d0a51c6962aac7012906d280aeb412900a7621f782c7c9",
          // WFFL
          #"1ddc54ce9d4d3a35a0ff4707636f8627cc491197ac435ba4fcf4d838",
          // ITC
          #"cba4b71bd8cecc54c526bcd71da84f6f79e568604e574149854dbb86",
          // SIPO
          #"f52cdec15ffcc8ace593dc3e0078458ba07a8c47866ba866e4554b6d",
          // NERD
          #"53d6b12089d642d3bfdc61d5f0f3fddfeeb56f55dcd5bd796b5c25a1",
          // FAIR
          #"dd8a02814820616b137e0fb4852fd8aab36875d849919ca68aa6cb70",
          // NEDS
          #"0b23328355b40d671d1a7ded332c697e1446ae0bb7301af2a7ed9577",
          // PAUL
          #"8be85963f17386d34bcd53b857071a01ee3c7ca543e4bd674492f78b",
          // FROG
          #"b3ec1a4fa75b0c9291af873d3d5365b46ab22eff2bc3f12d5064561b",
          // NORTH
          #"6c8ecf30ba1a025dd324cb0598c8ff87522b324901299cf3f4f1d0b2",
          // GROW
          #"7a7a02beabb674125d734a24817aea9505b9113540cc72f4ef5c2faf",
          // STI
          #"2daa1c8bb06659a89886f6b2677b8d8ccd1c2c7e9ef7ea4be88ac404",
          // TITAN
          #"6a054c69ea18300b0fcb330e07a8b56015d15527f31a361b1b1291de",
          // WAV
          #"ee33744f202c96c2024b25b6cc177ad9537d46437cd48cc291f58009",
          // EASY
          #"37eb116b3ff8a70e4be778b5e8d30d3b40421ffe6622f6a983f67f3f",
        ],
      ),
      authorized_staking_keys: [
        Script(#"4399813dad91bb78a5eb17c26ff50852bc75d3fa7b6e9ae87232ccc1"),
      ],
      base_fee: 332_000,
      simple_fee: 168_000,
      strategy_fee: 168_000,
      pool_creation_fee: 0,
      extensions: 0,
    },
  )
}
