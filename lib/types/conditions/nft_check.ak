use cardano/assets.{AssetName, PolicyId}

pub type Check {
  All
  Any
}

pub type NftCheckDatum {
  value: Pairs<PolicyId, Pairs<AssetName, Int>>,
  check: Check,
}
