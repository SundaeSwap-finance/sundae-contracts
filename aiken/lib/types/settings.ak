use aiken/transaction/credential.{VerificationKey}

pub type SettingsDatum {
  authorized_scoopers: List<VerificationKey>,
}
