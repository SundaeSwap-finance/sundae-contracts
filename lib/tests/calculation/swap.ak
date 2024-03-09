use aiken/transaction.{NoDatum, InlineDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value
use calculation/shared.{PoolState} as calc_shared
use sundae/multisig
use types/order.{Fixed, OrderDatum}
use calculation/swap.{do_swap}


test swap_mintakes_too_high() fail {
  let addr =
    Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let ada = (#"", #"")
  let rberry = (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  let pool_state = PoolState {
    quantity_a: (#"", #"", 1_000_000_000),
    quantity_b: (rberry.1st, rberry.2nd, 1_000_000_000),
    quantity_lp: (lp.1st, lp.2nd, 1_000_000_000),
  }
  let input_value =
    value.from_lovelace(14_500_000)
  let swap_offer = (ada.1st, ada.2nd, 10_000_000)
  let swap_min_received = (rberry.1st, rberry.2nd, 10_000_000)
  let order = OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2_500_000,
    destination: Fixed {
      address: addr,
      datum: NoDatum,
    },
    details: order.Swap { offer: swap_offer, min_received: swap_min_received, },
    extension: Void,
  }
  let output = Output {
    address: addr,
    value: value.from_lovelace(2_000_000)
      |> value.add(rberry.1st, rberry.2nd, 9_896_088),
    datum: NoDatum,
    reference_script: None,
  }
  let input = Output {
    address: addr,
    value: input_value,
    datum: InlineDatum(order),
    reference_script: None,
  }
  let final_pool_state = do_swap(pool_state, input, order.destination, 5, 2_500_000, swap_offer, swap_min_received, output)
  expect final_pool_state.quantity_a.3rd == 1_000_000_000 + 10_000_000
  expect final_pool_state.quantity_b.3rd == 1_000_000_000 - 9_896_088
  True
}
