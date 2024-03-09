use calculation/swap.{do_swap, swap_takes}
use calculation/shared as calc_shared
use shared.{SingletonValue, to_value}
use aiken/transaction.{Output, NoDatum}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{AssetName, PolicyId, from_lovelace, merge}
use aiken/fuzz.{int_between, int, map2, map3, map, constant, bool}
use aiken/hash.{blake2b_256}
use types/order.{Order, Swap, Fixed}
use aiken/bytearray
use aiken/math.{abs}

pub fn byte() -> Fuzzer<ByteArray> {
    int_between(0, 255) |> map2(constant(#""), _, bytearray.push)
}

fn policy_id() -> Fuzzer<PolicyId> {
    byte() |> map(blake2b_256)
}

pub fn positive_int() -> Fuzzer<Int> {
    int() |> map(abs) |> map(fn(x) { x + 1 })
}

pub fn asset_name() -> Fuzzer<AssetName> {
    byte() |> map(blake2b_256) |> map2(int_between(0,32), bytearray.take)
}

fn singleton_value() -> Fuzzer<SingletonValue> {
    map3(policy_id(), asset_name(), positive_int(), fn(a,b,c) { (a,b,c) })
}

fn lexographical() -> Fuzzer<(SingletonValue, SingletonValue)> {
    map2(singleton_value(), singleton_value(), fn(a,b) {
        when bytearray.compare(a.1st, b.1st) is {
            Less -> (a,b)
            Equal -> when bytearray.compare(a.2nd, b.2nd) is {
                Greater -> (b,a)
                _ -> (a,b)
            }
            Greater -> (b,a)
        }
    })
}

fn pool_state() -> Fuzzer<calc_shared.PoolState> {
    map2(lexographical(), singleton_value(), fn(values, liq) { calc_shared.PoolState(values.1st, values.2nd, liq) })
}

type SwapTestCase {
    swap: Order,
    pool_state: calc_shared.PoolState,
}

fn random_test() -> Fuzzer<SwapTestCase> {
    map3(
        pool_state(),
        positive_int(),
        bool(),
        fn(pool_state, swap_amt, a_to_b) {
            let swap_give = if a_to_b {
                (pool_state.quantity_a.1st, pool_state.quantity_a.2nd, swap_amt)
            } else {
                (pool_state.quantity_b.1st, pool_state.quantity_b.2nd, swap_amt)
            }
            let swap_take = if a_to_b {
                (pool_state.quantity_b.1st, pool_state.quantity_b.2nd, 0)
            } else {
                (pool_state.quantity_a.1st, pool_state.quantity_a.2nd, 0)
            }
            SwapTestCase {
                swap: Swap(swap_give, swap_take),
                pool_state: pool_state,
            }
        }
    )
}

test constant_product_should_always_increase(case via random_test()) {
    expect Swap(offer, min) = case.swap
    let input_value = from_lovelace(1) |> merge(to_value(offer)) 
    let input = Output {
        address: Address {
            payment_credential: VerificationKeyCredential(#""),
            stake_credential: None,
        },
        value: input_value,
        datum: NoDatum,
        reference_script: None,
    }
    let address = Address {
        payment_credential: VerificationKeyCredential(#""),
        stake_credential: None,
    }
    let destination = Fixed {
        address: address,
        datum: NoDatum,
    }
    let (pool_give, pool_take) = if offer.1st == case.pool_state.quantity_a.1st && offer.2nd == case.pool_state.quantity_a.2nd {
        (case.pool_state.quantity_a.3rd, case.pool_state.quantity_b.3rd)
    } else {
        (case.pool_state.quantity_b.3rd, case.pool_state.quantity_a.3rd)
    }
    let (_, out_value) = swap_takes(
        offer.1st,
        offer.2nd,
        min.1st,
        min.2nd,
        pool_give,
        pool_take,
        30,
        0,
        offer.3rd,
        input_value,
    )
    let output = Output {
        address: address,
        value: out_value,
        datum: NoDatum,
        reference_script: None,
    }

    let new_pool_state = do_swap(
        case.pool_state,
        input,
        destination,
        30,
        0,
        offer,
        min,
        output,
    )

    new_pool_state.quantity_a.3rd * new_pool_state.quantity_b.3rd >= case.pool_state.quantity_a.3rd * case.pool_state.quantity_b.3rd
}