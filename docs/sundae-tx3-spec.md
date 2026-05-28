# SundaeSwap Tx3 Protocol Spec

Status: draft
Purpose: capture the intended scope and roadmap for a Tx3 protocol describing SundaeSwap v3 user and operator transactions.

## Goals

- Provide a Tx3 interface for SundaeSwap v3 smart contracts in this repo.
- Start with the most useful integrator-facing transactions first.
- Keep the first version simple and correct.
- Leave room for later support of advanced order types and operator/admin flows.

## Source of truth

The Tx3 protocol should be derived from the current Aiken contract types and validators in this repo, especially:

- `lib/types/order.ak`
- `lib/types/pool.ak`
- `lib/types/settings.ak`
- `lib/types/oracle.ak`
- `validators/order.ak`
- `validators/pool.ak`
- `validators/settings.ak`
- `validators/oracle.ak`

Note: `lucid/types.ts` appears to be out of date relative to the current Aiken types and should not be treated as the canonical model.

## Protocol model

SundaeSwap v3 is a batched DEX with a two-step flow:

1. A user posts an order UTxO to the order script.
2. A scooper later executes one or more orders against a pool.

Because of that, the Tx3 protocol should likely be split into:

- user-facing order submission/cancellation transactions
- advanced operator/admin transactions

## Initial scope recommendation

The first published Tx3 protocol should focus on user-facing transactions only.

### Phase 1: MVP user-facing transactions

These are the highest priority transactions to model first:

1. `submit_swap`
   - Creates an order UTxO with `Order::Swap`
   - User supplies offered asset, minimum received asset, destination, and max protocol fee

2. `submit_deposit`
   - Creates an order UTxO with `Order::Deposit`
   - User supplies the pair of deposit assets, destination, and max protocol fee

3. `submit_withdrawal`
   - Creates an order UTxO with `Order::Withdrawal`
   - User supplies LP token amount, destination, and max protocol fee

4. `cancel_order`
   - Spends an order UTxO with `OrderRedeemer::Cancel`
   - Authorized by the order owner according to the datum owner field

5. `update_order` (optional but recommended)
   - Cancels an existing order and creates a replacement order in the same transaction
   - Useful UX primitive, even if internally it is cancel + re-post

### Phase 2: advanced user-facing order types

These can be added after MVP:

6. `submit_donation`
   - Creates `Order::Donation`

7. `submit_strategy_order`
   - Creates `Order::Strategy`
   - Requires careful modeling of strategy auth and later strategy execution by scoopers

8. `submit_record_order`
   - Creates `Order::Record`
   - Mainly useful for integrations such as on-chain oracle snapshot flows

### Phase 3: operator/admin transactions

These are real protocol actions, but should be deferred until the basic user protocol is stable:

9. `scoop_batch`
   - Consumes pool + order UTxOs
   - Uses `PoolRedeemer::PoolScoop`
   - Requires settings reference input
   - Requires authorized scooper logic
   - Requires zero-withdrawal witness path through the stake validator
   - May mint LP tokens
   - May mint oracle tokens for record orders
   - This is the hardest transaction in the whole protocol

10. `create_pool`
   - Uses `PoolMintRedeemer::CreatePool`

11. `withdraw_pool_fees`
   - Uses `ManageRedeemer::WithdrawFees`

12. `update_pool_fees`
   - Uses `ManageRedeemer::UpdatePoolFees`

13. `settings_admin_update`
   - Uses `SettingsRedeemer::SettingsAdminUpdate`

14. `treasury_admin_update`
   - Uses `SettingsRedeemer::TreasuryAdminUpdate`

15. `close_empty_pool` / pool shutdown flow
   - Likely tied to fee withdrawal and pool NFT burn behavior
   - Exact packaging should be decided later

## Tx3 design principles

### 1. Prefer high-level transactions over raw low-level primitives

The Tx3 protocol should expose business-level actions such as:

- `submit_swap`
- `submit_deposit`
- `submit_withdrawal`
- `cancel_order`

rather than forcing consumers to work directly with generic redeemer constructors.

### 2. Start simple, then widen coverage

The first version should prioritize:

- simple owners
- fixed destinations
- straightforward order posting
- order cancellation

Advanced features such as strategies, record orders, admin updates, and scoop execution should come later.

### 3. Use the Aiken types as canonical

Datum/redeemer shapes in Tx3 must follow the current Aiken contracts exactly, even if older off-chain code in the repo differs.

## Core on-chain data to model

### Order side

From `lib/types/order.ak`, the main items are:

- `OrderDatum`
- `Destination`
- `Order`
- `OrderRedeemer`
- `StrategyExecution`
- `SignedStrategyExecution`

Important order variants:

- `Swap`
- `Deposit`
- `Withdrawal`
- `Donation`
- `Strategy`
- `Record`

### Pool side

From `lib/types/pool.ak`:

- `PoolDatum`
- `PoolRedeemer`
- `PoolMintRedeemer`
- `ManageRedeemer`

### Settings side

From `lib/types/settings.ak`:

- `SettingsDatum`
- `SettingsRedeemer`

### Oracle side

From `lib/types/oracle.ak`:

- `OracleDatum`
- `OracleRedeemer`

## Simplifications for the MVP Tx3

The MVP protocol should probably intentionally narrow scope in the first version:

- focus on order creation + cancellation only
- prefer fixed destinations over all possible destination patterns
- avoid strategy execution support initially
- avoid record/oracle flows initially
- avoid scoop/admin transactions initially

Possible first-version assumptions:

- owner modeled as a simple signer-controlled multisig case
- destination usually paid back to a wallet address
- optional pool ident supported where practical
- max protocol fee supplied explicitly by the caller

## Known hard parts / risks

### 1. `scoop_batch` is complex

The scoop path involves:

- ordered processing of inputs
- validation of batch ordering
- fee amortization
- pool state transitions
- strategy validation
- LP mint behavior
- record/oracle behavior
- special withdrawal witness logic

This should be treated as a separate implementation phase.

### 2. strategy orders are more than simple order posting

`Order::Strategy` is not just another order constructor. It interacts with:

- strategy authorization
- signed strategy execution payloads
- transaction validity range
- possible withdrawal-based script authorization

So it should not be part of the first draft unless there is a strong need.

### 3. record/oracle flows are integration-oriented

`Order::Record` is part of a broader oracle snapshot design. It is valid protocol behavior, but less central than swap/deposit/withdraw for an initial tx3 release.

## Proposed file/module structure for the Tx3 project

Tentative idea for the future Tx3 package:

- `main.tx3`
  - parties
  - environment
  - core shared types
  - user-facing txs
- later optional split if Tx3 supports/includes modularization cleanly:
  - user txs section
  - operator/admin txs section
  - oracle-related section

## Proposed implementation order

1. write this spec
2. define MVP parties/env/types for order posting
3. implement `submit_swap`
4. implement `submit_deposit`
5. implement `submit_withdrawal`
6. implement `cancel_order`
7. add `update_order`
8. validate against real contract datum/redeemer schemas
9. consider `submit_donation`
10. consider `submit_record_order`
11. consider `submit_strategy_order`
12. only then consider `scoop_batch` and admin/operator flows

## Open questions

These should be resolved before or during implementation:

1. Should the first public Tx3 protocol be user-facing only, or include operator flows too?
2. How much of Sundae multisig ownership should be exposed in v1 of the protocol?
3. Should `update_order` be explicit in Tx3, or left as a client-composed pattern?
4. Should pool-specific order submission be modeled with a dynamic pool/order script party or via parameters?
5. How should reference deployment/config data be represented in Tx3 env fields?
6. When implementing `scoop_batch`, should the protocol describe the full transaction or only a constrained operator-facing subset?

## Immediate next step

Use this spec to draft a first `main.tx3` containing only the MVP user-facing SundaeSwap order lifecycle:

- `submit_swap`
- `submit_deposit`
- `submit_withdrawal`
- `cancel_order`
- optionally `update_order`
