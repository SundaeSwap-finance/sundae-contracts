# sundae-contracts

This repository contains version 3 and onward of the smart contracts for the SundaeSwap Protocol.

> Note: Version 1 consisted of the closed source Plutus v1 contracts written in 2021; Version 2 consisted of a short lived attempt to rewrite them in Plutus v2, before we decided to migrate to Aiken. It's also convenient, as we also released a "v2" of the UI, and so many were getting confused when we spoke of the "v2 contracts", not understanding the difference.

As part of the migration from the old contracts, we authored this plan document: https://docs.google.com/document/d/1iGdx3LXb-5EJNpRe-6u6FTc0k75fg2BIQj8cG5DvQVo/edit#heading=h.2ifsxfxprbh8

## Layout

The repository is organized as follows:
 - validators: The core validator logic for all contracts
 - lib:
   - types: Any type definitions (such as datums and redeemers) used elsewhere in the code
   - calculation: The math-heavy calculations for each order type
 - lucid: Some lucid skeleton work for testing and benchmarking the contracts end to end
 - tests:
   - examples: Tests which print out example serialized datums and redeemers
   - unit: Small, purpose focused tests of an individual unit of the contracts
   - integration: End to end tests that evaluate an entire script context

## Contract architecture

At the highest level, the Sundae v3 protocol is implemented as an extensible set of pool and order contracts, which implement batched AMM order execution and collect protocol fees for doing so.

Some notable differences from the v1 contracts:
 - Scooper fees (now generically called protocol fees) are collected at the pool UTXO, and can be withdrawn to a specific treasury address by a treasury administrator.
 - A pool enforces a "start time", before which no trades can happen.
 - A pool has a starting fee, which decays over some period of time, to a final fee.
 - A pool is not dependent on a specific order contract, only the structure of the order datum.
 - An order can be owned by a generic multisig, including a "script" requirement.
 - An order can donate funds to the pool, useful for protocols that want to build incentives for users
 - An order can be defined as a "strategy", deferring the details of the order until a later date, so long as its signed by some public key
 - The stake address attached to a pool can be updated by a treasury administrator
 - The rewards earned by a pool can be withdrawn to the treasury as protocol fees
 - Scoopers are not issued tokens to authorize them; they are defined in a global settings datum
 - Scoopers define the order inputs are processed, rather than relying on what order they appear in the transaction
