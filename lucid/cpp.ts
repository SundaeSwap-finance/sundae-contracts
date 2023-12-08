import { assertEquals } from "https://deno.land/std@0.193.0/testing/asserts.ts";

export type ABL = {
  a: bigint;
  b: bigint;
  liq: bigint;
}

export enum Coin {
  CoinA,
  CoinB,
}

export type SwapFees = {
  numerator: bigint;
  denominator: bigint;
}

export function doSwap(coin: Coin, gives: bigint, feesPer10k: bigint, pool: ABL): [ABL, ABL] {
  const diff = 10_000n - feesPer10k;
  if (coin == Coin.CoinA) {
    const takes = (pool.b * gives * diff) / (pool.a * 10_000n + gives * diff);
    if (pool.b > takes) {
      const newPool = {
        a: pool.a + gives,
        b: pool.b - takes,
        liq: pool.liq,
      };
      return [{ a: 0n, b: takes, liq: 0n, }, newPool];
    } else {
      throw "Can't do swap";
    }
  } else if (coin == Coin.CoinB) {
    const takes = (pool.a * gives * diff) / (pool.b * 10_000n + gives * diff);
    if (pool.a > takes) {
      const newPool = {
        a: pool.a - takes,
        b: pool.b + gives,
        liq: pool.liq,
      };
      return [{ a: takes, b: 0n, liq: 0n, }, newPool];
    } else {
      throw "Can't do swap";
    }
  } else {
    throw "Invalid coin";
  }
}

export function doDeposit(giveA: bigint, giveB: bigint, pool: ABL): [ABL, ABL] {
  let bInUnitsOfA = giveB * pool.a / pool.b;
  let finalA = 0n;
  let finalB = 0n;
  let issuedLPTokens = 0n;
  let out: ABL = { a: 0n, b: 0n, liq: 0n, };
  if (bInUnitsOfA > giveA) {
    let change = pool.b * (bInUnitsOfA - giveA) / pool.a;
    finalA = giveA;
    finalB = giveB - change;
    issuedLPTokens = finalA * pool.liq / pool.a;
    out = {
      a: 0n,
      b: change,
      liq: issuedLPTokens,
    };
  } else {
    let change = giveA - bInUnitsOfA;
    finalA = giveA - change;
    finalB = giveB;
    issuedLPTokens = finalA * pool.liq / pool.a;
    out = {
      a: change,
      b: 0n,
      liq: issuedLPTokens,
    };
  }

  const newPool = {
    a: pool.a + finalA,
    b: pool.b + finalB,
    liq: pool.liq + issuedLPTokens,
  };

  return [out, newPool];
}

export function doWithdrawal(giveLP: bigint, pool: ABL): [ABL, ABL] {
  let takesA = giveLP * pool.a / pool.liq;
  let takesB = giveLP * pool.b / pool.liq;

  const out = {
    a: takesA,
    b: takesB,
    liq: 0n,
  };

  const newPool = {
    a: pool.a - takesA,
    b: pool.b - takesB,
    liq: pool.liq - giveLP,
  };

  return [out, newPool];
}

Deno.test("doSwap", () => {
  let pool: ABL = {
    a: 1_000_000_000n,
    b: 1_000_000_000n,
    liq: 1_000_000_000n,
  };
  let takes: ABL | null = null;
  const swapFees: bigint = 5n;
  [takes, pool] = doSwap(Coin.CoinA, 10_000_000n, swapFees, pool);
  assertEquals(takes.b, 9_896_088n);
  [takes, pool] = doSwap(Coin.CoinA, 10_000_000n, swapFees, pool);
  assertEquals(takes.b, 9_702_095n);
});
