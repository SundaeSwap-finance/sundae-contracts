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

export function doSwap(coin: Coin, gives: bigint, swapFees: SwapFees, pool: ABL): [ABL, ABL] {
  const diff = swapFees.denominator - swapFees.numerator;
  if (coin == Coin.CoinA) {
    const takes = (pool.b * gives * diff) / (pool.a * swapFees.denominator + gives * diff);
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
    const takes = (pool.a * gives * diff) / (pool.b * swapFees.denominator + gives * diff);
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
  if (bInUnitsOfA > giveA) {
    let change = pool.b * (bInUnitsOfA - giveA) / pool.a;
    finalA = giveA;
    finalB = giveB - change;
  } else {
    let change = giveA - bInUnitsOfA;
    finalA = giveA - change;
    finalB = giveB;
  }
  let issuedLPTokens = finalA * pool.liq / pool.a;

  const newPool = {
    a: pool.a + finalA,
    b: pool.b + finalB,
    liq: pool.liq + issuedLPTokens,
  };

  return [{ a: 0n, b: 0n, liq: issuedLPTokens }, newPool];
}

Deno.test("doSwap", () => {
  let pool: ABL = {
    a: 1_000_000_000n,
    b: 1_000_000_000n,
    liq: 1_000_000_000n,
  };
  let takes: bigint = 0n;
  const swapFees: SwapFees = { numerator: 1n, denominator: 2000n };
  [takes, pool] = doSwap(Coin.CoinA, 10_000_000n, swapFees, pool);
  assertEquals(takes, 9_896_088n);
  [takes, pool] = doSwap(Coin.CoinA, 10_000_000n, swapFees, pool);
  assertEquals(takes, 9_702_095n);
});
