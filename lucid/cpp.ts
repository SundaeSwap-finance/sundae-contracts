import { assertEquals } from "https://deno.land/std@0.193.0/testing/asserts.ts";

type ABL = {
  a: bigint;
  b: bigint;
  liq: bigint;
}

export enum Coin {
  CoinA,
  CoinB,
}

type SwapFees = {
  numerator: bigint;
  denominator: bigint;
}

export function doSwap(coin: Coin, gives: bigint, swapFees: SwapFees, pool: ABL): [bigint, ABL] {
  const diff = swapFees.denominator - swapFees.numerator;
  if (coin == Coin.CoinA) {
    const takes = (pool.b * gives * diff) / (pool.a * swapFees.denominator + gives * diff);
    if (pool.b > takes) {
      const newPool = {
        a: pool.a + gives,
        b: pool.b - takes,
        liq: pool.liq,
      };
      return [takes, newPool];
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
      return [takes, newPool];
    } else {
      throw "Can't do swap";
    }
  } else {
    throw "Invalid coin";
  }
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
