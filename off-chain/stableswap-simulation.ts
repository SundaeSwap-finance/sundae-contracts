
/** Liquidity invariant as defined in the aiken contracts */
function liquidity_invariant(
    new_pool_gives: bigint,
    new_pool_takes: bigint,
    linear_amplification: bigint,
    new_sum_invariant: bigint
  ) {
    // 4 * A * 4 * (x*y) * D + D^3 - (4(x*y) * (4A(x + y) + D))
    // 4A * 4xy * D + D^3 - (4xy * 4a * (x + y) + 4xy * D)
    // 16Axy * D + D^3 - (16Axy * (x + y) + 4xy * D)
    let four_a = 4n * linear_amplification;
    let four_x_y = 4n * new_pool_gives * new_pool_takes;
    let d_plus_one = new_sum_invariant + 1n;
    let d_cubed = new_sum_invariant * new_sum_invariant * new_sum_invariant;
    let d_plus_one_cubed = d_plus_one * d_plus_one * d_plus_one;
    let x_plus_y = new_pool_gives + new_pool_takes;
    let sixteen_a_x_y = four_a * four_x_y;
    let sixteen_a_x_y_x_plus_y = sixteen_a_x_y * x_plus_y;
    let f1 =
      sixteen_a_x_y * new_sum_invariant +
      d_cubed -
      (sixteen_a_x_y_x_plus_y + four_x_y * new_sum_invariant);
    let f2 =
      sixteen_a_x_y * d_plus_one +
      d_plus_one_cubed -
      (sixteen_a_x_y_x_plus_y + four_x_y * d_plus_one);
    return f1 <= 0n && f2 > 0n;
  }

    /** Exchange invariant as defined by the aiken contracts */
    function exchange_invariant(
      new_pool_gives: bigint,
      new_pool_takes: bigint,
      linear_amplification: bigint,
      old_sum_invariant: bigint
    ) {
      // 4xy (4A(x + y) + D) - (4xy*4A*D + D^3)
      // 4x(y - 1) (4A(x + y - 1) + D) - (4x(y - 1)*4A*D + D^3)
      // (4xy - 4x) (4A(x + y - 1) + D) - ((4xy - 4x)*4A*D + D^3)
      let d_cubed = old_sum_invariant * old_sum_invariant * old_sum_invariant;
      let four_xy = 4n * new_pool_gives * new_pool_takes;
      let x_plus_y = new_pool_gives + new_pool_takes;
      let four_a = 4n * linear_amplification;
      let g1 =
        four_xy * (four_a * x_plus_y + old_sum_invariant) -
        four_xy * four_a * old_sum_invariant -
        d_cubed;
      let g2 =
        (four_xy - 4n * new_pool_gives) *
          (four_a * (x_plus_y - 1n) + old_sum_invariant) -
        (four_xy - 4n * new_pool_gives) * four_a * old_sum_invariant -
        d_cubed;
      return g1 >= 0n && g2 < 0n;
    }
  
  /** Curve on-chain implementation */
  function get_D_curve(a: bigint, x: bigint, y: bigint) {
    /*
      S: uint256 = 0
      for x in _xp:
          S += x
      if S == 0:
          return 0
      */
    let sum: bigint = x + y;
    if (sum === 0n) {
      return 0n;
    }
  
    /*
      D: uint256 = S
      Ann: uint256 = _amp * N_COINS
      */
    let d = sum;
    let ann = a * 2n;
    for (let i = 0; i < 255; i++) {
      /*
              D_P: uint256 = D
              for x in _xp:
                  D_P = D_P * D / x
              D_P /= pow_mod256(N_COINS, N_COINS)
              Dprev: uint256 = D
            */
      let d_p = (d * d * d) / (4n * x * y);
      let d_prev = d;
  
      /*
          D = (
                (unsafe_div(Ann * S, A_PRECISION) + D_P * N_COINS) * D
                /
                (
                    unsafe_div((Ann - A_PRECISION) * D, A_PRECISION) +
                    unsafe_add(N_COINS, 1) * D_P
                )
            )
          */
      d =
        (((ann * sum) / 100n + d_p * 2n) * d) /
        (((ann - 100n) * d) / 100n + 3n * d_p);
  
      if (d > d_prev) {
        if (d - d_prev <= 1) {
          
            return d;
          
        }
      } else {
        if (d_prev - d <= 1) {
          
            return d;
         
        }
      }
    }
    throw new Error();
  }
  
  function get_y_curve(new_x: bigint, a: bigint, d: bigint): bigint {
    let sum = 0n;
    let y_prev = 0n;
    let c = d;
    let ann = a * 2n;
  
    // for i = 0
    sum += new_x;
    c = (c * d) / (new_x * 2n);
  
    // for i = 1
    // continue
  
    // for i = 2
    // break
  
    c = (c * d * a_prec) / (ann * 2n);
    let b = sum + (d * a_prec) / ann;
    let y = d;
  
    // The aiken contracts expect this value to be rounded up. Considering we are approximating using integers, we use the exchange invariant to determine if we should round up or not.
    for (let i = 0; i < 255; i++) {
      y_prev = y;
      y = (y * y + c) / (2n * y + b - d);
      if (y > y_prev) {
        if (y - y_prev <= 1n) {
          if (exchange_invariant(new_x, y, a/2n/a_prec, d)) {
            return y;
          } else {
            return y+=1n;
          }
        }
      } else {
        if (y_prev - y <= 1n) {
          if (exchange_invariant(new_x, y, a/2n/a_prec, d)) {
            return y;
          } else {
            return y + 1n;
          }
        }
      }
    }
  
    throw new Error();
  }
  // NOTE: implemented just so we can compare with on-chain results
  // currently don't intend to implement this on-chain
  function dynamic_fee_curve(x: bigint, y: bigint, fee_base: bigint) {
    const off_peg_fee_multiplier = 100000000000n;
    if (off_peg_fee_multiplier <= fee_prec) {
      return fee_base;
    }
  
    let xpsq = (x + y) * (x + y);
    let fee_num = off_peg_fee_multiplier * fee_base;
    let fee_denom =
      ((off_peg_fee_multiplier - fee_prec) * 4n * x * y) / xpsq + fee_prec;
    return fee_num / fee_denom;
  }
  
  // The on chain implementation uses a fixed fee
  function fixed_fee(x: bigint, y: bigint, fee_base: bigint) {
    return fee_base;
  }
  
  /** Textbook Implementation */
  function get_D_textbook(a: bigint, x: bigint, y: bigint) {
    a = a / (2n * a_prec);
    let p = 4n * x * y * (4n * a - 1n);
    let q = 16n * a * x * y * (x + y);
    let term1 = q / 2n;
    let term1Sq = term1 * term1;
    let term2 = p / 3n;
    let term2Cube = term2 * term2 * term2;
    let sqrtTerm = iroot(term1Sq + term2Cube, 2n);
    return iroot(term1 + sqrtTerm, 3n) + iroot(term1 - sqrtTerm, 3n);
  }
  function get_y_textbook(
    x: bigint,
    A: bigint,
    D: bigint,
    maxIter = 256,
    tol = 1n
  ): bigint {
    if (x === 0n) {
      throw new Error("x cannot be zero");
    }
    // NOTE: we convert a from the "curve" scaling to the textbook scaling
    A = A / (2n * a_prec);
  
    // We'll start with a guess for y
    let y = D / 2n;
    if (y === 0n) {
      y = 1n;
    }
  
    for (let i = 0; i < maxIter; i++) {
      // f(y) as a Rational
      // f(y) = 4A(x + y) + D - 4AD - (D^3 / (4*x*y))
      const term1 = 4n * A * (x + y) + D - 4n * A * D; // integer part
      const term2: Rational = {
        num: D * D * D, // numerator for D^3
        den: 4n * x * y, // denominator = 4*x*y
      };
      // f(y) = term1 - term2
      const f = subR({ num: term1, den: 1n }, term2);
  
      // f'(y) = 4A + D^3 / (4*x*y^2)
      const dTerm2: Rational = {
        num: D * D * D,
        den: 4n * x * y * y,
      };
      const df = addR({ num: 4n * A, den: 1n }, dTerm2);
  
      // If f(y) = 0 => done
      // Otherwise Newton step: y_new = y - f/df
      const delta = divR(f, df); // this is (f/df) as rational
      const deltaInt = toBigIntFloor(delta);
  
      if (deltaInt === 0n) {
        // We might still be "close"
        // check if f is within tolerance
        if (absBigInt(f.num / f.den) <= tol) {
          return y;
        }
      }
  
      // y_{next} = y - delta
      const yNext = y - deltaInt;
      if (yNext <= 0n) {
        throw new Error("solveForYTextbook: y became non-positive");
      }
  
      if (absBigInt(yNext - y) <= tol) {
        return yNext;
      }
  
      y = yNext;
    }
  
    throw new Error("solveForYTextbook did not converge");
  }
  
  /** Constants */
  let a_prec = 100n;
  // Default fee precision (basis points * 1_000_000 for extra precision in intermediate calculations)
  // Note: Individual pools can override this via fee_denominator in the datum
  let default_fee_prec = 10_000_000_000n;
  let rates = 1_000_000_000_000_000_000_000_000_000_000n;
  let precision = 1_000_000_000_000_000_000n;
  // On-chain calc_precision from shared.ak
  let calc_precision = 1_000_000_000_000n;
  
  interface SwapX {
    dx: bigint;
    expected_dy?: bigint;
    fee?: bigint;
  }
  
  interface SwapY {
    dy: bigint;
    expected_dx?: bigint;
    fee?: bigint;
  }
  
  type Swap = SwapX | SwapY;
  
  interface TestCase {
    label: string;
    a: bigint;
    x: bigint;
    y: bigint;
    fee_basis: bigint;
    protocol_fee_basis: bigint;
    // Pre-scaling coefficients for normalizing tokens with different decimals
    // e.g., for USDC (6 decimals) + DAI (18 decimals): prescale = [1_000_000_000_000n, 1n]
    // Default is [1n, 1n] for equal decimals
    prescale?: [bigint, bigint];
    // Fee denominator for the pool (default: 10_000 for basis points)
    fee_denominator?: bigint;
    scenarios: Swap[][];
  }
  interface Method {
    label: string;
    get_D: (a: bigint, x: bigint, y: bigint) => bigint;
    get_y: (x: bigint, a: bigint, d: bigint) => bigint;
    fee: (x: bigint, y: bigint, base_fee: bigint) => bigint;
  }
  
  const methods: Method[] = [
    {
      label: "Curve Onchain",
      get_D: get_D_curve,
      get_y: get_y_curve,
      fee: fixed_fee,
    },
  ];
  
  /** Add more testcases here */
  const cases: TestCase[] = [
    {
      label: "Example 1 - Equal decimals",
      a: 400n,
      x: 1_000_000_000n,
      y: 1_000_000_000n,
      fee_basis: 5n * 1_000_000n,
      protocol_fee_basis: 10n * 1_000_000n,
      scenarios: [[{ dx: 10_000_000n },{ dx: 10_000_000n }]],
    },
    {
      label: "Example 2 - USDC/DAI style (6 vs 18 decimals)",
      a: 400n,
      // USDC with 6 decimals: 1M USDC = 1_000_000_000_000 base units
      x: 1_000_000_000_000n,
      // DAI with 18 decimals: 1M DAI = 1_000_000_000_000_000_000_000_000 base units
      y: 1_000_000_000_000_000_000_000_000n,
      fee_basis: 5n * 1_000_000n,
      protocol_fee_basis: 10n * 1_000_000n,
      // Prescale normalizes: USDC * 10^12 = DAI * 1
      prescale: [1_000_000_000_000n, 1n],
      scenarios: [[{ dx: 1_000_000n }]],  // Swap 1 USDC
    },
    {
      label: "Example 3 - Custom fee denominator (parts per million)",
      a: 400n,
      x: 1_000_000_000n,
      y: 1_000_000_000n,
      // 50 parts per million LP fee
      fee_basis: 50n * 1_000_000n,
      // 100 parts per million protocol fee
      protocol_fee_basis: 100n * 1_000_000n,
      // Use parts per million instead of basis points
      fee_denominator: 1_000_000n,
      scenarios: [[{ dx: 10_000_000n }]],
    },
  ];
  
  function run(test_case: TestCase) {
    let { a: a_init, x: x_init, y: y_init, fee_basis, scenarios, protocol_fee_basis } = test_case;
    // Extract prescale with default of [1, 1] (no scaling)
    let [prescale_x, prescale_y] = test_case.prescale ?? [1n, 1n];
    // Extract fee_denominator with default of 10_000 (basis points)
    // We multiply by 1_000_000 to match the fee_prec scaling used internally
    let fee_prec = (test_case.fee_denominator ?? 10_000n) * 1_000_000n;
    let a_curr = a_init * a_prec;

    let compact_test_data = "";

    for (const { label: method, get_D, get_y, fee } of methods) {
      console.log(`=== ${test_case.label} (${method}) ===`);
      console.log(`  Prescale: [${prescale_x}, ${prescale_y}]`);
      console.log(`  Fee denominator: ${test_case.fee_denominator ?? 10_000n}`);
      for (const scenario of scenarios) {
        let x_curr = (x_init * rates) / precision;
        let y_curr = (y_init * rates) / precision;
        for (const swap of scenario) {
          console.log(
            `Pool ${(x_curr * precision) / rates} / ${
              (y_curr * precision) / rates
            }`
          );
          let x_virt = x_curr;
          let y_virt = y_curr;
          // Apply prescale for D calculation (similar to on-chain: reserve * prescale * calc_precision)
          let x_scaled = x_virt * prescale_x;
          let y_scaled = y_virt * prescale_y;
          let dx_virt = 0n;
          let expected_dy_virt: bigint | undefined;
          let give = "";
          let take = "";
          let prescale_give = prescale_x;
          let prescale_take = prescale_y;
          if ("dx" in swap) {
            give = "X";
            take = "Y";
            dx_virt = swap.dx;
            expected_dy_virt = swap.expected_dy;
          } else if ("dy" in swap) {
            give = "Y";
            take = "X";
            x_virt = y_curr;
            y_virt = x_curr;
            x_scaled = y_curr * prescale_y;
            y_scaled = x_curr * prescale_x;
            prescale_give = prescale_y;
            prescale_take = prescale_x;
            dx_virt = swap.dy;
            expected_dy_virt = swap.expected_dx;
          }
          try {
            // Calculate D using prescaled values
            let d = get_D(a_curr, x_scaled, y_scaled);
            console.log(`  D = ${d}`);
            let dx_precise = (dx_virt * rates) / precision;
            let new_x_precise = x_virt + dx_precise;
            // Apply prescale when computing new_y
            let new_x_scaled = new_x_precise * prescale_give;
            let new_y_scaled = get_y(new_x_scaled, a_curr, d);
            // Convert back from prescaled to actual
            let new_y = new_y_scaled / prescale_take;
            let dy_precise = y_virt - new_y;
            let actual_lp_fee = fee(
              (x_virt + new_x_precise) / 2n,
              (y_virt + new_y) / 2n,
              fee_basis
            );
            let actual_protocol_fee = fee(
              (x_virt + new_x_precise) / 2n,
              (y_virt + new_y) / 2n,
              protocol_fee_basis
            );
            let actual_fee = actual_lp_fee + actual_protocol_fee;
            let out_fee =
              (((dy_precise * actual_fee) / fee_prec) * precision + rates - 1n) /
              rates;
            let protocol_fee = out_fee * actual_protocol_fee / actual_fee;
            let dy = (dy_precise * precision) / rates - out_fee;
            console.log(
              `  ${dx_virt} ${give} => ${dy} ${take} Fee: ${out_fee} Raw swap result: ${dy_precise}`
            );
            if (expected_dy_virt !== undefined && dy != expected_dy_virt) {
              console.log(`    (mismatch; expected ${expected_dy_virt})`);
            }
            if ("dx" in swap) {
              x_curr = new_x_precise;
              y_curr = y_virt - ((dy + protocol_fee) * rates) / precision;
            } else {
              x_curr = y_virt - ((dy + protocol_fee) * rates) / precision;
              y_curr = new_x_precise;
            }
            // Calculate final D using prescaled values
            let final_d = get_D(a_curr, x_curr * prescale_x, y_curr * prescale_y);
            compact_test_data = `${compact_test_data}\n(${final_d},${dy_precise},${dy}),`;
          } catch (e) {
            console.log(`${dx_virt} => ERROR`);
            console.error(e);
          }
        }
        console.log(
          `Pool ${(x_curr * precision) / rates} / ${(y_curr * precision) / rates}`
        );
        console.log(`${x_curr} ${y_curr}`);
        // Calculate final D using prescaled values
        let final_d = get_D(a_curr, x_curr * prescale_x, y_curr * prescale_y);
        console.log(`  D = ${final_d}`);
        console.log("----------------------------------");
        console.log(`Compact test data for ${test_case.label} (${method}):\n ${compact_test_data}`);
      }
    }
  }
  
  for (const tc of cases) {
    run(tc);
  }
  
  /**************
   * Utilities
   **************/
  
  type Rational = {
    num: bigint;
    den: bigint;
  };
  
  function addR(a: Rational, b: Rational): Rational {
    return {
      num: a.num * b.den + b.num * a.den,
      den: a.den * b.den,
    };
  }
  
  function subR(a: Rational, b: Rational): Rational {
    return {
      num: a.num * b.den - b.num * a.den,
      den: a.den * b.den,
    };
  }
  
  function mulR(a: Rational, b: Rational): Rational {
    return {
      num: a.num * b.num,
      den: a.den * b.den,
    };
  }
  
  function divR(a: Rational, b: Rational): Rational {
    return {
      num: a.num * b.den,
      den: a.den * b.num,
    };
  }
  
  function toBigInt(r: Rational, rounding: "down" | "up" = "down"): bigint {
    return rounding === "down" ? r.num / r.den : (r.num + r.den - 1n) / r.den;
  }
  function toBigIntFloor(r: Rational): bigint {
    return r.num / r.den;
  }
  
  function absBigInt(n: bigint): bigint {
    return n < 0n ? -n : n;
  }
  
  function iroot(base: bigint, root: bigint): bigint {
    if (root % 2n == 1n && base < 0) {
      return -iroot(-base, root);
    }
    let s = base + 1n;
    let k1 = root - 1n;
    let u = base;
    while (u < s) {
      s = u;
      u = (u * k1 + base / u ** k1) / root;
    }
    return s;
  }
  