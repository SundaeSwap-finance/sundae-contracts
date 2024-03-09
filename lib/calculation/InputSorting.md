# Input Sorting

Perhaps one of the most nuanced and subtle parts of the contracts is ensuring we process the orders in the correct order [sic].

So, it's worth writing a dedicated section explaining how this code works.

## Why is this even neccesary?

Unfortunately, the Cardano ledger will sort the inputs before passing them to the script context and executing your script.

This is because technically speaking, the ledger defines the transaction inputs as a set, so order isn't guaranteed. Therefore, since the value/datums have to be looked up from the UTxO, it's much more efficient to fetch those in the order they are in the UTxO, rather than the transaction, and avoid sorting them according to the tx ordering.

Unfortunately, for a DEX, this is very damaging. The ordering of these inputs can greatly impact the outcome. For the v1 contracts, this means we have to break the batches up into small monotonically increasing segments. Whereas the contracts could probably fit 8 orders per batch, in practice it's usually 2-4 before the next transaction that *should* be processed is out-of-order with the existing inputs.

So, a big innovation of the new contracts allows us to efficiently reorder the inputs according to the scoopers intention. However, done carelessly, this leads to many inefficiencies or bugs.

## Naive implementation

One simple way to do this is to have the scoop redeemer have an array of integers, and just sort the inputs based on that order.

However, this is horribly inefficient because of the primitives we have available to us.  For example, with no random access, accessing the Nth input means we have to iterate from 0 to N.  So, in total, we end up traversing the input list O(n^2) times.

## Conditions

So, how do we achieve this in our contracts? We have to carefully ensure the following conditions are true:

### Every order on the inputs is processed

We handle this by making the assumption that every script input other than the pool input is an order. We then count up the script inputs, subtract off one for the pool input, and then make sure that, after traversing the redeemer list, we've processed that many orders.

If someone includes extra, non-order scripts, then this will force them to be "considered" by the recursive order processing loop. If they have the correctly structured datum, then they can be processed as an order! If not, then matching on the order datum will fail.

The biggest risk would be if someone elsewhere in the ecosystem created a script that:
 - Shouldn't be processed as a SundaeSwap order
 - Happened to have a datum that could be parsed exactly as if it were a Sundae order
 - Happened to have outputs on the transaction that matched the result of the "order" that was described
 - Didn't fail for its own logic

And it's hard to imagine a script that meets these criteria and isn't also just fundamentally flawed anyway.

### Every order on the inputs is processed correctly

To ensure that the orders are processed *correctly*, we recursively traverse and process each one according to the rules in the datum. Since the above condition forces us to visit each order, this recursive traversal ensures that each one is processed according to the rules in the datum itself. This one was pretty easy to meet.

### Every order on the inputs is processed **exactly once**

The astute above might have noticed a clever hack:

A malicious scooper could include the right number of entries in the redeemer, but omit some orders and repeat others.

This would allow them to unlock the funds from the other orders, so long as they satisfied the repeated order multiple times.

For example, suppose a scooper included a single "swap 5 ada for RBERRY" order, and then 19 "swap 10,000 ADA for RBERRY" orders; then, in the redeemer, repeated "input 1" 20 times.

The orders would allow themselves to be spent, because the pool NFT was in the transaction; The pool would allow it to be spent, so long as you paid out 20 different outputs, each with a tiny amount of RBERRY *and* deposited the correct amount of ADA into the pool (100 ADA, easily fundable from the stolen orders).

To avoid this, we need to make sure that each order in the redeemer set is unqiue, and we went through a number of iterations to implement and optimize this.

For example, just naively checking for unique entries is still O(n^2) work: loop through each item in the redeemer, and check if you've seen it before in the list.

Instead, the key innovation is: we implement the uniqueness check by taking advantage of UPLC arbitrary size integers, and emulating bitwise operations with arithmetic!

As we loop over each entry in the redeemer set, such as one with index 7, we flip the 7th bit on a `uniqueness_flags` integer.

b0000_0100_0000

If we ever set the same bit twice, we'd like to fail.

However, UPLC currently doesn't have bitwise primitives, making this difficult.

Flipping the bit is easy enough, as we can just add in the `n`th power of 2. But checking if the bit is already set is tricky.

It was @Microproofs who came up with the bit of mathematical ingenuity to check for this.

We make the observation that if we add a power of two, and that bit is already set, it results in a carry operation. The number overall will be larger, but the number if we mask off any "higher" bits (i.e. if we put on horse blinders to only the bits up to the bit we just flipped), the number will decrease. Another way to picture it is that an integer carry will "carry" the one out of our field of view, resulting in a drop in value.

So, we can use the modulo operator to "scrub" these higher bits:
 - 0b1010_0101 modulo 2^5 (0b0001_0000) gives 0b0000_0101, exactly as if we focused on only the 4 lowest order bits.
 - 0b1010_1101 modulo 2^5 gives 0b0000_1101, which is greater than the above.
 - 0b1010_1101 + 0b0000_1000 = 0b1011_0101, and 0b1011_0101 modulo 2^5 gives 0b0101, which has decreased, signalling that we set the same bit twice.

So, by maintaining a bit flag, setting each bit for the index of the order we process, and failing if we ever set the same bit twice, we can ensure that **every order is processed once and only once**.

### Matching to outputs

In the v1 contracts, we processed orders by summing up a dictionary of destinations, to the value they were entitled to.

If there were multiple orders from the same address, they would get merged into the same output, resulting in a slightly smaller transaction.

However, in practice, the merging of Value's and traversal of dictionaries is expensive, and so we developed a different approach for the v3 contracts.

Instead, each order corresponds to 1 (or in one particular corner case, 0) outputs. And, since the outputs *are not* reordered by the script context, we can safely just recursively traverse these in order, checking that the address, datum, and value are all in accordance with the current order we're instructed to process. 

### Avoiding redundant list traversals

The last major trick that won a large performance gain is being careful about when we restart the traversal of the list.

Naively, if for each index we started from the beginning of the list of inputs to find the input at index `i`, then we would still be doing O(n^2) work.  In fact, with only linear list traversals, it's possible that O(n^2) is the best you can do in the general case.

However, in the best case, the inputs are already in order, and we can take advantage of this structure and just recursively `head / tail` traverse the list.

So, while recursing, we carefully keep two lists: the *whole* list of inputs, and the *rest* of the inputs from this traversal.  So long as the next index is ahead of us, we can continue to traverse the `rest` of the inputs. And once the next index is lower than the index we're currently processing, we have the list of all inputs so we can start over.

In this way, the scooper can provide a list of inputs, and in average case, we perform much better than simple naive traversal.