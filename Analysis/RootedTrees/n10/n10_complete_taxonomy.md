# Complete Compositional Taxonomy of N=10 Trees

## Overview

The 719 trees at n=10 can be categorized into two main groups:
1. **Direct Extensions** (572 trees): Formed by applying Nest or Widen to an n=9 parent
2. **Special Combinations** (147 trees): Formed by combining smaller structures from different levels

## Category A: Direct Extensions from N=9 Parents (572 Trees)

### Generation Rule

At n=10, each n=9 parent tree produces descendants through exactly **one or two** operations:
- **Nest**: Wrap the parent in `()` — always available
- **Widen**: Add an atom sibling `()` at root level — available when the parent already has multiple children

### Distribution Summary

| Operation | Count |
|:----------|:------|
| **Nest** | 286 |
| **Widen** | 286 |
| **Total Direct** | 572 |

---

### Family 1: Descendants of n=9 Tree 1 `((((((((())))))))` (9-Chain)

The 9-Chain generates the maximum-depth descendant at n=10.

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 1 | `(((((((((())))))))` | **Nest** | V⁹ | 10 | 1 |

*Note: Tree 1 is the maximum-depth 10-Chain (Church Numeral 9)*

---

### Families 2-286: Single and Multi-Child Families

Each n=9 parent produces descendants through Nesting and (where applicable) Widening operations.

| Parent Type | Nesting | Widening | Total |
|:------------|:--------|:---------|:------|
| Single-child (depth-heavy) | ✓ | — | 1 each |
| Multi-child (width > 1) | ✓ | ✓ | 2 each |

---

## Category B: Special Combinations (147 Trees)

These trees are not direct descendants of any single n=9 parent. They are formed by combining structures from different levels based on partitions of (n-1) = 9 into parts of size 2 or greater.

### Valid Partitions of 9 into Parts ≥ 2

| Partition | Combinations | Description |
|:----------|:-------------|:------------|
| **[7, 2]** | 48 | Each of 48 n=7 trees + n=2 container |
| **[6, 3]** | 40 | 20 n=6 trees × 2 n=3 trees |
| **[5, 4]** | 36 | 9 n=5 trees × 4 n=4 trees |
| **[5, 2, 2]** | 9 | Each of 9 n=5 trees + two n=2 containers |
| **[4, 3, 2]** | 8 | 4 n=4 trees × 2 n=3 trees × n=2 |
| **[3, 3, 3]** | 4 | Unique ternary n=3 combinations |
| **[3, 2, 2, 2]** | 2 | Each n=3 tree + three n=2 containers |
| **Total** | **147** | |

---

### Type 1: n=7 + n=2 Combinations (48 trees)

Each of the 48 trees at n=7 combines with the single n=2 container `(())`:

| # | Tree | n=7 Child | n=2 Child | Depth | Width |
|:--|:-----|:----------|:----------|:------|:------|
| 402 | `(((((((()))))))(()))` | `((((((()))))))` (7-Chain) | `(())` | 8 | 2 |
| 404 | `((((((()())))))(()))` | `(((((()())))))`  | `(())` | 7 | 2 |
| 406 | `((((((())()))))(()))` | `(((((())()))))`  | `(())` | 7 | 2 |
| ... | *(45 more trees from indices 403-449)* | ... | ... | ... | ... |
| 449 | `((()()()()()())(()))` | `(()()()()()())` (Septenary) | `(())` | 3 | 2 |

*Note: Tree indices are not strictly consecutive; shown are representative examples.*

**Significance**: These represent **all 48 ways** to asymmetrically combine each n=7 computational pattern (including the ternary structures from n=7) with a simple container.

---

### Type 2: n=6 + n=3 Combinations (40 trees)

Each of the 20 trees at n=6 combines with each of the 2 trees at n=3:

| # | Tree | n=6 Child | n=3 Child | Depth | Width |
|:--|:-----|:----------|:----------|:------|:------|
| 498 | `((((((())))))((()))` | `((((((()))))))` (6-Chain) | `((()))` (Vertical) | 7 | 2 |
| 499 | `((((((())))))(()()))` | `((((((()))))))` (6-Chain) | `(()())` (Horizontal) | 7 | 2 |
| 502 | `(((((()()))))((()))` | `((((()()))))`  | `((()))` (Vertical) | 6 | 2 |
| ... | ... | ... | ... | ... | ... |
| 537 | `((()()()()())(()()))` | `(()()()()())` (Quinary) | `(()())` (Horizontal) | 3 | 2 |

**Significance**: These represent **all 20×2 = 40 ways** to combine each n=6 structure with the n=3 duality. This is the complete cross-product of n=6 with the fundamental binary choice.

---

### Type 3: n=5 + n=4 Combinations (36 trees)

Each of the 9 trees at n=5 combines with each of the 4 trees at n=4:

| # | Tree | n=5 Child | n=4 Child | Depth | Width |
|:--|:-----|:----------|:----------|:------|:------|
| 578 | `(((((()))))(((())))` | `((((()))))` (5-Chain) | `(((())))` (4-Chain) | 6 | 2 |
| 579 | `(((((()))))((()()))` | `((((()))))` (5-Chain) | `((()()))` (Nested Fork) | 6 | 2 |
| 580 | `(((((()))))((())())` | `((((()))))` (5-Chain) | `((())())` (Chain+Sibling) | 6 | 2 |
| 581 | `(((((()))))(()()())` | `((((()))))` (5-Chain) | `(()()())` (Ternary) | 6 | 2 |
| ... | ... | ... | ... | ... | ... |
| 613 | `((()()()())(()()())` | `(()()()())` (Quaternary) | `(()()())` (Ternary) | 3 | 2 |

**Significance**: These represent **all 9×4 = 36 ways** to combine each n=5 structure with the n=4 vocabulary. This is the first complete cross-product of n=5 with the vocabulary level.

---

### Type 4: n=5 + n=2 + n=2 Combinations (9 trees)

Each of the 9 trees at n=5 combines with two n=2 containers:

| # | Tree | n=5 Child | n=2 Children | Depth | Width |
|:--|:-----|:----------|:-------------|:------|:------|
| 584 | `(((((()))))(())(()))` | `((((()))))` (5-Chain) | `(())`, `(())` | 6 | 3 |
| 593 | `((((()())))(())(()))` | `(((()())))`  | `(())`, `(())` | 5 | 3 |
| 602 | `((((())()))(())(()))` | `(((())()))`  | `(())`, `(())` | 5 | 3 |
| 611 | `(((()()()))(())(()))` | `((()()()))`  | `(())`, `(())` | 4 | 3 |
| 620 | `((((()))())(())(()))` | `(((()))())`  | `(())`, `(())` | 5 | 3 |
| 629 | `(((()())())(())(()))` | `((()())())`  | `(())`, `(())` | 4 | 3 |
| 638 | `(((())(()))(())(()))` | `((())(()))`  | `(())`, `(())` | 4 | 3 |
| 647 | `(((())()())(())(()))` | `((())()())`  | `(())`, `(())` | 4 | 3 |
| 656 | `((()()()())(())(()))` | `(()()()())`  | `(())`, `(())` | 3 | 3 |

**Significance**: These are **ternary combinations** with one complex n=5 component and two simple containers. They extend the [4,2,2] pattern from n=9.

---

### Type 5: n=4 + n=3 + n=2 Combinations (8 trees)

Each of the 4 trees at n=4 combines with each of the 2 trees at n=3, plus an n=2 container:

| # | Tree | n=4 Child | n=3 Child | n=2 Child | Depth | Width |
|:--|:-----|:----------|:----------|:----------|:------|:------|
| 663 | `((((())))((()))(()))` | `(((())))` (4-Chain) | `((()))` (Vertical) | `(())` | 5 | 3 |
| 665 | `((((())))(()())(()))` | `(((())))` (4-Chain) | `(()())` (Horizontal) | `(())` | 5 | 3 |
| 673 | `(((()()))((()))(()))` | `((()()))` (Nested Fork) | `((()))` (Vertical) | `(())` | 4 | 3 |
| 675 | `(((()()))(()())(()))` | `((()()))` (Nested Fork) | `(()())` (Horizontal) | `(())` | 4 | 3 |
| 682 | `(((())())((()))(()))` | `((())())` (Chain+Sibling) | `((()))` (Vertical) | `(())` | 4 | 3 |
| 684 | `(((())())(()())(()))` | `((())())` (Chain+Sibling) | `(()())` (Horizontal) | `(())` | 4 | 3 |
| 690 | `((()()())((()))(()))` | `(()()())` (Ternary) | `((()))` (Vertical) | `(())` | 3 | 3 |
| 692 | `((()()())(()())(()))` | `(()()())` (Ternary) | `(()())` (Horizontal) | `(())` | 3 | 3 |

**Significance**: These are **complete 4×2 = 8 combinations** of the vocabulary with the duality, each accompanied by a simple container. This represents the full cross-product of fundamental structures at a ternary level.

---

### Type 6: n=3 + n=3 + n=3 Ternary Symmetric Combinations (4 trees)

**First appearance of ternary n=3 combinations.** The 2 trees at n=3 combine in all unique ternary groupings:

| # | Tree | Children | Depth | Width |
|:--|:-----|:---------|:------|:------|
| 697 | `(((()))((()))((()))` | `((()))`, `((()))`, `((()))` | 4 | 3 |
| 698 | `(((()))((()))(()())` | `((()))`, `((()))`, `(()())` | 4 | 3 |
| 701 | `(((()))(()())(()())` | `((()))`, `(()())`, `(()())` | 4 | 3 |
| 702 | `((()())(()())(()())` | `(()())`, `(()())`, `(()())` | 3 | 3 |

**Significance**: These 4 trees represent **all unique ternary combinations** of the n=3 duality:
- 3× Vertical: `((()))((()))((()))`
- 2× Vertical + 1× Horizontal: `((()))((()))(()())`
- 1× Vertical + 2× Horizontal: `((()))(()())(()())`
- 3× Horizontal: `(()())(()())(()())`

This is the **first ternary combination at the duality level**—analogous to how [4,4] appeared at n=9 for the vocabulary level.

---

### Type 7: n=3 + n=2 + n=2 + n=2 Combinations (2 trees)

Each of the 2 trees at n=3 combines with three n=2 containers:

| # | Tree | n=3 Child | n=2 Children | Depth | Width |
|:--|:-----|:----------|:-------------|:------|:------|
| 704 | `(((()))(())(())(()))` | `((()))` (Vertical) | `(())`, `(())`, `(())` | 4 | 4 |
| 711 | `((()())(())(())(()))` | `(()())` (Horizontal) | `(())`, `(())`, `(())` | 3 | 4 |

**Significance**: These are **quaternary combinations** with one n=3 duality component and three simple containers. Each n=3 structure (vertical and horizontal) combines with the quaternary symmetric pattern.

---

## Summary Tables

### Distribution by Category

| Category | Count | Trees |
|:---------|:------|:------|
| **Direct Extensions (Nest)** | 286 | 1–286 (various) |
| **Direct Extensions (Widen)** | 286 | Various |
| **Special [7,2]** | 48 | 402–449 |
| **Special [6,3]** | 40 | 498–537 |
| **Special [5,4]** | 36 | 578–613 |
| **Special [5,2,2]** | 9 | 584, 593, 602, 611, 620, 629, 638, 647, 656 |
| **Special [4,3,2]** | 8 | 663, 665, 673, 675, 682, 684, 690, 692 |
| **Special [3,3,3]** | 4 | 697, 698, 701, 702 |
| **Special [3,2,2,2]** | 2 | 704, 711 |
| **Total** | **719** | |

### Depth Distribution

| Depth | Count | Description |
|:------|:------|:------------|
| 10 | 1 | 10-Chain (Tree 1) |
| 9 | ~20 | Deep nested structures |
| 8 | ~45 | |
| 7 | ~75 | |
| 6 | ~100 | |
| 5 | ~140 | Modal depth |
| 4 | ~180 | Secondary mode |
| 3 | ~120 | Shallow structures |
| 2 | 1 | Nonary Forest (Tree 719) |

### Width Distribution

| Width | Count | Description |
|:------|:------|:------------|
| 1 | 286 | Pure nested (all Nest operations) |
| 2 | ~220 | Binary combinations |
| 3 | ~110 | Ternary combinations |
| 4 | ~55 | Quaternary |
| 5 | ~25 | Quinary |
| 6 | ~12 | Senary |
| 7 | ~6 | Septenary |
| 8 | ~3 | Octenary |
| 9 | 1 | Nonary Forest (Tree 719) |

---

## Key Observations

### 1. Growth of Special Combinations

| Level | Special Combinations | Growth |
|:------|:---------------------|:-------|
| n=5 | 1 | — |
| n=6 | 2 | ×2.0 |
| n=7 | 8 | ×4.0 |
| n=8 | 19 | ×2.4 |
| n=9 | 56 | ×2.9 |
| n=10 | 147 | ×2.6 |

The special combinations continue to grow substantially, following approximately exponential growth.

### 2. First n=7 + n=2 Combinations

For the first time, all 48 trees at n=7 appear in asymmetric combinations with n=2. This continues the complete coverage pattern.

### 3. Complete n=6 × n=3 Cross-Product

All 20 n=6 trees combine with both n=3 trees, creating a complete 20×2 = 40 matrix of combinations.

### 4. Complete n=5 × n=4 Cross-Product

All 9 n=5 trees combine with all 4 n=4 trees, creating a complete 9×4 = 36 matrix—the first complete cross-product of n=5 with the vocabulary.

### 5. First Ternary n=3 Combinations

The [3,3,3] partition generates 4 trees—all possible ternary combinations of the n=3 duality with itself. This includes 2 self-triples and 2 mixed triples.

### 6. Balanced Operations

The 286-286 split between Nest and Widen operations maintains perfect generative balance.

---

## The Extremes at N=10

| Type | Tree | Structure | Significance |
|:-----|:-----|:----------|:-------------|
| **Max Depth** | 1 | `(((((((((())))))))` | 10-Chain, Church Numeral 9 |
| **Max Breadth** | 719 | `(()()()()()()()()())` | Nonary Forest, 9-argument function |

The depth-breadth spectrum at n=10 spans from 10 to 2, demonstrating the full range from maximum sequential composition (9 nested function calls) to maximum parallel application (9 independent arguments).

---

## Predictions for N=11

For n=11 (1842 trees), partitions of 10 into parts ≥ 2:
- **[8, 2]**: 115 combinations (one per n=8 tree)
- **[7, 3]**: 96 combinations (48 n=7 trees × 2 n=3 trees)
- **[6, 4]**: 80 combinations (20 n=6 trees × 4 n=4 trees)
- **[6, 2, 2]**: 20 combinations (each n=6 tree + two n=2)
- **[5, 5]**: 45 combinations (unique n=5 pairings)
- **[5, 3, 2]**: 18 combinations (each n=5 × each n=3 × n=2)
- **[4, 4, 2]**: 10 combinations (unique n=4 pairings × n=2)
- **[4, 3, 3]**: 12 combinations (4 n=4 × unique n=3 pairings)
- **[4, 2, 2, 2]**: 4 combinations (each n=4 + three n=2)
- **[3, 3, 2, 2]**: 3 combinations (unique n=3 pairings × n=2 pair)
- **[2, 2, 2, 2, 2]**: 1 combination (**quinary symmetric**)

Expected special combinations at n=11: **~404**

At n=11, we will see the **first quinary combination** (5 × n=2) and the **first symmetric n=5 combinations** ([5,5] partition).
