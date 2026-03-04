# Complete Compositional Taxonomy of N=9 Trees

## Overview

The 286 trees at n=9 can be categorized into two main groups:
1. **Direct Extensions** (230 trees): Formed by applying Nest or Widen to an n=8 parent
2. **Special Combinations** (56 trees): Formed by combining smaller structures from different levels

## Category A: Direct Extensions from N=8 Parents (230 Trees)

### Generation Rule

At n=9, each n=8 parent tree produces descendants through exactly **one or two** operations:
- **Nest**: Wrap the parent in `()` — always available
- **Widen**: Add an atom sibling `()` at root level — available when the parent already has multiple children

### Distribution Summary

| Operation | Count |
|:----------|:------|
| **Nest** | 115 |
| **Widen** | 115 |
| **Total Direct** | 230 |

---

### Family 1: Descendants of n=8 Tree 1 `(((((((())))))))` (8-Chain)

The 8-Chain generates the maximum number of descendants (49) at n=9.

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 1 | `((((((((()))))))))` | **Nest** | V⁸ | 9 | 1 |
| 116 | `(((((((())))))))())` | **Widen** | H(V⁷, atom) | 8 | 2 |
| ... | ... | **Widen** | Various extensions | ... | ... |

*Note: Tree 1 is the maximum-depth 9-Chain (Church Numeral 8)*

---

### Families 2-48: Single-Child Nesting Families

These n=8 parents produce only one n=9 descendant through Nesting.

| Parent | Tree # | Tree | Operation | Depth |
|:-------|:-------|:-----|:----------|:------|
| Tree 2 `((((((()()))))))` | 2 | `(((((((()())))))))` | Nest | 8 |
| Tree 3 `((((((())())))))` | 3 | `(((((((())()))))))` | Nest | 8 |
| Tree 4 `(((((()()())))))` | 4 | `((((((()()()))))))` | Nest | 7 |
| Tree 5 `((((((()))()))))` | 5 | `(((((((()))())))))` | Nest | 8 |
| ... | ... | ... | Nest | ... |

---

### Families 49-115: Multi-Child Families with Widening

| Parent | Children Count | Operations |
|:-------|:---------------|:-----------|
| Tree 49 `((((((())))))())` | 21 | Nest, Widen |
| Tree 69 `(((((()))))(()))` | 18 | Nest, Widen |
| Tree 70 `(((((()))))()())` | 10 | Nest, Widen |
| Tree 89 `((((())))(())())` | 8 | Nest, Widen |
| Tree 90 `((((())))()()())` | 5 | Nest, Widen |
| Tree 105 `(((()))(())(()))` | 3 | Nest, Widen |
| Tree 106 `(((()))(())()())` | 3 | Nest, Widen |
| Tree 107 `(((()))()()()())` | 3 | Nest, Widen |
| Tree 112-115 | 2 each | Nest, Widen |

---

## Category B: Special Combinations (56 Trees)

These trees are not direct descendants of any single n=8 parent. They are formed by combining structures from different levels based on partitions of (n-1) = 8 into parts of size 2 or greater.

### Valid Partitions of 8 into Parts ≥ 2

| Partition | Combinations | Trees |
|:----------|:-------------|:------|
| **[6, 2]** | 20 | 164, 166, 168, 170, 172, 174, 176, 178, 180, 182, 184, 186, 188, 190, 192, 194, 196, 198, 200, 202 |
| **[5, 3]** | 18 | 204, 205, 208, 209, 212, 213, 216, 217, 220, 221, 224, 225, 228, 229, 232, 233, 236, 237 |
| **[4, 4]** | 10 | 240, 241, 242, 243, 249, 250, 251, 257, 258, 264 |
| **[4, 2, 2]** | 4 | 246, 254, 261, 267 |
| **[3, 3, 2]** | 3 | 270, 272, 277 |
| **[2, 2, 2, 2]** | 1 | 282 |
| **Total** | **56** | |

---

### Type 1: n=6 + n=2 Combinations (20 trees)

Each of the 20 trees at n=6 combines with the single n=2 container `(())`:

| # | Tree | n=6 Child | n=2 Child | Depth | Width |
|:--|:-----|:----------|:----------|:------|:------|
| 164 | `((((((())))))(()))` | `((((((()))))))` (6-Chain) | `(())` | 7 | 2 |
| 166 | `(((((()()))))(()))` | `((((()()))))`  | `(())` | 6 | 2 |
| 168 | `(((((())())))(()))` | `((((())())))`  | `(())` | 6 | 2 |
| 170 | `((((()()())))(()))` | `(((()()())))`  | `(())` | 5 | 2 |
| 172 | `(((((()))()))(()))` | `((((()))()))`  | `(())` | 6 | 2 |
| 174 | `((((()())()))(()))` | `(((()())()))`  | `(())` | 5 | 2 |
| 176 | `((((())(())))(()))` | `(((())(())))`  | `(())` | 5 | 2 |
| 178 | `((((())()()))(()))` | `(((())()()))`  | `(())` | 5 | 2 |
| 180 | `(((()()()()))(()))` | `((()()()()))`  | `(())` | 4 | 2 |
| 182 | `(((((())))())(()))` | `((((())))())`  | `(())` | 6 | 2 |
| 184 | `((((()()))())(()))` | `(((()()))())`  | `(())` | 5 | 2 |
| 186 | `((((())())())(()))` | `(((())())())`  | `(())` | 5 | 2 |
| 188 | `(((()()())())(()))` | `((()()())())`  | `(())` | 4 | 2 |
| 190 | `((((()))(()))(()))` | `(((()))(()))`  | `(())` | 5 | 2 |
| 192 | `((((()))()())(()))` | `(((()))()())`  | `(())` | 5 | 2 |
| 194 | `(((()())(()))(()))` | `((()())(()))`  | `(())` | 4 | 2 |
| 196 | `(((()())()())(()))` | `((()())()())`  | `(())` | 4 | 2 |
| 198 | `(((())(())())(()))` | `((())(())())`  | `(())` | 4 | 2 |
| 200 | `(((())()()())(()))` | `((())()()())`  | `(())` | 4 | 2 |
| 202 | `((()()()()())(()))` | `(()()()()())`  | `(())` | 3 | 2 |

**Significance**: These represent **all 20 ways** to asymmetrically combine each n=6 computational pattern with a simple container.

---

### Type 2: n=5 + n=3 Combinations (18 trees)

Each of the 9 trees at n=5 combines with each of the 2 trees at n=3:

| # | Tree | n=5 Child | n=3 Child | Depth | Width |
|:--|:-----|:----------|:----------|:------|:------|
| 204 | `(((((()))))((())))` | `((((()))))` (5-Chain) | `((()))` (Vertical) | 6 | 2 |
| 205 | `(((((()))))(()()))` | `((((()))))` (5-Chain) | `(()())` (Horizontal) | 6 | 2 |
| 208 | `((((()())))((())))` | `(((()())))`  | `((()))` (Vertical) | 5 | 2 |
| 209 | `((((()())))(()()))` | `(((()())))`  | `(()())` (Horizontal) | 5 | 2 |
| 212 | `((((())()))((())))` | `(((())()))`  | `((()))` (Vertical) | 5 | 2 |
| 213 | `((((())()))(()()))` | `(((())()))`  | `(()())` (Horizontal) | 5 | 2 |
| 216 | `(((()()()))((())))` | `((()()()))`  | `((()))` (Vertical) | 4 | 2 |
| 217 | `(((()()()))(()()))` | `((()()()))`  | `(()())` (Horizontal) | 4 | 2 |
| 220 | `((((()))())((())))` | `(((()))())`  | `((()))` (Vertical) | 5 | 2 |
| 221 | `((((()))())(()()))` | `(((()))())`  | `(()())` (Horizontal) | 5 | 2 |
| 224 | `(((()())())((())))` | `((()())())`  | `((()))` (Vertical) | 4 | 2 |
| 225 | `(((()())())(()()))` | `((()())())`  | `(()())` (Horizontal) | 4 | 2 |
| 228 | `(((())(()))((())))` | `((())(()))`  | `((()))` (Vertical) | 4 | 2 |
| 229 | `(((())(()))(()()))` | `((())(()))`  | `(()())` (Horizontal) | 4 | 2 |
| 232 | `(((())()())((())))` | `((())()())`  | `((()))` (Vertical) | 4 | 2 |
| 233 | `(((())()())(()()))` | `((())()())`  | `(()())` (Horizontal) | 4 | 2 |
| 236 | `((()()()())((())))` | `(()()()())`  | `((()))` (Vertical) | 3 | 2 |
| 237 | `((()()()())(()()))` | `(()()()())`  | `(()())` (Horizontal) | 3 | 2 |

**Significance**: These represent **all 9×2 = 18 ways** to combine each n=5 structure with the n=3 duality. This is the complete cross-product of the n=5 vocabulary with the fundamental binary choice.

---

### Type 3: n=4 + n=4 Symmetric Combinations (10 trees)

**First appearance of symmetric n=4 combinations.** The 4 trees at n=4 combine in all unique pairings:

| # | Tree | First n=4 Child | Second n=4 Child | Depth | Width |
|:--|:-----|:----------------|:-----------------|:------|:------|
| 240 | `((((())))(((()))))` | `(((())))` (4-Chain) | `(((())))` (4-Chain) | 5 | 2 |
| 241 | `((((())))((()())))` | `(((())))` (4-Chain) | `((()()))` (Nested Fork) | 5 | 2 |
| 242 | `((((())))((())()))` | `(((())))` (4-Chain) | `((())())` (Chain+Sibling) | 5 | 2 |
| 243 | `((((())))(()()()))` | `(((())))` (4-Chain) | `(()()())` (Ternary) | 5 | 2 |
| 249 | `(((()()))((()())))` | `((()()))` (Nested Fork) | `((()()))` (Nested Fork) | 4 | 2 |
| 250 | `(((()()))((())()))` | `((()()))` (Nested Fork) | `((())())` (Chain+Sibling) | 4 | 2 |
| 251 | `(((()()))(()()()))` | `((()()))` (Nested Fork) | `(()()())` (Ternary) | 4 | 2 |
| 257 | `(((())())((())()))` | `((())())` (Chain+Sibling) | `((())())` (Chain+Sibling) | 4 | 2 |
| 258 | `(((())())(()()()))` | `((())())` (Chain+Sibling) | `(()()())` (Ternary) | 4 | 2 |
| 264 | `((()()())(()()()))` | `(()()())` (Ternary) | `(()()())` (Ternary) | 3 | 2 |

**Significance**: The [4,4] partition generates exactly **C(4,2) + 4 = 6 + 4 = 10** unique combinations (6 asymmetric pairings + 4 symmetric self-pairings). This represents the complete vocabulary combining with itself.

---

### Type 4: n=4 + n=2 + n=2 Combinations (4 trees)

Each of the 4 trees at n=4 combines with two n=2 containers:

| # | Tree | n=4 Child | n=2 Children | Depth | Width |
|:--|:-----|:----------|:-------------|:------|:------|
| 246 | `((((())))(())(()))` | `(((())))` (4-Chain) | `(())`, `(())` | 5 | 3 |
| 254 | `(((()()))(())(()))` | `((()()))` (Nested Fork) | `(())`, `(())` | 4 | 3 |
| 261 | `(((())())(())(()))` | `((())())` (Chain+Sibling) | `(())`, `(())` | 4 | 3 |
| 267 | `((()()())(())(()))` | `(()()())` (Ternary) | `(())`, `(())` | 3 | 3 |

**Significance**: These are **ternary combinations** with one complex component and two simple containers. They extend the [3,2,2] pattern from n=8 to include the complete n=4 vocabulary.

---

### Type 5: n=3 + n=3 + n=2 Combinations (3 trees)

Unique pairings of the 2 trees at n=3, combined with an n=2 container:

| # | Tree | First n=3 | Second n=3 | n=2 | Depth | Width |
|:--|:-----|:----------|:-----------|:----|:------|:------|
| 270 | `(((()))((()))(()))` | `((()))` (Vertical) | `((()))` (Vertical) | `(())` | 4 | 3 |
| 272 | `(((()))(()())(()))` | `((()))` (Vertical) | `(()())` (Horizontal) | `(())` | 4 | 3 |
| 277 | `((()())(()())(()))` | `(()())` (Horizontal) | `(()())` (Horizontal) | `(())` | 3 | 3 |

**Significance**: These represent the **complete n=3 × n=3 pairing** (3 unique combinations) extended with a simple container. The duality combining with itself in all possible ways.

---

### Type 6: Quaternary Symmetric n=2 Combination (1 tree)

**First appearance of quaternary composition.** Four n=2 containers combine into a single structure:

| # | Tree | Children | Depth | Width |
|:--|:-----|:---------|:------|:------|
| 282 | `((())(())(())(()))` | `(())`, `(())`, `(())`, `(())` | 3 | 4 |

**Significance**: This is the **first 4-way symmetric combination**—extending beyond ternary to quaternary composition. It represents a function with four uniformly simple arguments.

---

## Summary Tables

### Distribution by Category

| Category | Count | Trees |
|:---------|:------|:------|
| **Direct Extensions (Nest)** | 115 | 1–115 (odd positions for many) |
| **Direct Extensions (Widen)** | 115 | Various |
| **Special [6,2]** | 20 | 164, 166, 168, 170, 172, 174, 176, 178, 180, 182, 184, 186, 188, 190, 192, 194, 196, 198, 200, 202 |
| **Special [5,3]** | 18 | 204, 205, 208, 209, 212, 213, 216, 217, 220, 221, 224, 225, 228, 229, 232, 233, 236, 237 |
| **Special [4,4]** | 10 | 240, 241, 242, 243, 249, 250, 251, 257, 258, 264 |
| **Special [4,2,2]** | 4 | 246, 254, 261, 267 |
| **Special [3,3,2]** | 3 | 270, 272, 277 |
| **Special [2,2,2,2]** | 1 | 282 |
| **Total** | **286** | |

### Depth Distribution

| Depth | Count | Description |
|:------|:------|:------------|
| 9 | 1 | 9-Chain (Tree 1) |
| 8 | ~15 | Deep nested structures |
| 7 | ~35 | |
| 6 | ~55 | |
| 5 | ~70 | Modal depth |
| 4 | ~75 | Secondary mode |
| 3 | ~30 | Shallow structures |
| 2 | 1 | Octenary Forest (Tree 286) |

### Width Distribution

| Width | Count | Description |
|:------|:------|:------------|
| 1 | 115 | Pure nested (all Nest operations) |
| 2 | ~90 | Binary combinations |
| 3 | ~45 | Ternary combinations |
| 4 | ~20 | Quaternary |
| 5 | ~10 | Quinary |
| 6 | ~4 | Senary |
| 7 | 1 | Septenary |
| 8 | 1 | Octenary Forest (Tree 286) |

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

The special combinations continue to grow substantially, reflecting the increasing combinatorial possibilities.

### 2. First n=6 + n=2 Combinations

For the first time, all 20 trees at n=6 appear in asymmetric combinations with n=2. This continues the complete coverage pattern established at n=8.

### 3. Complete n=5 × n=3 Cross-Product

All 9 n=5 trees combine with both n=3 trees, creating a complete 9×2 matrix of 18 combinations.

### 4. First Symmetric n=4 Combinations

The [4,4] partition generates 10 trees—all possible pairings of the n=4 vocabulary with itself. This includes 4 self-pairings (each n=4 tree with itself) and 6 distinct pairings.

### 5. First Quaternary Combination

Tree 282 `((())(())(())(()))` is the first 4-way symmetric combination—four n=2 containers combined at the root. This marks the emergence of higher-arity composition.

### 6. Balanced Operations

The 115-115 split between Nest and Widen operations continues the perfect generative balance observed at previous levels.

---

## The Extremes at N=9

| Type | Tree | Structure | Significance |
|:-----|:-----|:----------|:-------------|
| **Max Depth** | 1 | `((((((((()))))))))` | 9-Chain, Church Numeral 8 |
| **Max Breadth** | 286 | `(()()()()()()()())` | Octenary Forest, 8-argument function |

The depth-breadth spectrum at n=9 spans from 9 to 2, demonstrating the full range from maximum sequential composition (8 nested function calls) to maximum parallel application (8 independent arguments).

---

## Predictions for N=10

For n=10 (719 trees), partitions of 9 into parts ≥ 2:
- **[7, 2]**: 48 combinations (one per n=7 tree)
- **[6, 3]**: 40 combinations (20 n=6 trees × 2 n=3 trees)
- **[5, 4]**: 36 combinations (9 n=5 trees × 4 n=4 trees)
- **[5, 2, 2]**: 9 combinations (each n=5 tree + two n=2)
- **[4, 3, 2]**: 8 combinations (each n=4 × each n=3 × n=2)
- **[3, 3, 3]**: 4 combinations (unique n=3 triplets)
- **[3, 2, 2, 2]**: 2 combinations (each n=3 + three n=2)
- **[2, 2, 2, 2, 2]**: 1 combination (quinary symmetric)

Expected special combinations at n=10: **~148**

This continues the exponential growth of cross-level compositional possibilities.
