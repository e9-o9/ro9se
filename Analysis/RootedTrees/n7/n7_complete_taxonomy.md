# Complete Compositional Taxonomy of N=7 Trees

## Overview

The 48 trees at n=7 can be categorized into two main groups:
1. **Direct Extensions** (40 trees): Formed by applying Nest or Widen to an n=6 parent
2. **Special Combinations** (8 trees): Formed by combining smaller structures from different levels

## Category A: Direct Extensions from N=6 Parents (40 Trees)

### Generation Rule

At n=7, each n=6 parent tree produces descendants through exactly **one or two** operations:
- **Nest**: Wrap the parent in `()` — always available
- **Widen**: Add an atom sibling `()` at root level — available when the parent already has multiple children

### Family 1: Descendants of n=6 Tree 1 `(((((()))))`  (6-Chain)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 1 | `((((((())))))))` | **Nest** | V⁶ | 7 | 1 |
| 21 | `(((((()))))())` | **Widen** | H(V⁵, atom) | 6 | 2 |

*Note: Tree 1 is the maximum-depth 7-Chain (Church Numeral 6)*

---

### Family 2: Descendants of n=6 Tree 2 `((((()()))))` (Nested Deep Fork)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 2 | `(((((()())))))` | **Nest** | V(V(V(V(H)))) | 6 | 1 |

---

### Family 3: Descendants of n=6 Tree 3 `((((())())))` (Double-Nested Asymmetric)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 3 | `(((((())()))))` | **Nest** | V(V(V(H(V, atom)))) | 6 | 1 |

---

### Family 4: Descendants of n=6 Tree 4 `(((()()())))` (Double-Nested Ternary)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 4 | `((((()()()))))` | **Nest** | V(V(V(H³))) | 5 | 1 |

---

### Family 5: Descendants of n=6 Tree 5 `((((()))()))` (Nested Chain-with-Sibling)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 5 | `(((((()))())))` | **Nest** | V(V(H(V³, atom))) | 6 | 1 |

---

### Family 6: Descendants of n=6 Tree 6 `(((()())()))` (Double-Nested Fork-with-Sibling)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 6 | `((((()())())))` | **Nest** | V(V(H(V(H), atom))) | 5 | 1 |

---

### Family 7: Descendants of n=6 Tree 7 `(((())(())))` (Nested Symmetric Double Fork)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 7 | `((((())(()))))` | **Nest** | V(V(H(V, V))) | 5 | 1 |

---

### Family 8: Descendants of n=6 Tree 8 `(((())()()))` (Nested Chain-with-Two-Siblings)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 8 | `((((())()())))` | **Nest** | V(V(H(V², atom, atom))) | 5 | 1 |

---

### Family 9: Descendants of n=6 Tree 9 `((()()()()))` (Nested Quaternary)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 9 | `(((()()()())))` | **Nest** | V(V(H⁴)) | 4 | 1 |

---

### Family 10: Descendants of n=6 Tree 10 `((((())))())` (5-Chain with Sibling)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 10 | `(((((())))()))` | **Nest** | V(H(V⁴, atom)) | 6 | 1 |
| 31 | `((((())))()())` | **Widen** | H(V⁴, atom, atom) | 5 | 3 |
| 33 | `(((()()))()())` | **Widen** | H(V(V(H)), atom, atom) | 4 | 3 |
| 35 | `(((())())()())` | **Widen** | H(V(H(V, atom)), atom, atom) | 4 | 3 |
| 37 | `((()()())()())` | **Widen** | H(V(H³), atom, atom) | 3 | 3 |

---

### Family 11: Descendants of n=6 Tree 11 `(((()()))())` (Deep Fork with Sibling)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 11 | `((((()()))()))` | **Nest** | V(H(V(V(H)), atom)) | 5 | 1 |

---

### Family 12: Descendants of n=6 Tree 12 `(((())())())` (Nested Asymmetric with Sibling)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 12 | `((((())())()))` | **Nest** | V(H(V(H(V, atom)), atom)) | 5 | 1 |

---

### Family 13: Descendants of n=6 Tree 13 `((()()())())` (Nested Ternary with Sibling)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 13 | `(((()()())()))` | **Nest** | V(H(V(H³), atom)) | 4 | 1 |

---

### Family 14: Descendants of n=6 Tree 14 `(((()))(()))` (Vertical n=3 + n=2)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 14 | `((((()))(())))` | **Nest** | V(H(V², V)) | 5 | 1 |
| 40 | `(((()))(())())` | **Widen** | H(V², V, atom) | 4 | 3 |
| 43 | `((()())(())())` | **Widen** | H(H, V, atom) | 3 | 3 |

---

### Family 15: Descendants of n=6 Tree 15 `(((()))()())` (3-Chain with Two Siblings)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 15 | `((((()))()()))` | **Nest** | V(H(V², atom, atom)) | 5 | 1 |
| 41 | `(((()))()()())` | **Widen** | H(V², atom, atom, atom) | 4 | 4 |
| 44 | `((()())()()())` | **Widen** | H(H, atom, atom, atom) | 3 | 4 |

---

### Family 16: Descendants of n=6 Tree 16 `((()())(()))` (Horizontal n=3 + n=2)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 16 | `(((()())(())))` | **Nest** | V(H(H, V)) | 4 | 1 |

---

### Family 17: Descendants of n=6 Tree 17 `((()())()())` (Nested Fork with Two Siblings)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 17 | `(((()())()()))` | **Nest** | V(H(V(H), atom, atom)) | 4 | 1 |

---

### Family 18: Descendants of n=6 Tree 18 `((())(())())` (Symmetric Double Fork with Sibling)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 18 | `(((())(())()))` | **Nest** | V(H(V, V, atom)) | 4 | 1 |
| 46 | `((())(())()())` | **Widen** | H(V, V, atom, atom) | 3 | 4 |

---

### Family 19: Descendants of n=6 Tree 19 `((())()()())` (2-Chain with Three Siblings)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 19 | `(((())()()()))` | **Nest** | V(H(V², atom, atom, atom)) | 4 | 1 |
| 47 | `((())()()()())` | **Widen** | H(V, atom, atom, atom, atom) | 3 | 5 |

---

### Family 20: Descendants of n=6 Tree 20 `(()()()()())` (Quinary Forest)

| # | Tree | Operation | Pattern | Depth | Width |
|:--|:-----|:----------|:--------|:------|:------|
| 20 | `((()()()()()))` | **Nest** | V(H⁵) | 3 | 1 |
| 48 | `(()()()()()())` | **Widen** | H⁶ | 2 | 6 |

*Note: Tree 48 is the maximum-breadth Senary Forest (6-argument function)*

---

## Category B: Special Combinations (8 Trees)

These trees are not direct descendants of any single n=6 parent. They are formed by combining structures from different levels.

### Type 1: n=4 + n=2 Asymmetric Combinations (4 trees)

These combine an n=4 structure with an n=2 structure at the root level.

| # | Tree | n=4 Child | n=2 Child | Pattern | Depth | Width |
|:--|:-----|:----------|:----------|:--------|:------|:------|
| 30 | `((((())))(()))` | `(((())))` (4-Chain) | `(())` | H(V³, V) | 5 | 2 |
| 32 | `(((()()))(()))` | `((()()))` (Nested Fork) | `(())` | H(V(V(H)), V) | 4 | 2 |
| 34 | `(((())())(()))` | `((())())` (2-Chain + Sibling) | `(())` | H(H(V², atom), V) | 4 | 2 |
| 36 | `((()()())(()))` | `(()()())` (Ternary) | `(())` | H(H³, V) | 3 | 2 |

**Significance**: These represent the **complete set** of ways to pair each n=4 tree with n=2.

---

### Type 2: n=3 + n=3 Symmetric Combinations (3 trees)

These combine two n=3 structures at the root level.

| # | Tree | Left n=3 | Right n=3 | Pattern | Depth | Width |
|:--|:-----|:---------|:----------|:--------|:------|:------|
| 38 | `(((()))((())))` | `((()))` (Vertical) | `((()))` (Vertical) | H(V², V²) | 4 | 2 |
| 39 | `(((()))(()()))` | `((()))` (Vertical) | `(()())` (Horizontal) | H(V², H) | 4 | 2 |
| 42 | `((()())(()()))` | `(()())` (Horizontal) | `(()())` (Horizontal) | H(H, H) | 3 | 2 |

**Significance**: These represent all unique pairings of the two n=3 trees (vertical and horizontal):
- Tree 38: V + V (pure depth composition)
- Tree 39: V + H (hybrid: vertical paired with horizontal)
- Tree 42: H + H (pure breadth composition)

---

### Type 3: Triple n=2 Symmetric Combination (1 tree)

| # | Tree | Composition | Pattern | Depth | Width |
|:--|:-----|:------------|:--------|:------|:------|
| 45 | `((())(())(()))` | Three n=2 containers | H(V, V, V) | 3 | 3 |

**Significance**: This is the first **ternary symmetric combination**—three identical n=2 structures combined at the root. It represents a ternary function where all three arguments have the same computational depth: `f(g(x), h(y), i(z))`.

---

## Summary Tables

### Distribution by Operation

| Operation | Count | Trees |
|:----------|:------|:------|
| **Nest** | 20 | 1–20 |
| **Widen** | 20 | 21–29, 31, 33, 35, 37, 40, 41, 43, 44, 46, 47, 48 |
| **Combine** | 8 | 30, 32, 34, 36, 38, 39, 42, 45 |
| **Total** | **48** | |

### Distribution by N=6 Parent

| N=6 Parent | Children Count | Trees |
|:-----------|:---------------|:------|
| Tree 1 `(((((())))))` | 2 | 1, 21 |
| Tree 2 `((((()()))))` | 1 | 2 |
| Tree 3 `((((())())))` | 1 | 3 |
| Tree 4 `(((()()())))` | 1 | 4 |
| Tree 5 `((((()))()))` | 1 | 5 |
| Tree 6 `(((()())()))` | 1 | 6 |
| Tree 7 `(((())(())))` | 1 | 7 |
| Tree 8 `(((())()()))` | 1 | 8 |
| Tree 9 `((()()()()))` | 1 | 9 |
| Tree 10 `((((())))())` | 5 | 10, 31, 33, 35, 37 |
| Tree 11 `(((()()))())` | 1 | 11 |
| Tree 12 `(((())())())` | 1 | 12 |
| Tree 13 `((()()())())` | 1 | 13 |
| Tree 14 `(((()))(()))` | 3 | 14, 40, 43 |
| Tree 15 `(((()))()())` | 3 | 15, 41, 44 |
| Tree 16 `((()())(()))` | 1 | 16 |
| Tree 17 `((()())()())` | 1 | 17 |
| Tree 18 `((())(())())` | 2 | 18, 46 |
| Tree 19 `((())()()())` | 2 | 19, 47 |
| Tree 20 `(()()()()())` | 2 | 20, 48 |
| **Special** | 8 | 30, 32, 34, 36, 38, 39, 42, 45 |
| **Total** | **48** | |

### Depth Distribution

| Depth | Count | Trees |
|:------|:------|:------|
| 7 | 1 | 1 |
| 6 | 5 | 2, 3, 5, 10, 21 |
| 5 | 13 | 4, 6, 7, 8, 11, 12, 14, 15, 22, 23, 25, 30, 31 |
| 4 | 18 | 9, 13, 16, 17, 18, 19, 24, 26, 27, 28, 32, 33, 34, 35, 38, 39, 40, 41 |
| 3 | 10 | 20, 29, 36, 37, 42, 43, 44, 45, 46, 47 |
| 2 | 1 | 48 |

### Width Distribution

| Width | Count | Trees |
|:------|:------|:------|
| 1 | 20 | 1–20 |
| 2 | 16 | 21–30, 32, 34, 36, 38, 39, 42 |
| 3 | 7 | 31, 33, 35, 37, 40, 43, 45 |
| 4 | 3 | 41, 44, 46 |
| 5 | 1 | 47 |
| 6 | 1 | 48 |

## Key Observations

### 1. Explosion of Special Combinations

At n=6, we had **2** special combinations. At n=7, we have **8**—a fourfold increase. This reflects the growing number of ways to partition n into meaningful substructures.

### 2. First Symmetric n=3 Combinations

For the first time, we see symmetric combinations of n=3 structures:
- `(((()))((())))` — Two vertical n=3 trees
- `((()())(()()))` — Two horizontal n=3 trees
- Plus the hybrid: `(((()))(()()))` — Vertical + Horizontal

### 3. First Triple Combination

Tree 45 `((())(())(()))` is the first **ternary symmetric combination**, showing that as n grows, combinations beyond binary pairs become possible.

### 4. The n=4 + n=2 Quartet

All 4 of the n=4 trees appear in combinations with n=2, creating a complete set of asymmetric compositions.

### 5. Balanced Growth Continues

The operation distribution remains perfectly balanced: 20 Nest operations and 20 Widen operations, maintaining the generative symmetry observed at n=6.

## The Compositional Formula

For n=7, the generation formula is:

```
48 trees = 40 direct extensions + 8 special combinations

Special combinations breakdown:
- 4 trees: n=4 + n=2 (asymmetric)
- 3 trees: n=3 + n=3 (symmetric pairs)
- 1 tree:  n=2 + n=2 + n=2 (ternary)
```

This reveals the exponentially growing combinatorial richness as n increases.

## The Extremes

| Type | Tree | Structure | Significance |
|:-----|:-----|:----------|:-------------|
| **Max Depth** | 1 | `((((((()))))))` | 7-Chain, Church Numeral 6 |
| **Max Breadth** | 48 | `(()()()()()())` | Senary Forest, 6-argument function |

The depth-breadth spectrum now spans from 7 to 2, demonstrating the full range of computational strategies from maximum sequential composition to maximum parallel application.
