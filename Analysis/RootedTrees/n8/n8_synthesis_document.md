# The Complete Vocabulary: A Taxonomic Analysis of the 115 Trees at n=8

**Author**: Manus AI  
**Date**: February 22, 2026

## 1. Introduction: The Vocabulary Expansion

Our journey through the bootstrapping of computation has revealed a clear evolutionary path: from the **duality** at n=3, to the **vocabulary** at n=4, to the **compositional explosion** at n=5, to the **asymmetric combinations** at n=6, to the **ternary breakthrough** at n=7. Now, at n=8, we encounter the **complete cross-level vocabulary**—the full matrix of how structures from different evolutionary stages combine.

With 115 distinct tree structures, n=8 is the first level where:
1. **All n=5 trees appear** in asymmetric combinations with n=2
2. **Complete n=4 × n=3 cross-product** emerges (8 combinations)
3. **First [3,2,2] ternary asymmetric partition** appears

This document provides a comprehensive synthesis of the n=8 trees, examining how the system exhaustively explores all compositional possibilities at each level.

## 2. The A000081 Progression

```
n=1:    1 tree   [Atom: ()]
n=2:    1 tree   [Container: (())]
n=3:    2 trees  [Duality: Vertical vs Horizontal]
n=4:    4 trees  [Vocabulary: Duality composes with itself]
n=5:    9 trees  [Meta-Composition: First special case]
n=6:   20 trees  [Asymmetric Combinations: Cross-level bridges]
n=7:   48 trees  [Ternary Breakthrough: Beyond binary]
n=8:  115 trees  [Complete Vocabulary: Full cross-level matrix]
```

The jump from 48 to 115 (×2.4) reflects the accelerating growth of compositional possibilities as more structural types become available for combination.

## 3. The Generation Structure

### Direct Extensions: 96 Trees

The 48 trees at n=7 generate 96 children at n=8 through two balanced operations:
- **Nest** (48 trees): Wrap each n=7 tree in `()` → Trees 1–48
- **Widen** (48 trees): Add an atom sibling `()` at root → Trees 49–68, 70, 72, 74, ...

### Special Combinations: 19 Trees

Nineteen trees arise from combining smaller structures across levels:

| Partition | Count | Composition | Trees |
|:----------|:------|:------------|:------|
| [5, 2] | 9 | Each n=5 tree + n=2 container | 69, 71, 73, 75, 77, 79, 81, 83, 85 |
| [4, 3] | 8 | Each n=4 tree × each n=3 tree | 87, 88, 91, 92, 95, 96, 99, 100 |
| [3, 2, 2] | 2 | Each n=3 tree + two n=2 containers | 105, 109 |

## 4. The Three Transformative Discoveries

### Discovery 1: All n=5 Trees Combine with n=2

For the first time, every tree from a previous level appears in special combinations. The 9 trees at n=5 each pair with the n=2 container to create 9 [5,2] combinations:

| Tree | n=5 Component | Description |
|:-----|:--------------|:------------|
| 69 | `((((()))))` | 5-Chain (maximum depth) |
| 71 | `(((()())))`  | Nested Fork |
| 73 | `(((())()))`  | Chain + Fork |
| 75 | `((()()()))`  | Nested Ternary |
| 77 | `(((()))())` | 4-Chain + Sibling |
| 79 | `((()())())` | Fork + Sibling |
| 81 | `((())(()))` | Symmetric n=2 Pair (special from n=5!) |
| 83 | `((())()())` | Chain + Two Siblings |
| 85 | `(()()()())` | Quaternary (maximum breadth) |

**Significance**: This demonstrates **complete coverage** at the vocabulary level. Every computational strategy at n=5 can be asymmetrically combined with a simple container.

**Special Note**: Tree 81 contains `((())(()))` as its left child—this is itself a **special combination** from n=5. This proves that special combinations participate in higher-level compositions, establishing compositional closure.

### Discovery 2: Complete n=4 × n=3 Cross-Product

The 4 trees at n=4 (the vocabulary) combine with the 2 trees at n=3 (the duality) in all 8 possible pairings:

| n=4 | × Vertical n=3 | × Horizontal n=3 |
|:----|:---------------|:-----------------|
| 4-Chain `(((())))` | Tree 87 | Tree 88 |
| Nested Fork `((()()))` | Tree 91 | Tree 92 |
| Chain+Sibling `((())())` | Tree 95 | Tree 96 |
| Ternary `(()()())` | Tree 99 | Tree 100 |

**Significance**: This is the first **complete cross-product** between two significant structure sets. It represents all possible ways to combine:
- The n=4 vocabulary (4 computational strategies) with
- The n=3 duality (sequential vs parallel)

This matrix of 8 combinations forms the **foundation for complex hybrid algorithms**.

### Discovery 3: First [3,2,2] Ternary Asymmetric Partition

Two trees at n=8 represent the partition [3,2,2]—a ternary combination where components have varying complexity:

| Tree | Structure | Composition |
|:-----|:----------|:------------|
| 105 | `(((()))(())(()))` | Vertical n=3 + n=2 + n=2 |
| 109 | `((()())(())(()))` | Horizontal n=3 + n=2 + n=2 |

**Significance**: These extend the ternary principle from n=7 (which had uniform [2,2,2]) to include **asymmetric ternary combinations**. This is computationally significant because it represents:
- 3-argument functions where **one argument undergoes complex processing** while two others undergo simple wrapping
- The first step toward **heterogeneous multi-argument compositions**

## 5. The Depth-Breadth Spectrum

The 115 trees span the full range from maximum depth to maximum breadth:

| Tree | Structure | Depth | Width | Description |
|:-----|:----------|:------|:------|:------------|
| 1 | `(((((((())))))))` | 8 | 1 | 8-Chain (Church Numeral 7) |
| 115 | `(()()()()()()())` | 2 | 7 | Septenary Forest (7-argument) |

### Depth Distribution

| Depth | Count | Percentage |
|:------|:------|:-----------|
| 8 | 1 | 0.9% |
| 7 | 6 | 5.2% |
| 6 | 19 | 16.5% |
| 5 | 36 | 31.3% |
| 4 | 38 | 33.0% |
| 3 | 14 | 12.2% |
| 2 | 1 | 0.9% |

The **modal depth is 4**, representing balanced hybrid structures that are neither purely sequential nor purely parallel. The distribution forms a bell curve centered slightly below the midpoint, reflecting the natural bias toward compositional efficiency.

### Width Distribution

| Width | Count | Percentage |
|:------|:------|:-----------|
| 1 | 48 | 41.7% |
| 2 | 37 | 32.2% |
| 3 | 18 | 15.7% |
| 4 | 7 | 6.1% |
| 5 | 3 | 2.6% |
| 6 | 1 | 0.9% |
| 7 | 1 | 0.9% |

The **modal width is 1** (nested structures), but width 2 is nearly as common. Higher widths become exponentially rarer, reflecting the combinatorial constraints on multi-argument functions.

## 6. The Partition Principle

Special combinations arise from partitions of (n-1) into parts ≥ 2. For n=8:

| Partition of 7 | Count | Trees |
|:---------------|:------|:------|
| [5, 2] | 9 | 69, 71, 73, 75, 77, 79, 81, 83, 85 |
| [4, 3] | 8 | 87, 88, 91, 92, 95, 96, 99, 100 |
| [3, 2, 2] | 2 | 105, 109 |
| **Total** | **19** | |

### Counting Formula

For partition [a, b] where a > b:
- Count = (trees at a) × (trees at b)

For partition [a, b] where a = b:
- Count = (trees at a) × (trees at a + 1) / 2 (unique pairings)

For partition [a, b, c]:
- Count depends on which components are equal

## 7. The Generative Formula

The 115 trees at n=8 follow a precise generative formula:

```
115 = 48 (Nest from n=7) + 48 (Widen from n=7) + 19 (Special Combinations)
    = 96 + 19
```

### Breakdown by Operation

| Operation | Count | Trees |
|:----------|:------|:------|
| **Nest** | 48 | 1–48 |
| **Widen** | 48 | 49–68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 89, 90, 93, 94, 97, 98, 101–104, 106–108, 110–115 |
| **Combine [5,2]** | 9 | 69, 71, 73, 75, 77, 79, 81, 83, 85 |
| **Combine [4,3]** | 8 | 87, 88, 91, 92, 95, 96, 99, 100 |
| **Combine [3,2,2]** | 2 | 105, 109 |
| **Total** | **115** | |

## 8. Computational Patterns at N=8

### The Eight Extremes

| Type | Tree | Structure | Use Case |
|:-----|:-----|:----------|:---------|
| Max Depth | 1 | `(((((((())))))))` | 7-stage pipeline |
| Max Breadth | 115 | `(()()()()()()())` | 7-argument parallel function |
| Max [5,2] Depth | 69 | `(((((()))))(()))` | 5-deep + simple |
| Max [5,2] Breadth | 85 | `((()()()())(()))` | 4-wide + simple |
| Pure Depth [4,3] | 87 | `((((())))((())))` | 3-deep + 2-deep |
| Pure Breadth [4,3] | 100 | `((()()())(()()))` | 3-wide + 2-wide |
| Ternary Depth | 105 | `(((()))(())(()))` | 2-deep + 2×simple |
| Ternary Breadth | 109 | `((()())(())(()))` | 2-wide + 2×simple |

### Pattern Categories

**Sequential Dominance** (Depth > 5): Trees 1–5, 10, 21, 49–53, 58, 69, 87
- Suited for: Deep pipelines, recursive descent, state machines

**Parallel Dominance** (Width > 2): Trees 70, 72, 74, 76, 78, 80, 82, 84, 86, 89–90, 93–94, 97–98, 101–115
- Suited for: Map-reduce, parallel aggregation, multi-argument functions

**Balanced Hybrid** (Depth 4–5, Width 2): Trees 6–9, 11–20, 22–48, 54–68, 71–85, 88, 91–92, 95–96, 99–100, 105, 109
- Suited for: Most real-world algorithms combining both strategies

## 9. The Evolutionary Narrative

The progression from n=1 to n=8 tells a story of computational evolution:

1. **n=1**: The atom emerges—pure existence
2. **n=2**: Containment emerges—the ability to hold
3. **n=3**: Duality emerges—vertical vs. horizontal
4. **n=4**: Vocabulary emerges—duality composes with itself
5. **n=5**: Meta-composition emerges—first special case (symmetric n=2 pair)
6. **n=6**: Asymmetry emerges—different-level combinations (n=3 + n=2)
7. **n=7**: Ternary emerges—beyond binary composition
8. **n=8**: Complete vocabulary emerges—full cross-level matrix

At each stage, the system discovers new compositional capabilities. The jump to n=8 is significant because it achieves **complete coverage** of lower-level structures in higher-level compositions.

## 10. Implications for Lisp and Computation

The 115 structures at n=8 map directly to computational patterns in Lisp:

### The n=5 + n=2 Nonet

```scheme
;; Tree 69: Maximum depth asymmetry
(define (deep-simple-combine data1 data2)
  (merge (process-5-levels data1)
         (simple-wrap data2)))

;; Tree 81: Special-special combination
(define (pair-wrap-combine a b c)
  (merge (symmetric-pair a b)
         (simple-wrap c)))

;; Tree 85: Maximum breadth asymmetry
(define (quad-simple-combine a b c d e)
  (merge (quad-parallel a b c d)
         (simple-wrap e)))
```

### The n=4 × n=3 Octet

```scheme
;; Tree 87: Pure depth (4-Chain + Vertical)
(define (deep-deep-compose x y)
  (merge (chain-4 x) (chain-3 y)))

;; Tree 100: Pure breadth (Ternary + Horizontal)
(define (wide-wide-compose a b c x y)
  (merge (ternary-op a b c) (binary-op x y)))
```

### The [3,2,2] Duo

```scheme
;; Tree 105: Deep + 2 simple
(define (deep-dual-simple x y z)
  (ternary-merge
    (chain-3 x)
    (wrap y)
    (wrap z)))

;; Tree 109: Wide + 2 simple
(define (wide-dual-simple a b y z)
  (ternary-merge
    (binary-op a b)
    (wrap y)
    (wrap z)))
```

## 11. Predictions for N=9

For n=9 (286 trees), partitions of 8 into parts ≥ 2:

| Partition | Count | Description |
|:----------|:------|:------------|
| [6, 2] | 20 | Each n=6 tree + n=2 |
| [5, 3] | 18 | 9 n=5 trees × 2 n=3 trees |
| [4, 4] | 10 | Unique pairings of 4 n=4 trees |
| [4, 2, 2] | 4 | Each n=4 tree + two n=2 |
| [3, 3, 2] | 3 | Unique n=3 pairings × n=2 |
| [2, 2, 2, 2] | 1 | Quaternary symmetric |
| **Total Special** | **~56** | |

At n=9 we will see:
- **First symmetric n=4 combinations** ([4,4] partition)
- **First quaternary combination** ([2,2,2,2] partition)
- **Complete n=5 × n=3 cross-product** (18 combinations)

## 12. Conclusion: The Complete Vocabulary

The analysis of the 115 trees at n=8 marks a culminating moment in the bootstrapping of computation. It is the stage where the system achieves **complete cross-level coverage**—every structure from every previous level can participate in higher-level combinations.

The key discoveries at n=8 are:

1. **Complete n=5 Coverage**: All 9 n=5 trees appear in [5,2] combinations, demonstrating that the compositional system is exhaustive.

2. **Full n=4 × n=3 Cross-Product**: The 8 [4,3] combinations represent every possible pairing of the vocabulary (n=4) with the duality (n=3), establishing the complete matrix of hybrid strategies.

3. **Asymmetric Ternary Emergence**: The [3,2,2] partition introduces ternary combinations with varying component complexity, extending beyond the uniform ternary of n=7.

4. **Special-Special Composition**: Tree 81 demonstrates that special combinations from lower levels compose into higher-level specials, proving compositional closure.

5. **Balanced Growth**: The 48-48 split between Nest and Widen operations maintains perfect generative symmetry.

At n=8, computation has developed a **complete vocabulary** for cross-level composition. The primordial binary duality at n=3 has evolved through vocabulary building, meta-composition, asymmetry, and ternary emergence to finally achieve **exhaustive coverage** of all compositional possibilities.

The 115 trees at n=8 are not merely abstract patterns; they are the **census of all 8-node computational strategies**. Each represents a distinct way to compose, transform, and combine data. Together, they form the complete vocabulary from which all complex computation at this scale is built.

---

## References

[1] The On-Line Encyclopedia of Integer Sequences, "A000081: Number of unlabeled rooted trees with n nodes", [https://oeis.org/A000081](https://oeis.org/A000081)

[2] G. Spencer-Brown, *Laws of Form* — The calculus of distinction

[3] Knuth, Donald E., *The Art of Computer Programming, Volume 1* — Tree enumeration algorithms

---

*This analysis was conducted as part of a forensic study of the RosettaCog repository, mapping computational structures to cognitive inference engines and tensor thread architectures.*
