# Compositional Taxonomy of the Nine N=5 Trees

## Overview

The 9 trees at n=5 can be systematically categorized by **which n=4 parent tree they extend** and **which compositional operation is applied**. This reveals the recursive, generative nature of tree structure growth.

## The Four Compositional Operations

Before diving into the taxonomy, let's define the four fundamental operations:

| Operation | Description | Effect | Applicable To |
|:----------|:------------|:-------|:--------------|
| **Deepen** | Add a node inside the deepest nesting | Depth +1 | Linear chains, nested structures |
| **Nest** | Wrap the entire structure in `()` | Depth +1 (at root) | Any structure |
| **Juxtapose** | Add a node beside the structure | Width +1 (at root) | Any structure |
| **Widen** | Add a node beside existing siblings | Width +1 (at leaf level) | Structures with siblings |

## Complete Taxonomy

### Family 1: Descendants of `(((())))` (Maximum Chain)

**Parent**: Tree 1 from n=4 — The pure vertical pattern

#### Child 1.1: `((((()))))` — Deepened Chain
```
(((())))  →  ((((()))))
   n=4          n=5

Operation: DEEPEN
Add () inside the innermost level
```

**Visual**:
```
    ●           ●
    |           |
    ●     →     ●
    |           |
    ●           ●
                |
                ●
                |
                ●
```

**Properties**:
- Depth: 5 (maximum for n=5)
- Width: 1 (minimum)
- Pattern: V⁴ (quadruple vertical)
- Meaning: 4-level pipeline, Church numeral 4

---

#### Child 1.2: `(((()))())` — Chain with Sibling
```
(((())))  →  (((()))())
   n=4          n=5

Operation: JUXTAPOSE
Add () beside the chain at the root level
```

**Visual**:
```
    ●               ●
    |              / \
    ●     →       ●   ●
    |             |
    ●             ●
                  |
                  ●
```

**Properties**:
- Depth: 4
- Width: 2
- Pattern: H(V³, atom)
- Meaning: Binary function where one arg is a 3-level composition

---

### Family 2: Descendants of `((()()))` (Nested Fork)

**Parent**: Tree 2 from n=4 — Vertical containing horizontal

#### Child 2.1: `(((()())))` — Deepened Fork
```
((()()))  →  (((()())))
   n=4          n=5

Operation: DEEPEN
Add () inside, creating a deeper binary split
```

**Visual**:
```
    ●               ●
    |               |
    ●               ●
   / \     →        |
  ●   ●             ●
                   / \
                  ●   ●
```

**Properties**:
- Depth: 4
- Width: 2 (at maximum depth)
- Pattern: V(V(H))
- Meaning: Binary operation at depth 3

---

#### Child 2.2: `((()())())` — Nested Fork with Sibling
```
((()()))  →  ((()())())
   n=4          n=5

Operation: JUXTAPOSE
Add () beside the nested fork
```

**Visual**:
```
    ●               ●
    |              / \
    ●     →       ●   ●
   / \           / \
  ●   ●         ●   ●
```

**Properties**:
- Depth: 3
- Width: 2
- Pattern: H(V(H), atom)
- Meaning: Binary function where one arg is itself binary

---

### Family 3: Descendants of `((())())` (Asymmetric Split)

**Parent**: Tree 3 from n=4 — Horizontal with vertical and atom

#### Child 3.1: `(((())()))` — Nested Asymmetric
```
((())())  →  (((())()))
   n=4          n=5

Operation: NEST
Wrap the entire asymmetric structure in ()
```

**Visual**:
```
      ●               ●
     / \              |
    ●   ●    →        ●
    |                / \
    ●               ●   ●
                    |
                    ●
```

**Properties**:
- Depth: 4
- Width: 2
- Pattern: V(H(V, atom))
- Meaning: Unary function applied to asymmetric binary result

---

#### Child 3.2: `((())()())` — Chain with Two Siblings
```
((())())  →  ((())()())
   n=4          n=5

Operation: WIDEN
Add () beside, making it ternary
```

**Visual**:
```
      ●                 ●
     / \              / | \
    ●   ●    →       ●  ●  ●
    |                |
    ●                ●
```

**Properties**:
- Depth: 3
- Width: 3
- Pattern: H(V², atom, atom)
- Meaning: Ternary function with one composed argument

---

### Family 4: Descendants of `(()()())` (Flat Forest)

**Parent**: Tree 4 from n=4 — Pure horizontal pattern

#### Child 4.1: `((()()()))` — Nested Ternary
```
(()()())  →  ((()()()))
   n=4          n=5

Operation: NEST
Wrap the entire ternary structure in ()
```

**Visual**:
```
      ●                 ●
    / | \               |
   ●  ●  ●    →         ●
                      / | \
                     ●  ●  ●
```

**Properties**:
- Depth: 3
- Width: 3
- Pattern: V(H³)
- Meaning: Unary function applied to ternary result

---

#### Child 4.2: `(()()()())` — Quaternary Forest
```
(()()())  →  (()()()())
   n=4          n=5

Operation: WIDEN
Add () beside, making it quaternary
```

**Visual**:
```
      ●                   ●
    / | \              / | \ \
   ●  ●  ●    →       ●  ●  ●  ●
```

**Properties**:
- Depth: 2 (minimum for n=5)
- Width: 4 (maximum for n=5)
- Pattern: H⁴
- Meaning: Quaternary function (4 independent arguments)

---

### Family 5: Symmetric Composition (Special Case)

#### Tree 5.1: `((())(()))` — Double Nested Pair
```
(())  +  (())  →  ((())(()))
 n=2      n=2        n=5

Operation: COMBINE (special)
Two identical n=2 structures side-by-side
```

**Visual**:
```
  ●    ●              ●
  |    |             / \
  ●    ●    →       ●   ●
                    |   |
                    ●   ●
```

**Properties**:
- Depth: 3
- Width: 2 (at depth 2), 4 (at depth 3)
- Pattern: H(V, V)
- Meaning: Binary function with two symmetric nested arguments

**Note**: This is the only n=5 tree that doesn't directly extend an n=4 tree. Instead, it combines two n=2 trees, demonstrating that composition can skip levels.

---

## Summary Table: Complete Taxonomy

| Tree # | Structure | N=4 Parent | Operation | Family |
|:-------|:----------|:-----------|:----------|:-------|
| 1 | `((((()))))` | `(((())))` | Deepen | 1.1 |
| 2 | `(((()())))` | `((()()))` | Deepen | 2.1 |
| 3 | `(((())()))` | `((())())` | Nest | 3.1 |
| 4 | `((()()()))` | `(()()())` | Nest | 4.1 |
| 5 | `(((()))())` | `(((())))` | Juxtapose | 1.2 |
| 6 | `((()())())` | `((()()))` | Juxtapose | 2.2 |
| 7 | `((())(()))` | `(())` + `(())` | Combine | 5.1 |
| 8 | `((())()())` | `((())())` | Widen | 3.2 |
| 9 | `(()()()())` | `(()()())` | Widen | 4.2 |

## Operation Distribution

| Operation | Count | Trees |
|:----------|:------|:------|
| **Deepen** | 2 | 1, 2 |
| **Nest** | 2 | 3, 4 |
| **Juxtapose** | 2 | 5, 6 |
| **Widen** | 2 | 8, 9 |
| **Combine** | 1 | 7 |
| **Total** | **9** | |

## Parent Distribution

| N=4 Parent | Children | Operations Used |
|:-----------|:---------|:----------------|
| `(((())))` (Tree 1) | 2 | Deepen, Juxtapose |
| `((()()))` (Tree 2) | 2 | Deepen, Juxtapose |
| `((())())` (Tree 3) | 2 | Nest, Widen |
| `(()()())` (Tree 4) | 2 | Nest, Widen |
| `(())` (n=2) | 1 | Combine (special) |

## Key Insights

### 1. Balanced Growth
Each of the four n=4 trees produces exactly **2 children** at n=5. This balanced growth pattern suggests that the compositional rules are **uniform and systematic**.

### 2. Operation Complementarity
The operations used for each parent are **complementary**:
- **Vertical parents** (Trees 1, 2): Use Deepen + Juxtapose
- **Mixed parents** (Trees 3, 4): Use Nest + Widen

This reflects the fundamental duality: vertical structures can go deeper or split, while mixed structures can be wrapped or widened.

### 3. The Special Case
Tree 7 `((())(()))` is unique because it's formed by **combining two identical smaller structures** rather than extending a single parent. This demonstrates that the composition space is richer than simple parent-child relationships.

### 4. Depth-Breadth Trade-off
The operations create a spectrum:
- **Deepen**: Increases depth, maintains width
- **Nest**: Increases depth at root
- **Juxtapose**: Maintains depth, increases width at root
- **Widen**: Maintains depth, increases width at leaves

## The Recursive Generation Rule

The general rule for generating n=5 trees from n=4 trees:

```
For each n=4 tree T:
  1. If T is linear or nested: Apply DEEPEN
  2. If T is any structure: Apply NEST
  3. If T is any structure: Apply JUXTAPOSE
  4. If T has siblings: Apply WIDEN

Additionally:
  5. For symmetric smaller structures: Apply COMBINE
```

This rule generates all 9 n=5 trees without duplication.

## Implications for Higher Levels

This taxonomy reveals the **recursive fractal structure** of tree generation:

- At each level n, we have a set of "parent" trees
- Each parent can be extended using 2-4 operations
- Some operations preserve structure type (e.g., Deepen keeps chains as chains)
- Other operations transform structure type (e.g., Juxtapose turns chains into forks)
- Special symmetric cases can arise from combining smaller structures

This pattern continues indefinitely, driving the exponential growth of the A000081 sequence.
