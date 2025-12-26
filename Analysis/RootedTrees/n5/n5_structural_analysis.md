# The Nine Trees at n=5: Complete Compositional Analysis

## Overview

At n=5, the A000081 sequence gives us exactly **9** distinct unlabeled rooted trees. This represents a significant jump from n=4's 4 trees, and marks the beginning of the **exponential explosion** of structural complexity.

The key insight is that each n=5 tree is formed by **composing one of the four n=4 patterns with a single additional node**. This document provides a complete taxonomy of how these 9 trees arise from the recursive composition of the n=4 vocabulary.

## The Nine Structures

### Tree 1: `((((()))))` - The Maximum Chain

```
Visual:
    ●
    |
    ●
    |
    ●
    |
    ●
    |
    ●

Depth: 5 (maximum)
Width: 1 (minimum)
```

**Compositional Origin**: Deepen `(((())))` (n=4 Tree 1)
```
(((())))  +  () inside  →  ((((()))))
  n=4                        n=5
```

**Pattern**: V ∘ V ∘ V ∘ V (quadruple vertical composition)  
**Meaning**: 4-level pipeline, Church numeral **4**

---

### Tree 2: `(((()())))` - Deep Fork

```
Visual:
    ●
    |
    ●
    |
    ●
   / \
  ●   ●

Depth: 4
Width: 2 (at depth 4)
```

**Compositional Origin**: Deepen `((()))` (n=3) inside `((()))`
```
((()))  →  ((()()))  →  (((()())))
  n=3        n=4           n=5
```

**Pattern**: V(V(H)) - vertical containing vertical containing horizontal  
**Meaning**: Binary operation at maximum depth

---

### Tree 3: `(((())()))` - Nested Asymmetric

```
Visual:
    ●
    |
    ●
   / \
  ●   ●
  |
  ●

Depth: 4
Width: 2 (at depth 3)
```

**Compositional Origin**: Nest `((())())` (n=4 Tree 3) inside `()`
```
((())())  →  (((())()))
   n=4          n=5
```

**Pattern**: V(H(V, atom)) - vertical containing horizontal-with-vertical  
**Meaning**: Asymmetric split at depth 2

---

### Tree 4: `((()()()))` - Nested Triple Fork

```
Visual:
    ●
    |
    ●
  / | \
 ●  ●  ●

Depth: 3
Width: 3 (at depth 3)
```

**Compositional Origin**: Nest `(()()())` (n=4 Tree 4) inside `()`
```
(()()())  →  ((()()()))
   n=4          n=5
```

**Pattern**: V(H ∘ H ∘ H) - vertical containing triple horizontal  
**Meaning**: Ternary operation nested one level deep

---

### Tree 5: `(((()))())` - Deep Chain with Sibling

```
Visual:
      ●
     / \
    ●   ●
    |
    ●
    |
    ●

Depth: 4
Width: 2 (at depth 2)
```

**Compositional Origin**: Juxtapose `(((())))` (n=4 Tree 1) with `()`
```
(((())))  +  () beside  →  (((()))())
   n=4                        n=5
```

**Pattern**: H(V ∘ V ∘ V, atom) - horizontal with deep vertical and atom  
**Meaning**: Binary function where one arg is a 3-level pipeline

---

### Tree 6: `((()())())` - Nested Fork with Sibling

```
Visual:
      ●
     / \
    ●   ●
   / \
  ●   ●

Depth: 3
Width: 2 (at multiple levels)
```

**Compositional Origin**: Juxtapose `((()()))` (n=4 Tree 2) with `()`
```
((()()))  +  () beside  →  ((()())())
   n=4                        n=5
```

**Pattern**: H(V(H), atom) - horizontal with nested-fork and atom  
**Meaning**: Binary function where one arg is a binary result

---

### Tree 7: `((())(()))` - Symmetric Double Fork

```
Visual:
      ●
     / \
    ●   ●
   / \ / \
  ●  ●●  ●

Depth: 3
Width: 2 (at depth 2), 4 (at depth 3)
```

**Compositional Origin**: Juxtapose two `(())` (n=2) structures
```
(())  +  (())  →  ((())(()))
 n=2      n=2        n=5
```

**Pattern**: H(V, V) - horizontal with two vertical children  
**Meaning**: Binary function with two nested arguments (symmetric)

---

### Tree 8: `((())()())` - Chain with Double Sibling

```
Visual:
      ●
    / | \
   ●  ●  ●
   |
   ●

Depth: 3
Width: 3 (at depth 2)
```

**Compositional Origin**: Widen `((())())` (n=4 Tree 3) by adding `()`
```
((())())  +  () beside  →  ((())()())
   n=4                        n=5
```

**Pattern**: H(V ∘ V, atom, atom) - horizontal with one vertical and two atoms  
**Meaning**: Ternary function where one arg is composed

---

### Tree 9: `(()()()())` - The Flat Forest

```
Visual:
       ●
    /  |  \  \
   ●   ●   ●   ●

Depth: 2 (minimum)
Width: 4 (maximum)
```

**Compositional Origin**: Widen `(()()())` (n=4 Tree 4) by adding `()`
```
(()()())  +  () beside  →  (()()()())
   n=4                        n=5
```

**Pattern**: H ∘ H ∘ H ∘ H (quadruple horizontal composition)  
**Meaning**: Quaternary function (4 independent arguments)

---

## Compositional Taxonomy

We can categorize the 9 trees by which n=4 pattern they extend:

### Category A: Extensions of Tree 1 `(((())))` (Maximum Chain)

| Tree | Structure | Operation | Pattern |
|:-----|:----------|:----------|:--------|
| 1 | `((((()))))` | **Deepen** | Add `()` inside the chain |
| 5 | `(((()))())` | **Juxtapose** | Add `()` beside the chain |

**Count**: 2 trees

---

### Category B: Extensions of Tree 2 `((()()))` (Nested Fork)

| Tree | Structure | Operation | Pattern |
|:-----|:----------|:----------|:--------|
| 2 | `(((()())))` | **Deepen** | Add `()` inside the fork |
| 6 | `((()())())` | **Juxtapose** | Add `()` beside the structure |

**Count**: 2 trees

---

### Category C: Extensions of Tree 3 `((())())` (Asymmetric Split)

| Tree | Structure | Operation | Pattern |
|:-----|:----------|:----------|:--------|
| 3 | `(((())()))` | **Nest** | Wrap entire structure in `()` |
| 8 | `((())()())` | **Widen** | Add `()` beside (make ternary) |

**Count**: 2 trees

---

### Category D: Extensions of Tree 4 `(()()())` (Flat Forest)

| Tree | Structure | Operation | Pattern |
|:-----|:----------|:----------|:--------|
| 4 | `((()()()))` | **Nest** | Wrap entire structure in `()` |
| 9 | `(()()()())` | **Widen** | Add `()` beside (make quaternary) |

**Count**: 2 trees

---

### Category E: Symmetric Composition (Special Case)

| Tree | Structure | Operation | Pattern |
|:-----|:----------|:----------|:--------|
| 7 | `((())(()))` | **Combine** | Two `(())` structures side-by-side |

**Count**: 1 tree

This is a **special symmetric case** where we combine two identical n=2 structures.

---

## The Composition Matrix

| N=4 Parent | Deepen | Nest | Juxtapose | Widen | Total Children |
|:-----------|:-------|:-----|:----------|:------|:---------------|
| `(((())))` (Tree 1) | Tree 1 | — | Tree 5 | — | 2 |
| `((()()))` (Tree 2) | Tree 2 | — | Tree 6 | — | 2 |
| `((())())` (Tree 3) | — | Tree 3 | — | Tree 8 | 2 |
| `(()()())` (Tree 4) | — | Tree 4 | — | Tree 9 | 2 |
| **Special** | — | — | — | — | **1** (Tree 7) |
| **Total** | 2 | 2 | 2 | 2 | **9** |

## The Four Composition Operations

### 1. Deepen (Add inside)
- Applicable to: Linear chains and nested structures
- Effect: Increases depth by 1
- Trees: 1, 2

### 2. Nest (Wrap in container)
- Applicable to: Any structure
- Effect: Adds one level of wrapping
- Trees: 3, 4

### 3. Juxtapose (Add beside)
- Applicable to: Any structure
- Effect: Creates binary sibling relationship
- Trees: 5, 6

### 4. Widen (Extend breadth)
- Applicable to: Structures with horizontal components
- Effect: Increases width by 1
- Trees: 8, 9

## Depth vs. Breadth Distribution

```
Depth 5: 1 tree   ((((()))))
Depth 4: 3 trees  (((()())))  (((())()))  (((()))())
Depth 3: 4 trees  ((()()()))  ((()())())  ((())(()))  ((())()())
Depth 2: 1 tree   (()()()())
```

The distribution forms a bell curve, with most trees at intermediate depths (3-4).

## Width Distribution

```
Width 1: 1 tree   ((((()))))
Width 2: 5 trees  (((()())))  (((())()))  (((()))())  ((()())())  ((())(()))
Width 3: 2 trees  ((()()()))  ((())()())
Width 4: 1 tree   (()()()())
```

Most trees have width 2, representing binary operations or splits.

## Summary: The Compositional Explosion

The jump from 4 trees (n=4) to 9 trees (n=5) demonstrates the **compositional explosion**:

- Each of the 4 n=4 trees can be extended in **2 different ways** (on average)
- This gives us 4 × 2 = 8 trees
- Plus 1 special symmetric case (Tree 7)
- **Total: 9 trees**

The pattern continues:
- n=4: 4 trees
- n=5: 9 trees (2.25× growth)
- n=6: 20 trees (2.22× growth)
- n=7: 48 trees (2.4× growth)

The growth rate stabilizes around **2.2-2.5×** per level, leading to exponential explosion.

## The Recursive Structure

Each n=5 tree is a **composition of smaller trees**:

```
n=5 tree = root + forest(n=4)

Where forest(n=4) can be:
- One n=4 tree
- One n=3 tree + one n=1 tree
- One n=2 tree + one n=2 tree
- One n=2 tree + two n=1 trees
- Four n=1 trees
```

This recursive structure is what the bag chain algorithm exploits, and it's the fundamental principle underlying the A000081 sequence.
