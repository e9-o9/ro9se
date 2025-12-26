# Visual Taxonomy: The Recursive Composition from N=3 to N=5

## The Complete Genealogy Tree

This document presents a visual representation of how the 9 trees at n=5 descend from the 4 trees at n=4, which in turn descend from the 2 trees at n=3.

## Level-by-Level Evolution

```
N=3 (2 trees)
     |
     |
N=4 (4 trees)
     |
     |
N=5 (9 trees)
```

## The Complete Family Tree

```
                        N=3: THE FUNDAMENTAL DUALITY
                        ============================

            ((()))                              (()())
         [Vertical]                          [Horizontal]
              |                                    |
              |                                    |
    +---------+---------+                +---------+---------+
    |                   |                |                   |
    |                   |                |                   |
    
                    N=4: THE VOCABULARY
                    ===================

(((())))          ((()()))          ((())())          (()()())
[Max Chain]    [Nested Fork]    [Asymmetric]      [Flat Forest]
    |                 |                |                 |
    |                 |                |                 |
 +--+--+           +--+--+          +--+--+           +--+--+
 |     |           |     |          |     |           |     |
 |     |           |     |          |     |           |     |

                    N=5: THE EXPANSION
                    ==================

1.         5.     2.      6.     3.      8.     4.      9.
((((()))))  (((()))())  (((()())))  ((()())())  (((())()))  ((())()())  ((()()()))  (()()()())
[Deepen]  [Juxtapose] [Deepen]  [Juxtapose]  [Nest]    [Widen]    [Nest]    [Widen]


                        SPECIAL CASE
                        ============
                    
                    (())  +  (())
                     n=2      n=2
                        \    /
                         \  /
                          \/
                          7.
                    ((())(()))
                    [Combine]
```

## Detailed Family Trees

### Family 1: The Vertical Dynasty

```
N=3:           ((()))
               Vertical
                  |
                  |
N=4:           (((())))
            Maximum Chain
                  |
          +-------+-------+
          |               |
       Deepen        Juxtapose
          |               |
N=5:  ((((()))))    (((()))())
      5-Chain       Chain+Sibling
```

### Family 2: The Nested Fork Dynasty

```
N=3:           ((()))
               Vertical
                  |
                  |
N=4:           ((()()))
            Nested Fork
                  |
          +-------+-------+
          |               |
       Deepen        Juxtapose
          |               |
N=5:  (((()())))    ((()())())
      Deep Fork     Fork+Sibling
```

### Family 3: The Asymmetric Dynasty

```
N=3:           ((()))  +  (()())
               Vertical   Horizontal
                     \    /
                      \  /
N=4:               ((())())
                 Asymmetric Split
                      |
              +-------+-------+
              |               |
            Nest           Widen
              |               |
N=5:    (((())()))      ((())()())
        Nested Asym     Chain+2Siblings
```

### Family 4: The Horizontal Dynasty

```
N=3:           (()())
              Horizontal
                  |
                  |
N=4:           (()()())
            Flat Forest (3)
                  |
          +-------+-------+
          |               |
        Nest           Widen
          |               |
N=5:  ((()()()))    (()()()())
      Nested 3      Flat Forest (4)
```

### Family 5: The Symmetric Branch

```
N=2:      (())          (())
        Container    Container
             \          /
              \        /
               \      /
                \    /
N=5:         ((())(()))
           Symmetric Double
```

## The Four Operations Visualized

### Operation 1: DEEPEN (Add inside)

```
Before:                After:
    ●                     ●
    |                     |
    ●                     ●
    |        →            |
    ●                     ●
                          |
                          ●

Increases depth by 1
Preserves vertical character
```

### Operation 2: NEST (Wrap in container)

```
Before:                After:
      ●                   ●
     / \                  |
    ●   ●      →          ●
                         / \
                        ●   ●

Adds one level at root
Wraps entire structure
```

### Operation 3: JUXTAPOSE (Add beside at root)

```
Before:                After:
    ●                     ●
    |                    / \
    ●        →          ●   ●
    |                   |
    ●                   ●

Adds sibling at root level
Creates binary split
```

### Operation 4: WIDEN (Add beside at leaves)

```
Before:                After:
      ●                   ●
    / | \              / | \ \
   ●  ●  ●    →       ●  ●  ●  ●

Extends breadth
Adds another independent branch
```

## Growth Pattern Matrix

```
                    N=4 PARENTS
                    ===========

Operation      Tree 1    Tree 2    Tree 3    Tree 4
               (((())))  ((()()))  ((())())  (()()())
               --------  --------  --------  --------
DEEPEN    →    Tree 1    Tree 2      —         —
NEST      →      —         —       Tree 3    Tree 4
JUXTAPOSE →    Tree 5    Tree 6      —         —
WIDEN     →      —         —       Tree 8    Tree 9

SPECIAL: COMBINE → Tree 7 (from two n=2 structures)
```

## The Spectrum: Depth vs. Breadth

```
N=5 Trees arranged by depth and breadth:

Depth 5, Width 1:
    ((((()))))
         |
         
Depth 4, Width 2:
    (((()())))  (((())()))  (((()))())
         |           |           |
         
Depth 3, Width 2-3:
    ((()()()))  ((()())())  ((())(()))  ((())()())
         |           |           |           |
         
Depth 2, Width 4:
    (()()()())
         |

    ↑                                            ↑
Max Depth                                   Max Breadth
Min Width                                   Min Depth
Sequential                                  Parallel
```

## Compositional Pathways

### Path to Maximum Depth (Tree 1)

```
n=3: ((()))
      ↓ Deepen
n=4: (((())))
      ↓ Deepen
n=5: ((((()))))
      ↓ Deepen
n=6: (((((())))))
      ...
```

### Path to Maximum Breadth (Tree 9)

```
n=3: (()())
      ↓ Widen
n=4: (()()())
      ↓ Widen
n=5: (()()()())
      ↓ Widen
n=6: (()()()()())
      ...
```

### Path to Balanced Structure (Tree 7)

```
n=2: (())
      ↓ Combine with (())
n=5: ((())(()))
      ↓ Various operations
n=8: More balanced structures
      ...
```

## The Fractal Nature

Each generation follows the same pattern:

```
Generation N:
  - Has k trees
  - Each tree can be extended in ~2 ways
  - Some special combinations arise
  
Generation N+1:
  - Has ~2k trees (roughly)
  - Each becomes a parent for N+2
  - Pattern repeats recursively
```

This creates a **fractal tree of trees**, where:
- Each node is a tree structure
- Each edge is a compositional operation
- The entire graph grows exponentially

## Summary: The Recursive Blueprint

The visual taxonomy reveals:

1. **Systematic Growth**: Each n=4 tree produces exactly 2 n=5 children
2. **Operation Pairing**: Vertical structures use (Deepen, Juxtapose); Mixed structures use (Nest, Widen)
3. **Special Cases**: Symmetric combinations can arise from smaller structures
4. **Fractal Self-Similarity**: The pattern repeats at every level
5. **Exponential Explosion**: 2 → 4 → 9 → 20 → 48 → ...

This is the **recursive blueprint** of computational structure, encoded in the A000081 sequence and embodied in the Lisp bootstrapping framework.
