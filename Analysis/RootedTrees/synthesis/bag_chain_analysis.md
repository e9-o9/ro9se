# Mathematical Analysis: Bag Chain Algorithm and OEIS A000081

## OEIS A000081: Key Mathematical Properties

### Sequence Definition
**A000081**: Number of unlabeled rooted trees with n nodes

**Sequence**: 0, 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, ...

### Recurrence Relation
The fundamental recurrence for A000081 is:

```
a(n+1) = (1/n) * Sum_{k=1..n} ( Sum_{d|k} d*a(d) ) * a(n-k+1)
```

Where:
- `a(n)` = number of rooted trees with n nodes
- `Sum_{d|k}` = sum over all divisors d of k
- The inner sum `Sum_{d|k} d*a(d)` counts weighted subtrees

### Generating Function (Pólya's Formula)
```
A(x) = x * exp(A(x) + A(x^2)/2 + A(x^3)/3 + A(x^4)/4 + ...)
```

Also expressed as:
```
A(x) = x / Product_{n>=1} (1-x^n)^a(n)
```

### Euler Transform Property
The Euler transform of A000081 is the sequence itself with offset -1.

This means: If we have trees of size k, we can combine them to form larger trees.

## Bag Chain Algorithm Structure

### Algorithm Overview

```python
def bags(n, cache={}):
    if not n: return [(0, "")]
    
    # Build all configurations for sizes 1 to n-1
    upto = sum([bags(x) for x in range(n-1, 0, -1)], [])
    
    # Chain together to form n-bag configurations
    return [(c+1, '('+s+')') for c,s in bagchain((0, ""), n-1, upto)]

def bagchain(x, n, bb, start=0):
    if not n: return [x]
    
    out = []
    for i in range(start, len(bb)):
        c, s = bb[i]
        if c <= n:
            out += bagchain((x[0] + c, x[1] + s), n-c, bb, i)
    return out
```

### Key Components

1. **Base Case**: `bags(0)` returns empty configuration
2. **Recursive Build**: For n bags, first compute all configurations for 1 to n-1 bags
3. **Chaining**: Combine smaller configurations to sum to n-1 nodes
4. **Wrapping**: Wrap the combined configuration in an outer bag (the root)

## Mathematical Connection

### The Core Insight: Partition-Based Generation

The bag chain algorithm implements a **partition-based tree generation** strategy:

1. **For n-node tree**: The root has n-1 nodes in its subtrees
2. **Partition n-1**: Break n-1 into sums of positive integers
3. **Map to subtrees**: Each part k in the partition corresponds to a k-node subtree
4. **Combine**: Attach these subtrees to the root

### Example: n=5 (9 trees expected)

For 5 bags, we need to partition 4 (since 1 bag is the root):

**Partitions of 4**:
- 4 = 4 (one 4-node subtree)
- 4 = 3+1 (one 3-node, one 1-node subtree)
- 4 = 2+2 (two 2-node subtrees)
- 4 = 2+1+1 (one 2-node, two 1-node subtrees)
- 4 = 1+1+1+1 (four 1-node subtrees)

For each partition, we select subtrees of the appropriate sizes:
- **Partition [4]**: Choose from 4 trees with 4 nodes → 4 configurations
- **Partition [3,1]**: Choose from 2 trees with 3 nodes, 1 tree with 1 node → 2 configurations
- **Partition [2,2]**: Choose from 1 tree with 2 nodes (same tree twice) → 1 configuration
- **Partition [2,1,1]**: Choose from 1 tree with 2 nodes, 1 tree with 1 node (twice) → 1 configuration
- **Partition [1,1,1,1]**: Choose from 1 tree with 1 node (four times) → 1 configuration

**Total**: 4 + 2 + 1 + 1 + 1 = 9 ✓

### How Bagchain Implements This

The `bagchain` function performs **ordered selection with repetition**:

```python
def bagchain(x, n, bb, start=0):
    # x: current accumulated configuration (count, string)
    # n: remaining nodes to allocate
    # bb: list of all smaller tree configurations
    # start: index to maintain ordering (avoid duplicates)
```

**Key mechanism**:
1. Iterate through available subtree sizes (from `bb`)
2. If subtree size `c <= n`, it can be added
3. Recursively fill remaining `n-c` nodes
4. `start=i` ensures we only use trees at index i or later (maintains canonical ordering)

### Connection to Recurrence Relation

The OEIS recurrence:
```
a(n+1) = (1/n) * Sum_{k=1..n} ( Sum_{d|k} d*a(d) ) * a(n-k+1)
```

Can be understood as:
- For each way to partition n into subtrees
- Count the number of ways to select subtrees of each size
- Account for symmetries (the 1/n normalization factor)

The bag chain algorithm achieves the same result through:
- **Explicit enumeration** of all partitions (via recursive chaining)
- **Canonical ordering** (via the `start` parameter) to avoid counting duplicates
- **Direct construction** rather than counting

### Why It Generates A000081

The algorithm generates exactly the A000081 sequence because:

1. **Completeness**: Every rooted tree is generated
   - All partitions of n-1 are explored
   - All combinations of subtrees are considered

2. **Uniqueness**: No tree is generated twice
   - Canonical ordering via `start` parameter
   - Only non-decreasing sequences of subtree indices

3. **Correctness**: Tree structure is preserved
   - Root wrapping: `'(' + s + ')'`
   - Subtree concatenation: `x[1] + s`

## Comparison with Direct Recurrence

### Direct Recurrence (Counting)
```python
def a(n):
    if n <= 1: return n
    return sum(
        sum(d * a(d) for d in divisors(k)) * a(n-k)
        for k in range(1, n)
    ) // (n-1)
```

**Characteristics**:
- Counts trees without enumerating them
- Uses divisor sums for weighted counting
- More efficient for large n (O(n²) with memoization)

### Bag Chain (Enumeration)
```python
def bags(n):
    # Builds actual tree representations
    # Returns list of all configurations
```

**Characteristics**:
- Enumerates all trees explicitly
- Constructs string representations
- Exponential space complexity (stores all trees)
- Provides actual tree structures, not just counts

## Generating Function Perspective

The generating function:
```
A(x) = x / Product_{n>=1} (1-x^n)^a(n)
```

Can be rewritten as:
```
A(x) = x * Product_{n>=1} (1-x^n)^(-a(n))
```

This is the **Euler transform**, which means:
- Take unlimited copies of trees of each size
- Combine them in all possible ways
- The coefficient of x^n gives a(n)

The bag chain algorithm implements this by:
1. Building all trees up to size n-1
2. Selecting combinations (with repetition) that sum to n-1
3. Wrapping in a root node

## Visualization: n=4 Example

**Partition [3]**: One 3-node subtree
```
Trees with 3 nodes: ((())), (()())

Result:
  (((()))) ← wrap ((()))
  ((()()))  ← wrap (()())
```

**Partition [2,1]**: One 2-node, one 1-node subtree
```
Trees with 2 nodes: (())
Trees with 1 node: ()

Result:
  ((())())  ← wrap (()) + ()
```

**Partition [1,1,1]**: Three 1-node subtrees
```
Trees with 1 node: ()

Result:
  (()()())  ← wrap () + () + ()
```

**Total for n=4**: 2 + 1 + 1 = 4 ✓

## Summary

The bag chain algorithm generates A000081 by:

1. **Recursive Decomposition**: Break n-node trees into (n-1)-node subtree collections
2. **Partition Enumeration**: Implicitly enumerate all integer partitions of n-1
3. **Canonical Selection**: Use ordered selection to avoid duplicates
4. **Explicit Construction**: Build actual tree representations

This approach is **mathematically equivalent** to the recurrence relation but operates through **explicit enumeration** rather than **combinatorial counting**. The algorithm naturally produces the A000081 sequence because it systematically generates all unlabeled rooted trees without duplication.
