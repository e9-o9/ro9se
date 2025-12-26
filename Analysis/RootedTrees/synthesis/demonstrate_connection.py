#!/usr/bin/env python3
"""
Demonstration: Connection between Bag Chain Algorithm and OEIS A000081

This script demonstrates how the bag chain dynamic programming approach
naturally generates the A000081 sequence (unlabeled rooted trees).
"""

from collections import defaultdict

# ============================================================================
# PART 1: Bag Chain Algorithm (Enumeration Approach)
# ============================================================================

def bags(n, cache={}):
    """
    Generate all rooted tree configurations for n nodes.
    Returns list of (count, string) tuples.
    """
    if not n: 
        return [(0, "")]
    
    # Build all configurations for sizes 1 to n-1
    upto = sum([bags(x) for x in range(n-1, 0, -1)], [])
    
    # Chain together to form n-bag configurations
    return [(c+1, '('+s+')') for c,s in bagchain((0, ""), n-1, upto)]

def bagchain(x, n, bb, start=0):
    """
    Chain together bag configurations to sum to n nodes.
    
    Args:
        x: Current accumulated configuration (count, string)
        n: Remaining nodes to allocate
        bb: List of all smaller tree configurations
        start: Index to maintain ordering (canonical form)
    """
    if not n: 
        return [x]
    
    out = []
    for i in range(start, len(bb)):
        c, s = bb[i]
        if c <= n:
            # Add this subtree and recursively fill remaining
            out += bagchain((x[0] + c, x[1] + s), n-c, bb, i)
    return out


# ============================================================================
# PART 2: Direct Recurrence (Counting Approach)
# ============================================================================

def divisors(n):
    """Return all divisors of n."""
    divs = []
    for i in range(1, int(n**0.5) + 1):
        if n % i == 0:
            divs.append(i)
            if i != n // i:
                divs.append(n // i)
    return sorted(divs)

def a000081_recurrence(n, memo={}):
    """
    Compute a(n) using the OEIS A000081 recurrence relation:
    a(n+1) = (1/n) * Sum_{k=1..n} ( Sum_{d|k} d*a(d) ) * a(n-k+1)
    
    Simplified for direct computation:
    a(n) = (1/(n-1)) * Sum_{k=1..n-1} ( Sum_{d|k} d*a(d) ) * a(n-k)
    """
    if n <= 1:
        return n
    
    if n in memo:
        return memo[n]
    
    total = 0
    for k in range(1, n):
        # Inner sum: Sum_{d|k} d*a(d)
        inner_sum = sum(d * a000081_recurrence(d, memo) for d in divisors(k))
        total += inner_sum * a000081_recurrence(n - k, memo)
    
    result = total // (n - 1)
    memo[n] = result
    return result


# ============================================================================
# PART 3: Analysis and Visualization
# ============================================================================

def analyze_partition_structure(n):
    """
    Analyze how the bag chain algorithm implicitly generates partitions.
    """
    print(f"\n{'='*70}")
    print(f"Analysis for n={n} (expecting a({n}) = {a000081_recurrence(n)} trees)")
    print(f"{'='*70}")
    
    # Generate all trees
    all_trees = bags(n)
    
    print(f"\nGenerated {len(all_trees)} trees:")
    for i, (count, tree_str) in enumerate(all_trees, 1):
        print(f"  {i:2d}. {tree_str}")
    
    # Analyze partition structure
    print(f"\n{'─'*70}")
    print("Partition Analysis (how subtrees combine):")
    print(f"{'─'*70}")
    
    # Group by structure
    partition_groups = defaultdict(list)
    
    for tree_str in [t[1] for t in all_trees]:
        # Count depth-1 subtrees (immediate children of root)
        subtree_sizes = extract_subtree_sizes(tree_str)
        partition = tuple(sorted(subtree_sizes, reverse=True))
        partition_groups[partition].append(tree_str)
    
    for partition in sorted(partition_groups.keys(), reverse=True):
        trees = partition_groups[partition]
        print(f"\nPartition {list(partition)} (sum={sum(partition)}): {len(trees)} tree(s)")
        for tree in trees:
            print(f"  {tree}")
    
    return all_trees

def extract_subtree_sizes(tree_str):
    """
    Extract sizes of immediate subtrees of the root.
    """
    if tree_str == "()":
        return []
    
    # Remove outer parentheses
    inner = tree_str[1:-1]
    
    subtree_sizes = []
    depth = 0
    start = 0
    
    for i, char in enumerate(inner):
        if char == '(':
            depth += 1
        elif char == ')':
            depth -= 1
            if depth == 0:
                # Found a complete subtree
                subtree = inner[start:i+1]
                subtree_sizes.append(count_nodes(subtree))
                start = i + 1
    
    return subtree_sizes

def count_nodes(tree_str):
    """Count nodes in a tree (number of '(' characters)."""
    return tree_str.count('(')


# ============================================================================
# PART 4: Verification and Comparison
# ============================================================================

def verify_sequence(max_n=10):
    """
    Verify that bag chain algorithm generates A000081 sequence.
    """
    print("\n" + "="*70)
    print("VERIFICATION: Bag Chain vs. A000081 Recurrence")
    print("="*70)
    print(f"\n{'n':>3} | {'Bag Chain':>12} | {'Recurrence':>12} | {'Match':>6}")
    print("-" * 50)
    
    for n in range(1, max_n + 1):
        bag_count = len(bags(n))
        recurrence_count = a000081_recurrence(n)
        match = "✓" if bag_count == recurrence_count else "✗"
        print(f"{n:3d} | {bag_count:12d} | {recurrence_count:12d} | {match:>6}")
    
    # Print the sequence
    sequence = [len(bags(n)) for n in range(1, max_n + 1)]
    print(f"\nGenerated sequence: {sequence}")
    print(f"OEIS A000081:       [1, 1, 2, 4, 9, 20, 48, 115, 286, 719, ...]")


def explain_connection():
    """
    Explain the mathematical connection between the two approaches.
    """
    print("\n" + "="*70)
    print("MATHEMATICAL CONNECTION")
    print("="*70)
    
    explanation = """
The bag chain algorithm and A000081 recurrence are mathematically equivalent:

1. RECURRENCE RELATION (Counting):
   a(n) = (1/(n-1)) * Σ_{k=1..n-1} [ Σ_{d|k} d·a(d) ] · a(n-k)
   
   This counts trees by:
   - Partitioning n-1 nodes among subtrees
   - Weighted sum accounts for symmetries
   - Division by (n-1) normalizes the count

2. BAG CHAIN ALGORITHM (Enumeration):
   - For n nodes: partition (n-1) into subtree sizes
   - Select subtrees of each size (with repetition)
   - Maintain canonical ordering to avoid duplicates
   - Wrap in root node: '(' + subtrees + ')'

3. KEY INSIGHT:
   The bagchain() function implements ORDERED SELECTION with repetition:
   - 'start' parameter ensures non-decreasing subtree indices
   - This naturally generates each tree exactly once
   - Equivalent to the symmetry handling in the recurrence

4. EULER TRANSFORM CONNECTION:
   A(x) = x / ∏_{n≥1} (1-x^n)^{a(n)}
   
   This generating function means:
   - Take unlimited copies of trees of each size
   - Combine them to form larger trees
   - The bag chain algorithm does this explicitly

5. PARTITION-BASED GENERATION:
   Both approaches implicitly enumerate integer partitions:
   - Recurrence: Σ_{k=1..n-1} sums over partition parts
   - Bag chain: Recursive selection generates all partitions
   - Both ensure each tree structure appears exactly once
"""
    print(explanation)


# ============================================================================
# MAIN DEMONSTRATION
# ============================================================================

def main():
    print("="*70)
    print("DEMONSTRATION: Bag Chain Algorithm and OEIS A000081")
    print("="*70)
    
    # Verify the sequence matches
    verify_sequence(max_n=10)
    
    # Detailed analysis for specific values
    for n in [4, 5]:
        analyze_partition_structure(n)
    
    # Explain the mathematical connection
    explain_connection()
    
    print("\n" + "="*70)
    print("CONCLUSION")
    print("="*70)
    print("""
The bag chain algorithm generates the A000081 sequence because it:
1. Systematically enumerates all rooted tree structures
2. Uses canonical ordering to avoid duplicates
3. Implements partition-based generation (equivalent to recurrence)
4. Explicitly constructs what the recurrence counts

Both approaches solve the same problem:
- Recurrence: "How many trees?" (counting)
- Bag chain: "What are the trees?" (enumeration)

The sequence emerges naturally from the combinatorial structure of
rooted trees and their recursive decomposition into subtrees.
""")

if __name__ == "__main__":
    main()
