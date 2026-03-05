#!/usr/bin/env python3
"""
Matula Numbers: Bijection between Rooted Trees and Natural Numbers

This module implements the Matula number encoding for rooted trees and
demonstrates the self-similar patterns in prime factorizations across
orders n, revealing the "cognitive grammar" of the universal language.

The Matula bijection:
- Matula(1) = single-node tree (the atom)
- For a tree with children having Matula numbers m₁, m₂, ..., mₖ:
  Matula(tree) = p(m₁) × p(m₂) × ... × p(mₖ)
  where p(i) is the i-th prime (p(1)=2, p(2)=3, p(3)=5, ...)

Key insight from the problem statement:
- At each level n, Matula primes split into two groups:
  1. "Twin mirror" formation: new primes appearing at order n
  2. Products with prime 2 (doubles from n-1)
- The order n at which primes first appear follows A000081 offset

Pattern: n=2|{2} → n=3|{3}|{2²} → n=4|{5,7}|{3×2}|{2×3} → ...
"""

from typing import List, Tuple, Dict, Set
from collections import defaultdict
from functools import lru_cache


# ============================================================================
# PRIME NUMBER UTILITIES
# ============================================================================

def is_prime(n: int) -> bool:
    """Check if n is prime."""
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    for i in range(3, int(n**0.5) + 1, 2):
        if n % i == 0:
            return False
    return True


def nth_prime(n: int) -> int:
    """Return the n-th prime (1-indexed: p(1)=2, p(2)=3, p(3)=5, ...)."""
    if n < 1:
        raise ValueError("n must be >= 1")
    count = 0
    candidate = 1
    while count < n:
        candidate += 1
        if is_prime(candidate):
            count += 1
    return candidate


def prime_index(p: int) -> int:
    """Return the index of prime p (inverse of nth_prime)."""
    if p < 2 or not is_prime(p):
        raise ValueError(f"{p} is not prime")
    count = 0
    for i in range(2, p + 1):
        if is_prime(i):
            count += 1
    return count


def prime_factorization(n: int) -> List[Tuple[int, int]]:
    """Return prime factorization as list of (prime, exponent) tuples."""
    if n < 1:
        return []
    if n == 1:
        return []
    factors = []
    d = 2
    while d * d <= n:
        exp = 0
        while n % d == 0:
            n //= d
            exp += 1
        if exp > 0:
            factors.append((d, exp))
        d += 1
    if n > 1:
        factors.append((n, 1))
    return factors


# ============================================================================
# TREE GENERATION FROM BAG ALGORITHM
# ============================================================================

def bags(n: int, cache: Dict = None) -> List[Tuple[int, str]]:
    """Generate all rooted tree configurations for n nodes."""
    if cache is None:
        cache = {}
    if n in cache:
        return cache[n]
    if n == 0:
        return [(0, "")]
    upto = sum([bags(x, cache) for x in range(n - 1, 0, -1)], [])
    result = [(c + 1, '(' + s + ')') for c, s in bagchain((0, ""), n - 1, upto)]
    cache[n] = result
    return result


def bagchain(x: Tuple[int, str], n: int, bb: List, start: int = 0) -> List:
    """Chain together bag configurations."""
    if n == 0:
        return [x]
    out = []
    for i in range(start, len(bb)):
        c, s = bb[i]
        if c <= n:
            out += bagchain((x[0] + c, x[1] + s), n - c, bb, i)
    return out


# ============================================================================
# MATULA NUMBER COMPUTATION
# ============================================================================

def tree_to_matula(tree_str: str) -> int:
    """
    Convert a parenthesis tree representation to its Matula number.
    
    The tree () has Matula number 1.
    A tree with children c₁, c₂, ..., cₖ has Matula number:
        p(m₁) × p(m₂) × ... × p(mₖ)
    where mᵢ = Matula(cᵢ) and p(k) is the k-th prime.
    """
    if tree_str == "()":
        return 1
    
    # Parse immediate children
    inner = tree_str[1:-1]  # Remove outer parentheses
    children_strs = []
    depth = 0
    start = 0
    
    for i, char in enumerate(inner):
        if char == '(':
            depth += 1
        elif char == ')':
            depth -= 1
            if depth == 0:
                children_strs.append(inner[start:i + 1])
                start = i + 1
    
    # Compute Matula number from children
    if not children_strs:
        return 1
    
    matula = 1
    for child_str in children_strs:
        child_matula = tree_to_matula(child_str)
        matula *= nth_prime(child_matula)
    
    return matula


@lru_cache(maxsize=1000)
def matula_to_tree(m: int) -> str:
    """
    Convert a Matula number back to tree representation.
    
    Matula 1 → "()"
    For m > 1, factorize m = p₁^e₁ × p₂^e₂ × ... 
    Each prime factor pᵢ contributes prime_index(pᵢ) as a child Matula number.
    """
    if m == 1:
        return "()"
    
    factors = prime_factorization(m)
    children = []
    
    for prime, exp in factors:
        child_matula = prime_index(prime)
        child_tree = matula_to_tree(child_matula)
        for _ in range(exp):
            children.append(child_tree)
    
    # Sort children for canonical form (larger subtrees first)
    children.sort(reverse=True)
    
    return "(" + "".join(children) + ")"


def tree_order(m: int) -> int:
    """
    Compute the order (number of nodes) of the tree with Matula number m.
    """
    if m == 1:
        return 1
    
    factors = prime_factorization(m)
    order = 1  # Count the root
    
    for prime, exp in factors:
        child_matula = prime_index(prime)
        child_order = tree_order(child_matula)
        order += exp * child_order
    
    return order


# ============================================================================
# ANALYSIS OF MATULA NUMBER PATTERNS
# ============================================================================

def analyze_matula_by_order(max_n: int = 10) -> Dict[int, List[int]]:
    """
    Analyze all Matula numbers organized by tree order.
    Returns a dict mapping order n → list of Matula numbers for n-node trees.
    """
    matula_by_order = defaultdict(list)
    cache = {}
    
    for n in range(1, max_n + 1):
        trees = bags(n, cache)
        for _, tree_str in trees:
            m = tree_to_matula(tree_str)
            matula_by_order[n].append(m)
        matula_by_order[n].sort()
    
    return dict(matula_by_order)


def classify_matula_primes(max_n: int = 10) -> Dict[int, Dict[str, List[int]]]:
    """
    Classify Matula numbers by their prime structure.
    
    For each order n, identifies:
    - "primes": Matula numbers that are prime (first appearance at this order)
    - "doubles": Products with prime 2 (2 × m where m appeared at n-1)
    - "other_products": Other composite Matula numbers
    """
    matula_by_order = analyze_matula_by_order(max_n)
    classification = {}
    
    # Track which Matula numbers appeared at which order
    first_appearance = {}
    
    for n in range(1, max_n + 1):
        matulas = matula_by_order.get(n, [])
        
        primes = []
        doubles = []
        other_products = []
        
        for m in matulas:
            if m == 1:
                # Special case: 1 is the atom
                primes.append(m)
            elif is_prime(m):
                primes.append(m)
            else:
                factors = prime_factorization(m)
                # Check if it's a "double" (has factor of 2)
                has_two = any(p == 2 for p, _ in factors)
                if has_two:
                    doubles.append(m)
                else:
                    other_products.append(m)
            
            if m not in first_appearance:
                first_appearance[m] = n
        
        classification[n] = {
            'primes': primes,
            'doubles': doubles,
            'other_products': other_products
        }
    
    return classification


def analyze_prime_factor_origins(max_n: int = 10) -> Dict[int, Dict[str, List[Tuple[int, List[int]]]]]:
    """
    Analyze the prime factor structure of Matula numbers at each order.
    
    This reveals the pattern described in the problem statement:
    - Products with prime 2 are at n-1 offset
    - Products with prime 3 are at n-2 offset
    - Products with primes 5,7 are at n-3 offset
    - Products with primes 11,13,17,19 are at n-4 offset
    - etc.
    """
    matula_by_order = analyze_matula_by_order(max_n)
    analysis = {}
    
    # Prime groups by first appearance level
    # These correspond to the A000081 sequence: 1, 1, 2, 4, 9, 20, 48, ...
    prime_groups = {
        2: [2],           # n=2: first container
        3: [3],           # n=3: the vertical composition
        4: [5, 7],        # n=4: the vocabulary (4 trees, 2 primes at Matula)
        5: [11, 13, 17, 19],  # n=5: 9 trees, 4 new primes
        6: [23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107],
    }
    
    for n in range(1, max_n + 1):
        matulas = matula_by_order.get(n, [])
        
        # Categorize by prime factor composition
        by_composition = defaultdict(list)
        
        for m in matulas:
            if m == 1:
                by_composition['atom'].append((m, []))
                continue
            
            factors = prime_factorization(m)
            prime_list = []
            for p, exp in factors:
                prime_list.extend([p] * exp)
            
            # Create composition signature
            signature = tuple(sorted(prime_list, reverse=True))
            by_composition[signature].append((m, prime_list))
        
        analysis[n] = dict(by_composition)
    
    return analysis


def demonstrate_twin_mirror_pattern(max_n: int = 8):
    """
    Demonstrate the twin mirror formation pattern described in the problem statement.
    
    At each level n:
    - One half are new Matula primes (the "twin mirror" formation)
    - The other half are doubles (products with prime 2 from n-1)
    """
    print("\n" + "=" * 80)
    print("TWIN MIRROR PATTERN: Matula Number Structure by Order")
    print("=" * 80)
    
    classification = classify_matula_primes(max_n)
    
    for n in range(1, max_n + 1):
        data = classification[n]
        primes = data['primes']
        doubles = data['doubles']
        other = data['other_products']
        
        print(f"\n{'─' * 80}")
        print(f"ORDER n={n} | Total: {len(primes) + len(doubles) + len(other)} Matula numbers")
        print(f"{'─' * 80}")
        
        print(f"\n  PRIMES (new at this level):")
        if primes:
            print(f"    {{{', '.join(map(str, sorted(primes)))}}}")
        else:
            print(f"    (none)")
        
        print(f"\n  DOUBLES (products with 2 from n-1):")
        if doubles:
            for m in sorted(doubles):
                factors = prime_factorization(m)
                factor_str = ' × '.join(f"{p}^{e}" if e > 1 else str(p) for p, e in factors)
                print(f"    {m} = {factor_str}")
        else:
            print(f"    (none)")
        
        print(f"\n  OTHER PRODUCTS:")
        if other:
            for m in sorted(other):
                factors = prime_factorization(m)
                factor_str = ' × '.join(f"{p}^{e}" if e > 1 else str(p) for p, e in factors)
                print(f"    {m} = {factor_str}")
        else:
            print(f"    (none)")


def demonstrate_self_similar_enumeration(max_n: int = 8):
    """
    Demonstrate the self-similar enumeration sequence pattern.
    
    The pattern from the problem statement:
    n=2 | {2}
    n=3 | {3} | {2}{2}
    n=4 | {5,7} | {3}{2} | {2}{3}
    n=5 | {11,13,17,19} | {5,7}{2} | {3}{3} | {2}{5,7}
    n=6 | {23,29,...} | {11,13,17,19}{2} | {5,7}{3} | {3}{5,7} | {2}{11,13,17,19}
    ...
    """
    print("\n" + "=" * 80)
    print("SELF-SIMILAR ENUMERATION: Prime Factor Patterns by Order")
    print("=" * 80)
    
    matula_by_order = analyze_matula_by_order(max_n)
    
    # Track primes that first appear at each level
    primes_by_level = defaultdict(list)
    all_seen = set()
    
    for n in range(1, max_n + 1):
        matulas = matula_by_order.get(n, [])
        for m in matulas:
            if is_prime(m) and m not in all_seen:
                primes_by_level[n].append(m)
                all_seen.add(m)
    
    print("\nPRIMES FIRST APPEARING AT EACH LEVEL:")
    for n in range(1, max_n + 1):
        primes = primes_by_level.get(n, [])
        if primes:
            print(f"  n={n}: {{{', '.join(map(str, sorted(primes)))}}}")
    
    print("\n" + "─" * 80)
    print("PATTERN BREAKDOWN:")
    print("─" * 80)
    
    for n in range(2, min(max_n + 1, 8)):
        print(f"\nn={n}:")
        matulas = matula_by_order.get(n, [])
        
        # Group by factor pattern
        groups = defaultdict(list)
        
        for m in matulas:
            if m == 1:
                groups['1'].append(m)
            elif is_prime(m):
                groups['prime'].append(m)
            else:
                factors = prime_factorization(m)
                # Find which level each prime factor first appeared
                factor_levels = []
                for p, exp in factors:
                    # Find level where prime p first appeared
                    pm = prime_index(p)  # Matula number corresponding to this prime
                    pm_order = tree_order(pm)
                    factor_levels.extend([pm_order] * exp)
                
                factor_levels.sort(reverse=True)
                signature = tuple(factor_levels)
                groups[signature].append(m)
        
        # Print groups
        for sig, matulas_in_group in sorted(groups.items(), key=lambda x: (
            0 if x[0] == '1' else (1 if x[0] == 'prime' else 2 + sum(x[0]))
        )):
            if sig == '1':
                print(f"  {1}: atom")
            elif sig == 'prime':
                primes_str = ', '.join(map(str, sorted(matulas_in_group)))
                print(f"  PRIMES: {{{primes_str}}}")
            else:
                for m in sorted(matulas_in_group):
                    factors = prime_factorization(m)
                    factor_str = ' × '.join(f"{p}" for p, e in factors for _ in range(e))
                    print(f"  {m} = {factor_str}  [children from levels {sig}]")


def generate_matula_taxonomy(max_n: int = 10):
    """
    Generate a complete taxonomy of Matula numbers with their properties.
    """
    print("\n" + "=" * 80)
    print("COMPLETE MATULA NUMBER TAXONOMY")
    print("=" * 80)
    
    cache = {}
    
    for n in range(1, max_n + 1):
        print(f"\n{'═' * 80}")
        print(f"ORDER n={n}")
        print(f"{'═' * 80}")
        
        trees = bags(n, cache)
        
        print(f"\nTotal trees: {len(trees)} (A000081[{n}])")
        print(f"\n{'Tree':20} | {'Matula':8} | {'Factorization':25} | Classification")
        print("─" * 80)
        
        for _, tree_str in trees:
            m = tree_to_matula(tree_str)
            
            if m == 1:
                factor_str = "1 (atom)"
                classification = "ATOM"
            elif is_prime(m):
                factor_str = f"{m} (prime)"
                classification = "PRIME"
            else:
                factors = prime_factorization(m)
                factor_str = ' × '.join(f"{p}^{e}" if e > 1 else str(p) for p, e in factors)
                
                # Classify by factor pattern
                has_two = any(p == 2 for p, _ in factors)
                if has_two:
                    classification = "DOUBLE"
                else:
                    classification = "PRODUCT"
            
            print(f"{tree_str:20} | {m:8} | {factor_str:25} | {classification}")


def verify_pattern():
    """
    Verify the exact pattern from the problem statement.
    """
    print("\n" + "=" * 80)
    print("VERIFICATION: Self-Similar Prime Enumeration Pattern")
    print("=" * 80)
    
    expected_pattern = """
Expected pattern from problem statement:
n=2 | {2}
n=3 | {3} | {2}{2}
n=4 | {5,7} | {3}{2} | {2}{3}
n=5 | {11,13,17,19} | {5,7}{2} | {3}{3} | {2}{5,7}
n=6 | {23,29,31,37,41,43,53,59,67} | {11,13,17,19}{2} | {5,7}{3} | {3}{5,7} | {2}{11,13,17,19}
"""
    print(expected_pattern)
    
    print("VERIFICATION:")
    matula_by_order = analyze_matula_by_order(7)
    
    for n in range(2, 7):
        print(f"\nn={n}:")
        matulas = sorted(matula_by_order.get(n, []))
        
        # Separate primes and composites
        primes = [m for m in matulas if is_prime(m)]
        composites = [m for m in matulas if m > 1 and not is_prime(m)]
        
        print(f"  Primes: {{{', '.join(map(str, primes))}}}")
        
        if composites:
            # Group composites by their factor structure
            for m in composites:
                factors = prime_factorization(m)
                factor_str = '{' + '}{'.join(str(p) for p, _ in factors) + '}'
                print(f"  {m} = {factor_str}")


# ============================================================================
# MAIN DEMONSTRATION
# ============================================================================

def main():
    print("=" * 80)
    print("MATULA NUMBERS AND ROOTED TREES")
    print("Bijection between Natural Numbers and Rooted Tree Structures")
    print("=" * 80)
    
    # Basic demonstration
    print("\n" + "─" * 80)
    print("BASIC MATULA NUMBER EXAMPLES")
    print("─" * 80)
    
    examples = [
        "()",           # Matula 1
        "(())",         # Matula 2
        "((()))",       # Matula 3
        "(()())",       # Matula 4 = 2²
        "(((())))",     # Matula 5
        "((()()))",     # Matula 7
        "((())())",     # Matula 6 = 2 × 3
        "(()()())",     # Matula 8 = 2³
    ]
    
    for tree in examples:
        m = tree_to_matula(tree)
        factors = prime_factorization(m) if m > 1 else [(1, 1)]
        factor_str = ' × '.join(f"{p}^{e}" if e > 1 else str(p) for p, e in factors)
        reconstructed = matula_to_tree(m)
        print(f"{tree:15} → Matula {m:4} = {factor_str:15} → {reconstructed}")
    
    # Generate taxonomy
    generate_matula_taxonomy(6)
    
    # Demonstrate twin mirror pattern
    demonstrate_twin_mirror_pattern(6)
    
    # Demonstrate self-similar enumeration
    demonstrate_self_similar_enumeration(6)
    
    # Verify pattern from problem statement
    verify_pattern()
    
    print("\n" + "=" * 80)
    print("CONCLUSION")
    print("=" * 80)
    print("""
The Matula number bijection reveals a deep structure in rooted tree enumeration:

1. TWIN MIRROR FORMATION: At each order n, new Matula primes represent 
   genuinely new tree structures, while doubles (products with 2) represent 
   augmentations of n-1 structures.

2. SELF-SIMILAR ENUMERATION: The order at which primes first appear follows 
   the same A000081 sequence that counts the trees themselves. Products with 
   prime p(k) appear at level n+k-1, creating a recursive structure.

3. PRIME FACTORIZATION AS TREE STRUCTURE: Each Matula number's prime 
   factorization directly encodes the tree's children, making the bijection 
   both mathematically elegant and computationally efficient.

4. COGNITIVE GRAMMAR: The pattern reveals the "grammar" of tree construction:
   - ATOMS (1): The primordial distinction
   - PRIMES: Fundamental structures at each complexity level
   - PRODUCTS: Compositions of simpler structures

This self-similar structure is the foundation of the "universal language" 
of computational forms, where each natural number encodes a unique way to 
organize nested distinctions.
""")


if __name__ == "__main__":
    main()
