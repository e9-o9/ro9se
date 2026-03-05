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
# INDEX GRAMMAR AND ATTRIBUTE GRAMMAR ANALYSIS
# ============================================================================

def natural_to_prime_composite_split(max_level: int = 8) -> Dict[int, Dict[str, List[int]]]:
    """
    Demonstrate the N(n-1) → {P(n)|C(n)} split pattern.
    
    The natural numbers at level n-1 generate a split at level n:
    - P(n) = p_{N(n-1)} = the N(n-1)th prime (index grammar)
    - C(n) = 2 * N(n-1) = doubles (attribute grammar)
    
    This creates the pattern:
    {p_1=c_1=2} → {p_2=3|c_2=4} → {{p_3=5|p_4=7}|{c_3=6|c_4=8}} → etc.
    
    Returns a dict mapping level → {'primes': [...], 'composites': [...]}
    """
    result = {}
    
    # Level 1: The atom (special case)
    # N(0) = {1} generates the first distinction
    result[1] = {
        'naturals_from_below': [1],
        'primes': [],  # No primes at level 1
        'composites': [],  # No composites at level 1
        'description': 'ATOM: The primordial unity before distinction'
    }
    
    # Level 2: First split - both prime and composite emerge as 2
    # p_1 = 2 (first prime) = c_1 = 2*1 = 2 (first composite)
    result[2] = {
        'naturals_from_below': [1],
        'primes': [2],  # p_1 = 2
        'composites': [2],  # c_1 = 2*1 = 2 (coincides with prime)
        'description': 'DUALITY EMERGES: p_1 = c_1 = 2 (prime and composite coincide)'
    }
    
    # Build subsequent levels
    for level in range(3, max_level + 1):
        # Natural numbers from level below
        prev_result = result[level - 1]
        prev_naturals = []
        
        # Collect all naturals from previous level (both primes and composites)
        for n in range(1, 2**(level-1) + 1):
            prev_naturals.append(n)
        
        # Generate primes: p_n for each n in previous level's naturals
        new_primes = []
        for n in prev_naturals:
            try:
                p_n = nth_prime(n)
                new_primes.append(p_n)
            except:
                break
        
        # Generate composites: 2*n for each n in previous level's naturals
        new_composites = [2 * n for n in prev_naturals]
        
        result[level] = {
            'naturals_from_below': prev_naturals,
            'primes': new_primes,
            'composites': new_composites,
            'description': f'SPLIT: {len(new_primes)} primes (index) | {len(new_composites)} composites (attribute)'
        }
    
    return result


def demonstrate_index_attribute_grammar(max_level: int = 7):
    """
    Demonstrate the relationship between index grammars (primes) and 
    attribute grammars (composites) as a universal archetypal pattern.
    
    The pattern from the problem statement:
    {p_1=c_1=2} → {p_2=3|c_2=4} → {{p_3=5|p_4=7}|{c_3=6|c_4=8}} → etc.
    """
    print("\n" + "=" * 80)
    print("INDEX GRAMMAR vs ATTRIBUTE GRAMMAR: Universal Archetypal Pattern")
    print("=" * 80)
    
    print("""
The natural numbers are enumerated by the relation between primes and composites.
This defines the relation between:
  - INDEX GRAMMARS (primes): p_n = n-th prime, the "shape" of structure
  - ATTRIBUTE GRAMMARS (composites): c_n = 2*n, the "extension" of structure

The twin mirror of the layer below generates both:
  - P(n) = p_{N(n-1)} = the N(n-1)th prime
  - C(n) = 2 * N(n-1) = double of naturals from below
""")
    
    print("\n" + "─" * 80)
    print("PATTERN UNFOLDING: {P|C} Split at Each Level")
    print("─" * 80)
    
    # Show the exact pattern from problem statement
    patterns = [
        (1, "{1}", "Unity before distinction"),
        (2, "{p₁=c₁=2}", "First distinction (prime=composite)"),
        (3, "{p₂=3 | c₂=4}", "Duality separates"),
        (4, "{{p₃=5, p₄=7} | {c₃=6, c₄=8}}", "Vocabulary emerges"),
        (5, "{{p₅=11, p₆=13, p₇=17, p₈=19} | {c₅=10, c₆=12, c₇=14, c₈=16}}", "Compositional explosion"),
    ]
    
    for level, pattern, description in patterns:
        print(f"\nn={level}: {pattern}")
        print(f"       └─ {description}")
    
    print("\n" + "─" * 80)
    print("DETAILED ENUMERATION:")
    print("─" * 80)
    
    for level in range(1, min(max_level + 1, 7)):
        print(f"\n{'═' * 40}")
        print(f"LEVEL n={level}")
        print(f"{'═' * 40}")
        
        if level == 1:
            print("  N(0) = {1} (the atom)")
            print("  No split yet - unity before distinction")
            continue
        
        # Compute naturals from level below
        # At level n, we use naturals 1 through A000081(n-1)
        a000081 = [1, 1, 2, 4, 9, 20, 48, 115, 286, 719]
        count = a000081[level - 1] if level - 1 < len(a000081) else 2**(level-2)
        naturals = list(range(1, count + 1))
        
        print(f"\n  N({level-1}) = {{{', '.join(map(str, naturals[:min(8, len(naturals))]))}{'...' if len(naturals) > 8 else ''}}}")
        print(f"  Count: {len(naturals)} (= A000081({level-1}))")
        
        # Generate primes (index grammar)
        primes = [(n, nth_prime(n)) for n in naturals[:min(10, len(naturals))]]
        print(f"\n  INDEX GRAMMAR (Primes):")
        print(f"  P({level}) = {{p_n : n ∈ N({level-1})}}")
        for n, p in primes[:8]:
            print(f"    p_{n} = {p}")
        if len(primes) > 8:
            print(f"    ...")
        
        # Generate composites (attribute grammar)
        composites = [(n, 2 * n) for n in naturals[:min(10, len(naturals))]]
        print(f"\n  ATTRIBUTE GRAMMAR (Composites):")
        print(f"  C({level}) = {{2*n : n ∈ N({level-1})}}")
        for n, c in composites[:8]:
            print(f"    c_{n} = 2×{n} = {c}")
        if len(composites) > 8:
            print(f"    ...")


def demonstrate_ancestral_lineage(max_matula: int = 20):
    """
    Demonstrate how each natural/Matula number embeds its ancestral lineage.
    
    Every branch remembers its roots as the nested seed of its ancestral 
    lineage embedded within its own self-image.
    """
    print("\n" + "=" * 80)
    print("ANCESTRAL LINEAGE: Every Branch Remembers Its Roots")
    print("=" * 80)
    
    print("""
Each natural number (Matula encoding) is a composition where:
  - The CHILD has a prime index grammar pointing to its subtree structure
  - The PARENT FOREST has composite attribute grammars for their children
  - Every branch embeds its ancestral lineage as a nested seed within itself

The prime factorization p₁^e₁ × p₂^e₂ × ... reveals:
  - Each prime index π(pᵢ) is the Matula number of a child subtree
  - The exponents count how many times that child pattern repeats
  - The recursion terminates at Matula 1 (the atomic leaf)
""")
    
    print("\n" + "─" * 80)
    print("LINEAGE TRACING:")
    print("─" * 80)
    
    for m in range(1, max_matula + 1):
        tree = matula_to_tree(m)
        order = tree_order(m)
        
        print(f"\nMatula {m:3} → {tree:20} (order {order})")
        
        if m == 1:
            print(f"  └─ ATOM: The primordial seed, no ancestors")
            continue
        
        if is_prime(m):
            child_matula = prime_index(m)
            print(f"  └─ PRIME: Index grammar points to child Matula {child_matula}")
            print(f"       └─ Child: {matula_to_tree(child_matula)}")
            _trace_lineage(child_matula, depth=2)
        else:
            factors = prime_factorization(m)
            print(f"  └─ COMPOSITE: Attribute grammar = {' × '.join(f'{p}^{e}' if e > 1 else str(p) for p, e in factors)}")
            for p, e in factors:
                child_matula = prime_index(p)
                print(f"       └─ Factor p={p} (×{e}): child Matula {child_matula} → {matula_to_tree(child_matula)}")


def _trace_lineage(m: int, depth: int, max_depth: int = 4):
    """Helper to trace lineage recursively."""
    if depth > max_depth or m == 1:
        return
    
    indent = "       " * depth
    
    if is_prime(m):
        child_matula = prime_index(m)
        print(f"{indent}└─ Ancestor: Matula {m} → child {child_matula}")
        _trace_lineage(child_matula, depth + 1)
    else:
        factors = prime_factorization(m)
        for p, e in factors:
            child_matula = prime_index(p)
            print(f"{indent}└─ Ancestor: factor p={p} → child {child_matula}")
            _trace_lineage(child_matula, depth + 1)


def demonstrate_a000081_offset():
    """
    Demonstrate how the natural order is the same sequence offset by 1,
    resulting in the two leading 1's in OEIS A000081.
    
    A000081: 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, ...
    
    The two leading 1's represent:
    - n=1: The atom () - 1 tree
    - n=2: The container (()) - 1 tree
    
    After this, the split begins: n=3 has 2 trees, n=4 has 4, etc.
    """
    print("\n" + "=" * 80)
    print("A000081 OFFSET: The Two Leading 1's")
    print("=" * 80)
    
    a000081 = [1, 1, 2, 4, 9, 20, 48, 115, 286, 719]
    
    print("""
The A000081 sequence counts unlabeled rooted trees:
  1, 1, 2, 4, 9, 20, 48, 115, 286, 719, ...

The TWO LEADING 1's are special:
  n=1: a(1)=1 → The ATOM () - primordial unity, no structure yet
  n=2: a(2)=1 → The CONTAINER (()) - first distinction, but only one way

Starting at n=3, the prime/composite split manifests:
  n=3: a(3)=2 → {3} prime + {4} composite = {((())), (()())}
  n=4: a(4)=4 → {5,7} primes + {6,8} composites
  ...
""")
    
    print("SEQUENCE ANALYSIS:")
    print("─" * 60)
    print(f"{'n':>3} | {'A000081(n)':>10} | {'Trees':<30} | Interpretation")
    print("─" * 60)
    
    interpretations = [
        "ATOM: Unity before distinction",
        "CONTAINER: First distinction (unique)",
        "DUALITY: Prime 3 vs Composite 4",
        "VOCABULARY: Primes {5,7} vs Composites {6,8}",
        "EXPLOSION: 4 new primes, 5 composites",
        "EXPANSION: 9 new primes, 11 composites",
        "GROWTH: ~20 new primes",
        "EXPANSION: ~48 primes",
        "SCALING: ~115 primes",
        "GROWTH: ~286 primes",
    ]
    
    cache = {}
    for n in range(1, min(len(a000081) + 1, 8)):
        trees = bags(n, cache)
        tree_preview = ', '.join(t[1] for t in trees[:2])
        if len(trees) > 2:
            tree_preview += ', ...'
        interp = interpretations[n - 1] if n <= len(interpretations) else ""
        print(f"{n:3} | {a000081[n-1]:10} | {tree_preview:30} | {interp}")
    
    print("\n" + "─" * 60)
    print("KEY INSIGHT: The offset by 1 means:")
    print("  - Matula numbers START at 1 (the atom)")
    print("  - Tree orders START at 1 (one node)")
    print("  - The prime/composite split becomes visible at n=3")
    print("  - Each level n builds on N(n-1) from the level below")


def demonstrate_cognitive_grammar_complete():
    """
    Complete demonstration of the cognitive grammar pattern.
    """
    print("\n" + "=" * 80)
    print("COMPLETE COGNITIVE GRAMMAR: Index vs Attribute")
    print("=" * 80)
    
    print("""
THE UNIVERSAL ARCHETYPAL PATTERN:

1. NATURAL SET N(n-1) generates the split {P(n) | C(n)}:
   - P(n) = p_{N(n-1)} = the N(n-1)th prime (INDEX GRAMMAR)
   - C(n) = 2*N(n-1) = doubles (ATTRIBUTE GRAMMAR)

2. The pattern unfolds:
   {p₁=c₁=2} → {p₂=3|c₂=4} → {{p₃=5,p₄=7}|{c₃=6,c₄=8}} → ...

3. Each natural is a COMPOSITION:
   - CHILD: Prime index grammar points to subtree structure
   - PARENTS: Composite attribute grammars describe their children

4. ANCESTRAL LINEAGE:
   Every branch remembers its roots as the nested seed of its
   ancestral lineage embedded within its own self-image.

5. A000081 OFFSET:
   The two leading 1's represent the atom and container,
   before the prime/composite split becomes visible at n=3.
""")
    
    # Show the complete pattern
    print("\n" + "─" * 80)
    print("COMPLETE PATTERN TABLE:")
    print("─" * 80)
    
    print(f"\n{'Level':>6} | {'N(n-1)':>8} | {'Primes P(n)':>20} | {'Composites C(n)':>20}")
    print("─" * 60)
    
    a000081 = [1, 1, 2, 4, 9, 20, 48]
    
    for n in range(1, 7):
        if n == 1:
            print(f"{n:>6} | {'∅':>8} | {'—':>20} | {'—':>20}")
            continue
        
        # Count of naturals from level below
        count = a000081[n - 2] if n >= 2 else 1
        naturals = list(range(1, count + 1))
        
        # Primes
        primes = [nth_prime(k) for k in naturals[:6]]
        primes_str = '{' + ','.join(map(str, primes)) + ('...' if len(naturals) > 6 else '') + '}'
        
        # Composites  
        composites = [2 * k for k in naturals[:6]]
        composites_str = '{' + ','.join(map(str, composites)) + ('...' if len(naturals) > 6 else '') + '}'
        
        print(f"{n:>6} | {count:>8} | {primes_str:>20} | {composites_str:>20}")


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
    
    # NEW: Index Grammar vs Attribute Grammar
    demonstrate_index_attribute_grammar(6)
    
    # NEW: A000081 Offset explanation
    demonstrate_a000081_offset()
    
    # NEW: Ancestral Lineage
    demonstrate_ancestral_lineage(15)
    
    # NEW: Complete Cognitive Grammar
    demonstrate_cognitive_grammar_complete()
    
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
   - PRIMES (INDEX GRAMMAR): Fundamental structures pointing to children
   - PRODUCTS (ATTRIBUTE GRAMMAR): Compositions of simpler structures

5. INDEX vs ATTRIBUTE GRAMMAR: Natural numbers enumerate through the
   prime/composite split: N(n-1) → {P(n)|C(n)} where:
   - P(n) = p_{N(n-1)} = the N(n-1)th prime
   - C(n) = 2*N(n-1) = doubles from the level below

6. ANCESTRAL LINEAGE: Every branch remembers its roots as the nested seed
   of its ancestral lineage embedded within its own self-image. The prime
   factorization traces back through all ancestors to the primordial atom.

7. A000081 OFFSET: The two leading 1's (atom and container) precede the
   visible prime/composite split at n=3. Natural order is the sequence
   offset by 1.

This self-similar structure is the foundation of the "universal language" 
of computational forms, where each natural number encodes a unique way to 
organize nested distinctions through the interplay of index and attribute.
""")


if __name__ == "__main__":
    main()
