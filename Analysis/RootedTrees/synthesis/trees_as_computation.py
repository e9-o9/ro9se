#!/usr/bin/env python3
"""
Trees as Computational Structures: A Demonstration

This script demonstrates how rooted trees from the List-rooted-trees problem
can be interpreted as computational structures in the Lisp bootstrapping framework.
"""

from typing import List, Tuple, Callable, Any
from dataclasses import dataclass
from enum import Enum

# ============================================================================
# PART 1: Tree Generation (from previous analysis)
# ============================================================================

def bags(n, cache={}):
    """Generate all rooted tree configurations for n nodes."""
    if not n: 
        return [(0, "")]
    upto = sum([bags(x) for x in range(n-1, 0, -1)], [])
    return [(c+1, '('+s+')') for c,s in bagchain((0, ""), n-1, upto)]

def bagchain(x, n, bb, start=0):
    """Chain together bag configurations."""
    if not n: 
        return [x]
    out = []
    for i in range(start, len(bb)):
        c, s = bb[i]
        if c <= n:
            out += bagchain((x[0] + c, x[1] + s), n-c, bb, i)
    return out


# ============================================================================
# PART 2: Tree as Data Structure
# ============================================================================

@dataclass
class Tree:
    """Represent a rooted tree as a nested structure."""
    children: List['Tree']
    
    def __repr__(self):
        if not self.children:
            return "()"
        return "(" + "".join(repr(c) for c in self.children) + ")"
    
    def size(self) -> int:
        """Count total nodes in tree."""
        return 1 + sum(c.size() for c in self.children)
    
    def depth(self) -> int:
        """Calculate maximum depth."""
        if not self.children:
            return 1
        return 1 + max(c.depth() for c in self.children)
    
    def is_linear(self) -> bool:
        """Check if tree is a linear chain (max depth)."""
        return len(self.children) <= 1


def parse_tree(s: str) -> Tree:
    """Parse parenthesis string into Tree structure."""
    if s == "()":
        return Tree([])
    
    # Remove outer parentheses
    inner = s[1:-1]
    
    # Find immediate children
    children = []
    depth = 0
    start = 0
    
    for i, char in enumerate(inner):
        if char == '(':
            depth += 1
        elif char == ')':
            depth -= 1
            if depth == 0:
                children.append(parse_tree(inner[start:i+1]))
                start = i + 1
    
    return Tree(children)


# ============================================================================
# PART 3: Tree as Lambda Calculus Term
# ============================================================================

class LambdaTerm(Enum):
    """Types of lambda calculus terms."""
    VAR = "variable"
    ABS = "abstraction"
    APP = "application"

@dataclass
class Lambda:
    """Lambda calculus term."""
    term_type: LambdaTerm
    value: Any
    
    def __repr__(self):
        if self.term_type == LambdaTerm.VAR:
            return f"x{self.value}"
        elif self.term_type == LambdaTerm.ABS:
            return f"(λx{self.value[0]}.{self.value[1]})"
        else:  # APP
            return f"({self.value[0]} {self.value[1]})"


def tree_to_lambda(tree: Tree, var_counter: List[int] = None) -> Lambda:
    """
    Convert a rooted tree to a lambda calculus term.
    
    Interpretation:
    - Empty tree () → variable
    - Single child → abstraction (lambda)
    - Multiple children → nested applications
    """
    if var_counter is None:
        var_counter = [0]
    
    if not tree.children:
        # Leaf = variable
        var_counter[0] += 1
        return Lambda(LambdaTerm.VAR, var_counter[0])
    
    if len(tree.children) == 1:
        # Single child = abstraction
        var_num = var_counter[0]
        var_counter[0] += 1
        body = tree_to_lambda(tree.children[0], var_counter)
        return Lambda(LambdaTerm.ABS, (var_num, body))
    
    # Multiple children = applications
    terms = [tree_to_lambda(c, var_counter) for c in tree.children]
    result = terms[0]
    for term in terms[1:]:
        result = Lambda(LambdaTerm.APP, (result, term))
    return result


# ============================================================================
# PART 4: Tree as Church Numeral
# ============================================================================

def tree_to_church(tree: Tree) -> int:
    """
    Interpret tree as Church numeral.
    Linear chains represent natural numbers.
    """
    if tree.is_linear():
        return tree.depth() - 1
    else:
        return None  # Not a Church numeral


def church_to_tree(n: int) -> Tree:
    """Generate Church numeral as linear tree."""
    if n == 0:
        return Tree([])
    return Tree([church_to_tree(n - 1)])


# ============================================================================
# PART 5: Tree as Combinator
# ============================================================================

class Combinator(Enum):
    """SKI combinator basis."""
    I = "identity"
    K = "constant"
    S = "substitution"
    UNKNOWN = "unknown"


def classify_combinator(tree: Tree) -> Combinator:
    """
    Classify tree as SKI combinator.
    
    I = ()           (identity)
    K = (()())       (constant - returns first arg)
    S = complex      (substitution)
    """
    if tree.size() == 1:
        return Combinator.I
    elif tree.size() == 3 and len(tree.children) == 2:
        return Combinator.K
    elif tree.size() >= 5:
        return Combinator.S
    return Combinator.UNKNOWN


# ============================================================================
# PART 6: Tree as Lisp S-Expression
# ============================================================================

def tree_to_sexp(tree: Tree, symbols: List[str] = None) -> str:
    """
    Convert tree to Lisp S-expression with symbols.
    """
    if symbols is None:
        symbols = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
    
    if not tree.children:
        if symbols:
            return symbols.pop(0)
        return "nil"
    
    children_sexp = [tree_to_sexp(c, symbols) for c in tree.children]
    return "(" + " ".join(children_sexp) + ")"


# ============================================================================
# PART 7: Tree Pattern Analysis
# ============================================================================

def analyze_tree_pattern(tree: Tree) -> dict:
    """Analyze computational properties of tree."""
    return {
        'size': tree.size(),
        'depth': tree.depth(),
        'is_linear': tree.is_linear(),
        'is_balanced': is_balanced(tree),
        'is_flat': tree.depth() == 2,
        'branching_factor': len(tree.children),
        'church_numeral': tree_to_church(tree),
        'combinator': classify_combinator(tree),
        'computational_pattern': identify_pattern(tree)
    }


def is_balanced(tree: Tree) -> bool:
    """Check if tree is balanced (all leaves at similar depth)."""
    if not tree.children:
        return True
    depths = [c.depth() for c in tree.children]
    return max(depths) - min(depths) <= 1


def identify_pattern(tree: Tree) -> str:
    """Identify the computational pattern."""
    if tree.is_linear():
        return "Sequential Composition / Pipeline"
    elif tree.depth() == 2:
        return "Multiple Arguments / Flat List"
    elif is_balanced(tree):
        return "Parallel Evaluation / Balanced Tree"
    else:
        return "Mixed Structure / Partial Application"


# ============================================================================
# PART 8: Demonstration Functions
# ============================================================================

def demonstrate_trees_as_computation(n: int):
    """Demonstrate all computational interpretations for n-node trees."""
    print(f"\n{'='*80}")
    print(f"COMPUTATIONAL INTERPRETATIONS OF {n}-NODE ROOTED TREES")
    print(f"{'='*80}\n")
    
    all_trees = bags(n)
    print(f"Total trees: {len(all_trees)}\n")
    
    for i, (count, tree_str) in enumerate(all_trees, 1):
        print(f"\n{'─'*80}")
        print(f"Tree {i}: {tree_str}")
        print(f"{'─'*80}")
        
        # Parse tree
        tree = parse_tree(tree_str)
        
        # Analyze pattern
        analysis = analyze_tree_pattern(tree)
        
        print(f"\n📊 Structural Properties:")
        print(f"   Size: {analysis['size']} nodes")
        print(f"   Depth: {analysis['depth']} levels")
        print(f"   Pattern: {analysis['computational_pattern']}")
        
        # Lambda calculus interpretation
        print(f"\n🔬 Lambda Calculus:")
        lambda_term = tree_to_lambda(tree)
        print(f"   {lambda_term}")
        
        # Church numeral (if applicable)
        if analysis['church_numeral'] is not None:
            print(f"\n🔢 Church Numeral:")
            print(f"   Represents: {analysis['church_numeral']}")
        
        # Combinator classification
        print(f"\n⚡ Combinator:")
        print(f"   Type: {analysis['combinator'].value}")
        
        # Lisp S-expression
        print(f"\n📝 Lisp S-Expression:")
        sexp = tree_to_sexp(tree, ['a', 'b', 'c', 'd', 'e'])
        print(f"   {sexp}")
        
        # Computational interpretation
        print(f"\n💡 Computational Meaning:")
        if analysis['is_linear']:
            print(f"   • Function composition: f(g(h(...)))")
            print(f"   • Pipeline processing")
            print(f"   • Iteration/recursion depth: {analysis['depth'] - 1}")
        elif analysis['is_flat']:
            print(f"   • Multi-argument function: (f a b c ...)")
            print(f"   • Flat list processing")
            print(f"   • {analysis['branching_factor']} independent arguments")
        elif analysis['is_balanced']:
            print(f"   • Parallel evaluation")
            print(f"   • Binary tree structure")
            print(f"   • Divide-and-conquer pattern")
        else:
            print(f"   • Mixed evaluation strategy")
            print(f"   • Partial application / currying")
            print(f"   • Nested contexts")


def demonstrate_church_numerals():
    """Show how Church numerals map to linear trees."""
    print(f"\n{'='*80}")
    print(f"CHURCH NUMERALS AS ROOTED TREES")
    print(f"{'='*80}\n")
    
    for n in range(6):
        tree = church_to_tree(n)
        print(f"Church {n}: {tree}")
        print(f"   Depth: {tree.depth()}")
        print(f"   Interpretation: λf.λx.f^{n}(x) (apply f {n} times)\n")


def demonstrate_combinators():
    """Show SKI combinators as trees."""
    print(f"\n{'='*80}")
    print(f"SKI COMBINATORS AS ROOTED TREES")
    print(f"{'='*80}\n")
    
    # I combinator
    i_tree = parse_tree("()")
    print(f"I (Identity): {i_tree}")
    print(f"   λx.x")
    print(f"   Returns its argument unchanged\n")
    
    # K combinator
    k_tree = parse_tree("(()())")
    print(f"K (Constant): {k_tree}")
    print(f"   λx.λy.x")
    print(f"   Returns first argument, ignores second\n")
    
    # Example S-like structure
    print(f"S (Substitution): More complex structures")
    print(f"   λf.λg.λx.(f x (g x))")
    print(f"   Applies f to x and (g x)\n")


def demonstrate_lisp_forms():
    """Show how trees map to Lisp forms."""
    print(f"\n{'='*80}")
    print(f"ROOTED TREES AS LISP FORMS")
    print(f"{'='*80}\n")
    
    examples = [
        ("()", "nil", "Empty list"),
        ("(())", "(nil)", "List containing nil"),
        ("(()())", "(a b)", "Two-element list"),
        ("((()))", "((a))", "Nested list"),
        ("((())())", "((a) b)", "Mixed structure"),
        ("(()()())", "(a b c)", "Three-element list"),
    ]
    
    for tree_str, lisp_form, description in examples:
        tree = parse_tree(tree_str)
        print(f"Tree: {tree_str:15} → Lisp: {lisp_form:15} ({description})")


# ============================================================================
# MAIN DEMONSTRATION
# ============================================================================

def main():
    print("="*80)
    print("ROOTED TREES AS COMPUTATIONAL STRUCTURES")
    print("Demonstrating the connection between tree enumeration and Lisp bootstrapping")
    print("="*80)
    
    # Show Church numerals
    demonstrate_church_numerals()
    
    # Show combinators
    demonstrate_combinators()
    
    # Show Lisp forms
    demonstrate_lisp_forms()
    
    # Detailed analysis for n=4
    demonstrate_trees_as_computation(4)
    
    # Summary
    print(f"\n{'='*80}")
    print("SUMMARY")
    print(f"{'='*80}\n")
    print("""
The rooted tree enumeration problem explores the space of all possible
computational structures. Each tree represents:

1. A unique way to organize nested distinctions (Spencer-Brown)
2. A lambda calculus term (functional programming)
3. A potential Lisp S-expression (code/data)
4. A combinator structure (computational primitive)
5. An evaluation strategy (execution order)

The A000081 sequence counts these fundamental computational patterns.
As n grows, the number of distinct patterns grows exponentially,
reflecting the inherent complexity of hierarchical organization.

This is why Lisp is powerful: it operates directly on these tree
structures, treating code and data uniformly as rooted trees.
""")


if __name__ == "__main__":
    main()
