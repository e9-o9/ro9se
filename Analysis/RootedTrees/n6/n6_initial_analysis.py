#!/usr/bin/env python3.11
"""
Analyze n=6 trees to identify:
1. Which trees descend from n=5 parents
2. Which trees are special combinations
3. The partition structure of each tree
"""

def bags(n):
    if not n: 
        return [(0, '')]
    upto = sum([bags(x) for x in range(n-1, 0, -1)], [])
    return [(c+1, '('+s+')') for c,s in bagchain((0, ''), n-1, upto)]

def bagchain(x, n, bb, start=0):
    if not n: 
        return [x]
    out = []
    for i in range(start, len(bb)):
        c, s = bb[i]
        if c <= n:
            out += bagchain((x[0] + c, x[1] + s), n-c, bb, i)
    return out

def analyze_partition(tree_str):
    """Analyze the partition structure of a tree by looking at its children"""
    # Remove outer parentheses
    if not tree_str or tree_str == '()':
        return []
    
    inner = tree_str[1:-1]
    if not inner:
        return []
    
    # Parse children
    children = []
    depth = 0
    current = ''
    
    for char in inner:
        if char == '(':
            depth += 1
            current += char
        elif char == ')':
            depth -= 1
            current += char
            if depth == 0:
                children.append(current)
                current = ''
    
    return children

def count_nodes(tree_str):
    """Count nodes in a tree (number of '(' characters)"""
    return tree_str.count('(')

def can_be_extended_from(child, parent, operation):
    """Check if child can be formed from parent via operation"""
    # This is a simplified check
    child_nodes = count_nodes(child)
    parent_nodes = count_nodes(parent)
    
    if child_nodes != parent_nodes + 1:
        return False
    
    # Check specific operations
    if operation == "deepen":
        # Child should have parent nested deeper
        return child.startswith('(') and child[1:-1].startswith(parent[:-1])
    elif operation == "nest":
        # Child should wrap parent
        return child == '(' + parent + ')'
    elif operation == "juxtapose":
        # Child should have parent + () at root level
        children = analyze_partition(child)
        return len(children) == 2 and (parent in children or '()' in children)
    elif operation == "widen":
        # Child should have one more sibling than parent
        child_children = analyze_partition(child)
        parent_children = analyze_partition(parent)
        return len(child_children) == len(parent_children) + 1
    
    return False

# Generate trees for n=3, n=4, n=5, n=6
trees_3 = [t[1] for t in bags(3)]
trees_4 = [t[1] for t in bags(4)]
trees_5 = [t[1] for t in bags(5)]
trees_6 = [t[1] for t in bags(6)]

print("=" * 80)
print("N=6 TREE ANALYSIS")
print("=" * 80)
print()

# Analyze each n=6 tree
for i, tree in enumerate(trees_6, 1):
    print(f"Tree {i:2d}: {tree}")
    
    # Get partition structure
    children = analyze_partition(tree)
    child_sizes = [count_nodes(c) for c in children]
    
    print(f"  Partition: {child_sizes} (children sizes)")
    print(f"  Children: {children}")
    
    # Check if it's a special combination
    if len(children) == 2:
        c1_size, c2_size = child_sizes
        # Check for symmetric combinations
        if c1_size == c2_size:
            print(f"  → SYMMETRIC COMBINATION: Two {c1_size}-node subtrees")
        # Check for specific combinations
        if c1_size == 2 and c2_size == 3:
            print(f"  → SPECIAL: n=2 + n=3 combination")
        elif c1_size == 3 and c2_size == 2:
            print(f"  → SPECIAL: n=3 + n=2 combination")
    
    elif len(children) == 3:
        if all(s == children[0] for s in child_sizes):
            print(f"  → TRIPLE SYMMETRIC: Three {child_sizes[0]}-node subtrees")
        else:
            print(f"  → ASYMMETRIC TRIPLE")
    
    print()

print("=" * 80)
print("SPECIAL COMBINATIONS SUMMARY")
print("=" * 80)
print()

# Identify special combinations
special = []

for i, tree in enumerate(trees_6, 1):
    children = analyze_partition(tree)
    child_sizes = [count_nodes(c) for c in children]
    
    # Symmetric pairs
    if len(children) == 2 and child_sizes[0] == child_sizes[1]:
        if child_sizes[0] > 1:  # Not just (()())
            special.append((i, tree, f"Symmetric pair: 2 × n={child_sizes[0]}", children))
    
    # n=2 + n=3 combinations
    if len(children) == 2 and sorted(child_sizes) == [2, 3]:
        special.append((i, tree, "n=2 + n=3 combination", children))
    
    # Triple symmetric
    if len(children) == 3 and len(set(child_sizes)) == 1 and child_sizes[0] > 1:
        special.append((i, tree, f"Triple symmetric: 3 × n={child_sizes[0]}", children))

print(f"Found {len(special)} special combinations:\n")

for idx, tree, desc, children in special:
    print(f"Tree {idx:2d}: {tree}")
    print(f"  Type: {desc}")
    print(f"  Children: {children}")
    print()
