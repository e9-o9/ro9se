#!/usr/bin/env python3
"""
Analyze n=8 trees to identify:
1. Which trees descend from n=7 parents
2. Which trees are special combinations
3. The partition structure of each tree
4. The complete compositional taxonomy

Expected: 115 trees at n=8 (A000081 sequence: 1, 1, 2, 4, 9, 20, 48, 115, ...)
"""

def bags(n):
    """Generate all unlabeled rooted trees with n nodes using bag chain algorithm"""
    if not n: 
        return [(0, '')]
    upto = sum([bags(x) for x in range(n-1, 0, -1)], [])
    return [(c+1, '('+s+')') for c,s in bagchain((0, ''), n-1, upto)]

def bagchain(x, n, bb, start=0):
    """Chain bags to form trees"""
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

def tree_depth(tree_str):
    """Calculate the maximum depth of a tree"""
    max_depth = 0
    current_depth = 0
    for char in tree_str:
        if char == '(':
            current_depth += 1
            max_depth = max(max_depth, current_depth)
        elif char == ')':
            current_depth -= 1
    return max_depth

def tree_width(tree_str):
    """Calculate the width of a tree at root level (number of children)"""
    children = analyze_partition(tree_str)
    return len(children) if children else 0

def is_nested_from(child, parent):
    """Check if child is formed by nesting parent: child = '(' + parent + ')'"""
    return child == '(' + parent + ')'

def is_widened_from(child, parent):
    """Check if child is formed by widening parent (adding an atom sibling)"""
    child_children = analyze_partition(child)
    parent_children = analyze_partition(parent)
    
    if len(child_children) != len(parent_children) + 1:
        return False
    
    # Child should have all parent's children plus one '()'
    if '()' not in child_children:
        return False
    
    child_without_atom = [c for c in child_children if c != '()']
    parent_without_atom = [c for c in parent_children if c != '()']
    child_atom_count = len([c for c in child_children if c == '()'])
    parent_atom_count = len([c for c in parent_children if c == '()'])
    
    return sorted(child_without_atom) == sorted(parent_without_atom) or \
           child_atom_count == parent_atom_count + 1

def is_juxtaposed_from(child, parent):
    """Check if child is formed by juxtaposing parent with an atom at root"""
    child_children = analyze_partition(child)
    
    if len(child_children) != 2:
        return False
    
    # One child should be parent, the other should be '()'
    return (parent in child_children and '()' in child_children)

def find_parent(child, parents):
    """Find which parent tree the child descends from and by what operation"""
    for parent in parents:
        if is_nested_from(child, parent):
            return parent, "Nest"
        if is_widened_from(child, parent):
            return parent, "Widen"
        if is_juxtaposed_from(child, parent):
            return parent, "Juxtapose"
    return None, None

# Generate trees for n=2 through n=8
trees_2 = [t[1] for t in bags(2)]
trees_3 = [t[1] for t in bags(3)]
trees_4 = [t[1] for t in bags(4)]
trees_5 = [t[1] for t in bags(5)]
trees_6 = [t[1] for t in bags(6)]
trees_7 = [t[1] for t in bags(7)]
trees_8 = [t[1] for t in bags(8)]

print("=" * 80)
print("N=8 TREE ANALYSIS")
print("=" * 80)
print()
print(f"Number of trees at n=8: {len(trees_8)} (expected: 115)")
print()

# Analyze each n=8 tree
tree_data = []

for i, tree in enumerate(trees_8, 1):
    children = analyze_partition(tree)
    child_sizes = [count_nodes(c) for c in children]
    depth = tree_depth(tree)
    width = tree_width(tree)
    
    # Find parent
    parent, operation = find_parent(tree, trees_7)
    
    tree_info = {
        'index': i,
        'tree': tree,
        'children': children,
        'child_sizes': child_sizes,
        'depth': depth,
        'width': width,
        'parent': parent,
        'operation': operation,
        'special': []
    }
    
    # Identify special combinations based on partition of n-1 = 7
    sorted_sizes = sorted(child_sizes, reverse=True)
    
    if len(children) == 2:
        c1_size, c2_size = sorted_sizes
        if c1_size == c2_size and c1_size > 1:
            tree_info['special'].append(f"Symmetric pair: 2 × n={c1_size}")
        if sorted_sizes == [5, 2]:
            tree_info['special'].append("n=5 + n=2 combination")
        if sorted_sizes == [4, 3]:
            tree_info['special'].append("n=4 + n=3 combination")
    
    elif len(children) == 3:
        if sorted_sizes == [3, 2, 2]:
            tree_info['special'].append("n=3 + n=2 + n=2 combination")
        elif sorted_sizes == [2, 2, 2] and all(s == 2 for s in sorted_sizes):
            # This is actually n=6+1, not special for n=8
            pass
        elif sorted_sizes == [3, 3, 1]:
            tree_info['special'].append("n=3 + n=3 + atom (derived from special)")
    
    elif len(children) == 4:
        if sorted_sizes == [2, 2, 2, 1]:
            tree_info['special'].append("3×n=2 + atom (derived from ternary)")
    
    tree_data.append(tree_info)

# Print detailed analysis
for info in tree_data:
    print(f"Tree {info['index']:3d}: {info['tree']}")
    print(f"  Partition: {info['child_sizes']} (children sizes)")
    print(f"  Children: {info['children']}")
    print(f"  Depth: {info['depth']}, Width: {info['width']}")
    
    if info['parent']:
        parent_idx = trees_7.index(info['parent']) + 1 if info['parent'] in trees_7 else '?'
        print(f"  Parent: n=7 Tree {parent_idx} `{info['parent']}`")
        print(f"  Operation: {info['operation']}")
    else:
        print(f"  Parent: SPECIAL COMBINATION (no direct n=7 parent)")
    
    if info['special']:
        for s in info['special']:
            print(f"  → SPECIAL: {s}")
    
    print()

# Summary statistics
print("=" * 80)
print("SUMMARY STATISTICS")
print("=" * 80)
print()

# Trees with parents vs special combinations
with_parents = [t for t in tree_data if t['parent']]
special_combinations = [t for t in tree_data if not t['parent']]

print(f"Trees with n=7 parents: {len(with_parents)}")
print(f"Special combinations: {len(special_combinations)}")
print()

# Operation distribution
ops = {}
for t in with_parents:
    op = t['operation']
    ops[op] = ops.get(op, 0) + 1

print("Operation Distribution:")
for op, count in sorted(ops.items()):
    print(f"  {op}: {count}")
print()

# Depth distribution
depths = {}
for t in tree_data:
    d = t['depth']
    depths[d] = depths.get(d, 0) + 1

print("Depth Distribution:")
for d in sorted(depths.keys()):
    print(f"  Depth {d}: {depths[d]} trees")
print()

# Width distribution
widths = {}
for t in tree_data:
    w = t['width']
    widths[w] = widths.get(w, 0) + 1

print("Width Distribution:")
for w in sorted(widths.keys()):
    print(f"  Width {w}: {widths[w]} trees")
print()

# Special combinations detailed
print("=" * 80)
print("SPECIAL COMBINATIONS ANALYSIS")
print("=" * 80)
print()

for info in special_combinations:
    print(f"Tree {info['index']:3d}: {info['tree']}")
    print(f"  Partition: {info['child_sizes']}")
    print(f"  Children: {info['children']}")
    if info['special']:
        for s in info['special']:
            print(f"  Type: {s}")
    print()

# Categorize special combinations by partition type
print("=" * 80)
print("SPECIAL COMBINATIONS BY PARTITION TYPE")
print("=" * 80)
print()

# For n=8, special combinations come from partitions of 7 into parts >= 2
# [5,2], [4,3], [3,2,2]
partition_types = {}
for info in special_combinations:
    sorted_sizes = tuple(sorted(info['child_sizes'], reverse=True))
    if sorted_sizes not in partition_types:
        partition_types[sorted_sizes] = []
    partition_types[sorted_sizes].append(info)

for partition, trees in sorted(partition_types.items(), key=lambda x: (len(x[0]), -x[0][0])):
    print(f"Partition {list(partition)}: {len(trees)} trees")
    for info in trees:
        print(f"  Tree {info['index']:3d}: {info['tree']}")
        print(f"    Children: {info['children']}")
    print()

# Parent-child relationship summary
print("=" * 80)
print("PARENT-CHILD RELATIONSHIPS (Summary)")
print("=" * 80)
print()

# Group by parent
from collections import defaultdict
parent_children = defaultdict(list)
for t in tree_data:
    if t['parent']:
        parent_children[t['parent']].append((t['index'], t['tree'], t['operation']))

# Count children per parent
child_counts = {}
for parent in trees_7:
    children = parent_children.get(parent, [])
    parent_idx = trees_7.index(parent) + 1
    child_counts[parent_idx] = len(children)
    if len(children) > 0:
        print(f"n=7 Tree {parent_idx:2d}: {parent}")
        print(f"  → {len(children)} children via:", end=" ")
        ops_for_parent = [op for _, _, op in children]
        print(f"{', '.join(set(ops_for_parent))}")

print()
print(f"Total direct extensions: {sum(child_counts.values())}")
print(f"Total special combinations: {len(special_combinations)}")
print(f"Total n=8 trees: {sum(child_counts.values()) + len(special_combinations)}")

# Verify we get 115
print()
print("=" * 80)
print("VERIFICATION")
print("=" * 80)
print(f"Generated trees: {len(trees_8)}")
print(f"Expected (A000081): 115")
print(f"Match: {'✓' if len(trees_8) == 115 else '✗'}")
