#!/usr/bin/env python3
"""
Analyze n=10 trees to identify:
1. Which trees descend from n=9 parents
2. Which trees are special combinations
3. The partition structure of each tree
4. The complete compositional taxonomy

Expected: 719 trees at n=10 (A000081 sequence: 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, ...)
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
    
    # A tree is widened from parent if adding one atom sibling at root transforms parent to child
    # This happens when: non-atom children match exactly (new atom added),
    # or when the atom count increased by 1 (new atom added to existing atoms)
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

# Generate trees for n=2 through n=10
trees_2 = [t[1] for t in bags(2)]
trees_3 = [t[1] for t in bags(3)]
trees_4 = [t[1] for t in bags(4)]
trees_5 = [t[1] for t in bags(5)]
trees_6 = [t[1] for t in bags(6)]
trees_7 = [t[1] for t in bags(7)]
trees_8 = [t[1] for t in bags(8)]
trees_9 = [t[1] for t in bags(9)]
trees_10 = [t[1] for t in bags(10)]

print("=" * 80)
print("N=10 TREE ANALYSIS")
print("=" * 80)
print()
print(f"Number of trees at n=10: {len(trees_10)} (expected: 719)")
print()

# Analyze each n=10 tree
tree_data = []

for i, tree in enumerate(trees_10, 1):
    children = analyze_partition(tree)
    child_sizes = [count_nodes(c) for c in children]
    depth = tree_depth(tree)
    width = tree_width(tree)
    
    # Find parent
    parent, operation = find_parent(tree, trees_9)
    
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
    
    # Identify special combinations based on partition of n-1 = 9
    sorted_sizes = sorted(child_sizes, reverse=True)
    
    if len(children) == 2:
        c1_size, c2_size = sorted_sizes
        if c1_size == c2_size and c1_size > 1:
            tree_info['special'].append(f"Symmetric pair: 2 × n={c1_size}")
        if sorted_sizes == [7, 2]:
            tree_info['special'].append("n=7 + n=2 combination")
        if sorted_sizes == [6, 3]:
            tree_info['special'].append("n=6 + n=3 combination")
        if sorted_sizes == [5, 4]:
            tree_info['special'].append("n=5 + n=4 combination")
    
    elif len(children) == 3:
        if sorted_sizes == [5, 2, 2]:
            tree_info['special'].append("n=5 + n=2 + n=2 combination")
        elif sorted_sizes == [4, 3, 2]:
            tree_info['special'].append("n=4 + n=3 + n=2 combination")
        elif sorted_sizes == [3, 3, 3]:
            tree_info['special'].append("Ternary symmetric: 3 × n=3")
        elif sorted_sizes == [3, 3, 2]:
            # This could be from n=9+1 (duality pair + widen), not a new special for n=10
            pass
    
    elif len(children) == 4:
        if sorted_sizes == [3, 2, 2, 2]:
            tree_info['special'].append("n=3 + 3×n=2 combination")
        elif sorted_sizes == [2, 2, 2, 2]:
            # This is from n=9+1 (quaternary + widen), not a new special for n=10
            pass
    
    elif len(children) == 5:
        if sorted_sizes == [2, 2, 2, 2, 2]:
            tree_info['special'].append("Quinary symmetric: 5 × n=2")
    
    tree_data.append(tree_info)

# Print detailed analysis
for info in tree_data:
    print(f"Tree {info['index']:3d}: {info['tree']}")
    print(f"  Partition: {info['child_sizes']} (children sizes)")
    print(f"  Children: {info['children']}")
    print(f"  Depth: {info['depth']}, Width: {info['width']}")
    
    if info['parent']:
        parent_idx = trees_9.index(info['parent']) + 1 if info['parent'] in trees_9 else '?'
        print(f"  Parent: n=9 Tree {parent_idx} `{info['parent']}`")
        print(f"  Operation: {info['operation']}")
    else:
        print(f"  Parent: SPECIAL COMBINATION (no direct n=9 parent)")
    
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

print(f"Trees with n=9 parents: {len(with_parents)}")
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

# For n=10, special combinations come from partitions of 9 into parts >= 2
# [7,2], [6,3], [5,4], [5,2,2], [4,3,2], [3,3,3], [3,2,2,2], [2,2,2,2,2]
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
for parent in trees_9:
    children = parent_children.get(parent, [])
    parent_idx = trees_9.index(parent) + 1
    child_counts[parent_idx] = len(children)
    if len(children) > 0:
        print(f"n=9 Tree {parent_idx:3d}: {parent}")
        print(f"  → {len(children)} children via:", end=" ")
        ops_for_parent = [op for _, _, op in children]
        print(f"{', '.join(set(ops_for_parent))}")

print()
print(f"Total direct extensions: {sum(child_counts.values())}")
print(f"Total special combinations: {len(special_combinations)}")
print(f"Total n=10 trees: {sum(child_counts.values()) + len(special_combinations)}")

# Verify we get 719
print()
print("=" * 80)
print("VERIFICATION")
print("=" * 80)
print(f"Generated trees: {len(trees_10)}")
print(f"Expected (A000081): 719")
print(f"Match: {'✓' if len(trees_10) == 719 else '✗'}")

# Export key statistics for documentation
print()
print("=" * 80)
print("STATISTICS FOR DOCUMENTATION")
print("=" * 80)
print()
print(f"Total Trees: {len(trees_10)}")
print(f"Direct Extensions (Nest): {ops.get('Nest', 0)}")
print(f"Direct Extensions (Widen): {ops.get('Widen', 0)}")
print(f"Special Combinations: {len(special_combinations)}")
print()
print("Partition Type Counts:")
for partition, trees in sorted(partition_types.items(), key=lambda x: (len(x[0]), -x[0][0])):
    print(f"  {list(partition)}: {len(trees)}")
