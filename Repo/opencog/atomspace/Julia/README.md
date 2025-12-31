# OpenCog AtomSpace - Julia Implementation

## Overview

This is a single-file Julia implementation of the OpenCog AtomSpace hypergraph knowledge representation system.

## Features

- **Hypergraph Database**: Store knowledge as nodes and links
- **Truth Values**: Probabilistic truth with strength and confidence
- **Type System**: Multiple dispatch for polymorphic atom operations
- **Thread-Safe**: Lock-based concurrency protection
- **Performance**: Type-stable design for JIT optimization

## Julia Strengths Demonstrated

- **Multiple Dispatch**: Polymorphic operations based on type signatures
- **Type Hierarchy**: Abstract `Atom` type with concrete `Node` and `Link` subtypes
- **Parametric Types**: Generic programming with type parameters
- **Mathematical Notation**: Clean syntax for truth values and operations
- **Performance**: JIT compilation to native code for speed

## Usage

### Running the Demo

```bash
julia atomspace.jl
```

### As a Library

```julia
include("atomspace.jl")

# Create AtomSpace
as = AtomSpace()

# Add nodes
human = add_node!(as, CONCEPT_NODE, "human")
mortal = add_node!(as, CONCEPT_NODE, "mortal")

# Add links
inheritance = add_link!(as, INHERITANCE_LINK, [human, mortal])

# Truth values
tv = TruthValue(0.95, 0.90)
set_truth_value!(inheritance, tv)

# Query
println("Size: $(size(as))")
println("Node: $(get_node(as, CONCEPT_NODE, "human"))")
```

## Core Types

### AtomType Enum
- Node types: `CONCEPT_NODE`, `PREDICATE_NODE`, `VARIABLE_NODE`, `NUMBER_NODE`
- Link types: `INHERITANCE_LINK`, `SIMILARITY_LINK`, `EVALUATION_LINK`, `LIST_LINK`, `AND_LINK`, `OR_LINK`

### TruthValue
Immutable struct with `strength` and `confidence` (both [0.0, 1.0])

### Atom Hierarchy
- `Atom` (abstract type)
  - `Node` - Named node in hypergraph
  - `Link` - Connection between atoms

### AtomSpace
Main hypergraph database with thread-safe operations

## API Reference

### Node Operations
- `add_node!(as, type, name)` - Add/retrieve node
- `get_node(as, type, name)` - Get existing node

### Link Operations
- `add_link!(as, type, outgoing)` - Add link
- `outgoing(link)` - Get connected atoms
- `arity(link)` - Get number of connections

### AtomSpace Operations
- `size(as)` / `length(as)` - Number of atoms
- `isempty(as)` - Check if empty
- `empty!(as)` - Clear all atoms
- `get_atom(as, handle)` - Get by handle
- `get_atoms_by_type(as, type)` - Filter by type
- `remove_atom!(as, handle)` - Delete atom

### Convenience Functions
- `concept_node(as, name)` - Create concept node
- `inheritance_link(as, from, to)` - Create inheritance link

## Performance Characteristics

- **JIT Compilation**: Julia compiles to native code on first run
- **Type Stability**: Careful type design enables compiler optimizations
- **Lock-Based Concurrency**: Thread-safe with minimal overhead
- **Efficient Indexing**: Hash-based lookup for nodes

## Requirements

- Julia 1.6 or later
- No external dependencies (uses only Julia standard library)

## Architecture

```
AtomSpace
├── atoms: Dict{Handle, Atom}
├── node_index: Dict{(Type, String), Handle}
└── lock: ReentrantLock

Atom (abstract)
├── Node
│   ├── handle: Handle
│   ├── atom_type: AtomType
│   ├── name: String
│   └── truth_value: TruthValue
└── Link
    ├── handle: Handle
    ├── atom_type: AtomType
    ├── outgoing: Vector{Atom}
    └── truth_value: TruthValue
```

## Example: Knowledge Base

```julia
# Create knowledge base
kb = AtomSpace()

# Define entities
human = concept_node(kb, "human")
mortal = concept_node(kb, "mortal")
socrates = concept_node(kb, "Socrates")
greek = concept_node(kb, "Greek")

# Define relationships
inheritance_link(kb, human, mortal)
inheritance_link(kb, socrates, human)
inheritance_link(kb, socrates, greek)

# Query
println("Total atoms: $(size(kb))")
concepts = get_atoms_by_type(kb, CONCEPT_NODE)
println("Concepts: $(length(concepts))")
```

## License

Part of the RosettaCog project - see main repository for license details.
