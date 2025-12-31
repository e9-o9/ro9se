# OpenCog AtomSpace - Rust Implementation

## Overview

This is a single-file Rust implementation of the OpenCog AtomSpace hypergraph knowledge representation system.

## Features

- **Hypergraph Database**: Store knowledge as nodes and links
- **Truth Values**: Probabilistic truth with strength and confidence
- **Memory Safety**: No garbage collection, ownership-based safety
- **Concurrent Access**: Thread-safe with Arc and RwLock
- **Zero-Cost Abstractions**: Performance without overhead

## Rust Strengths Demonstrated

- **Ownership System**: Safe memory management without GC
- **Arc and RwLock**: Shared ownership with thread-safe access
- **Trait System**: Polymorphic behavior through traits
- **Pattern Matching**: Exhaustive enum matching
- **Type Safety**: Compile-time guarantees
- **Performance**: Native code with minimal runtime

## Usage

### Compiling and Running

```bash
# Compile
rustc atomspace.rs

# Run demo
./atomspace
```

### As a Library

```rust
use atomspace::{AtomSpace, AtomType, TruthValue};

// Create AtomSpace
let atomspace = AtomSpace::new();

// Add nodes
let human = atomspace.add_node(AtomType::ConceptNode, "human".to_string());
let mortal = atomspace.add_node(AtomType::ConceptNode, "mortal".to_string());

// Add links
let inheritance = atomspace.add_link(
    AtomType::InheritanceLink,
    vec![Arc::clone(&human), Arc::clone(&mortal)]
);

// Truth values
let tv = TruthValue::new(0.95, 0.90);
inheritance.write().unwrap().set_truth_value(tv);

// Query
println!("Size: {}", atomspace.size());
if let Some(node) = atomspace.get_node(AtomType::ConceptNode, "human") {
    println!("Node: {}", node.read().unwrap().to_string());
}
```

## Core Types

### AtomType Enum
- Node types: `ConceptNode`, `PredicateNode`, `VariableNode`, `NumberNode`
- Link types: `InheritanceLink`, `SimilarityLink`, `EvaluationLink`, `ListLink`, `AndLink`, `OrLink`

### TruthValue Struct
Immutable struct with `strength` and `confidence` (both [0.0, 1.0])

### Atom Trait
Base trait for all atoms with methods:
- `handle()` - Get unique identifier
- `atom_type()` - Get atom type
- `truth_value()` - Get truth value
- `set_truth_value(tv)` - Set truth value
- `to_string()` - String representation

### Node Struct
Named node in hypergraph implementing `Atom` trait

### Link Struct
Connection between atoms with outgoing set

### AtomSpace Struct
Main hypergraph database with concurrent access

## API Reference

### Node Operations
- `add_node(type, name)` - Add/retrieve node
- `get_node(type, name)` - Get existing node

### Link Operations
- `add_link(type, outgoing)` - Add link
- `outgoing()` - Get connected atoms
- `arity()` - Get number of connections

### AtomSpace Operations
- `size()` - Number of atoms
- `is_empty()` - Check if empty
- `clear()` - Remove all atoms
- `get_atom(handle)` - Get by handle
- `get_atoms_by_type(type)` - Filter by type
- `remove_atom(handle)` - Delete atom
- `get_all_atoms()` - Get all atoms

### Convenience Methods
- `concept_node(name)` - Create concept node
- `inheritance_link(from, to)` - Create inheritance link

## Memory Management

### Ownership Model
- **Arc**: Atomic Reference Counted pointer for shared ownership
- **RwLock**: Reader-writer lock for concurrent access
- **AtomRef**: Type alias for `Arc<RwLock<dyn Atom>>`

### Thread Safety
- Multiple readers or single writer access pattern
- Lock guards ensure safe concurrent operations
- No data races or memory corruption possible

### Zero-Cost Abstractions
- Trait objects use dynamic dispatch (vtable)
- Arc has small overhead for reference counting
- RwLock has minimal contention in read-heavy workloads
- Compiler optimizations eliminate most abstraction costs

## Architecture

```
AtomSpace
├── atoms: Arc<RwLock<HashMap<Handle, AtomRef>>>
├── node_index: Arc<RwLock<HashMap<(AtomType, String), Handle>>>
└── next_handle: Arc<RwLock<Handle>>

Atom (trait)
├── Node (struct)
│   ├── handle: Handle
│   ├── atom_type: AtomType
│   ├── name: String
│   └── truth_value: TruthValue
└── Link (struct)
    ├── handle: Handle
    ├── atom_type: AtomType
    ├── outgoing: Vec<AtomRef>
    └── truth_value: TruthValue

AtomRef = Arc<RwLock<dyn Atom>>
```

## Example: Knowledge Base

```rust
// Create knowledge base
let kb = AtomSpace::new();

// Define entities
let human = kb.concept_node("human".to_string());
let mortal = kb.concept_node("mortal".to_string());
let socrates = kb.concept_node("Socrates".to_string());

// Define relationships
kb.inheritance_link(Arc::clone(&human), Arc::clone(&mortal));
kb.inheritance_link(Arc::clone(&socrates), Arc::clone(&human));

// Query
println!("Total atoms: {}", kb.size());
let concepts = kb.get_atoms_by_type(AtomType::ConceptNode);
println!("Concepts: {}", concepts.len());

// Access with read lock
for concept in &concepts {
    let atom = concept.read().unwrap();
    println!("  {}", atom.to_string());
}
```

## Performance Characteristics

- **Lock Contention**: Read-heavy workloads scale well
- **Memory Overhead**: Arc adds one pointer per shared reference
- **Atomic Operations**: Reference counting uses atomic operations
- **Cache Locality**: HashMap provides good cache behavior
- **Zero Runtime**: No GC pauses, predictable performance

## Requirements

- Rust 1.50 or later
- No external dependencies (uses only std library)

## Safety Guarantees

- **No Use-After-Free**: Ownership prevents dangling pointers
- **No Data Races**: RwLock ensures exclusive write access
- **No Memory Leaks**: Arc automatically deallocates when count reaches zero
- **No Null Pointer Dereferences**: Option type enforces null checking
- **Thread Safety**: Send + Sync traits ensure safe concurrent use

## Advanced Features

### Trait Objects
```rust
// Dynamic dispatch through trait objects
let atom_ref: AtomRef = Arc::new(RwLock::new(node));
```

### Arc Cloning
```rust
// Cheap reference counting, not deep copy
let atom_clone = Arc::clone(&atom_ref);
```

### Lock Guards
```rust
// Automatic lock release with RAII
{
    let atom = atom_ref.read().unwrap();
    println!("{}", atom.to_string());
} // Lock released here
```

## Error Handling

Uses `Result` and `Option` types for error handling:
- `Option<AtomRef>` for operations that may not find atoms
- `unwrap()` in demo code (production should handle errors properly)
- Lock poisoning handled through unwrap (can use `?` operator in production)

## License

Part of the RosettaCog project - see main repository for license details.
