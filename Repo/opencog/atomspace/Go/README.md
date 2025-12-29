# AtomSpace Go Implementation

A Go implementation of the OpenCog AtomSpace knowledge representation system.

## Overview

The AtomSpace is a hypergraph database for storing and manipulating knowledge in the form of **Atoms**. Atoms come in two varieties:

- **Nodes**: Represent concepts, entities, or values
- **Links**: Represent relationships between atoms

## Features

- Thread-safe concurrent access using Go's sync primitives
- Efficient indexing for fast node lookup
- Truth values for uncertain reasoning
- Type-safe atom types using Go's type system
- Idempotent node creation (duplicate nodes return same handle)

## Installation

```bash
go get github.com/e9-o9/ro9se/atomspace
```

## Usage

### Creating an AtomSpace

```go
import "github.com/e9-o9/ro9se/atomspace"

as := atomspace.NewAtomSpace()
```

### Adding Nodes

```go
cat := as.AddNode(atomspace.ConceptNode, "cat")
dog := as.AddNode(atomspace.ConceptNode, "dog")
mammal := as.AddNode(atomspace.ConceptNode, "mammal")
```

### Adding Links

```go
// (InheritanceLink (ConceptNode "cat") (ConceptNode "mammal"))
link1 := as.AddLink(atomspace.InheritanceLink, []atomspace.Atom{cat, mammal})

// (InheritanceLink (ConceptNode "dog") (ConceptNode "mammal"))
link2 := as.AddLink(atomspace.InheritanceLink, []atomspace.Atom{dog, mammal})
```

### Truth Values

```go
// Set truth value
tv := atomspace.NewTruthValue(0.8, 0.9) // strength=0.8, confidence=0.9
cat.SetTruthValue(tv)

// Get truth value
tv = cat.GetTruthValue()
fmt.Printf("Strength: %f, Confidence: %f\n", tv.Strength, tv.Confidence)
```

### Querying

```go
// Get node by type and name
node := as.GetNode(atomspace.ConceptNode, "cat")

// Get all atoms of a type
concepts := as.GetAtomsByType(atomspace.ConceptNode)

// Get all atoms
allAtoms := as.GetAllAtoms()

// Get atom by handle
atom := as.GetAtom(handle)
```

### Removing Atoms

```go
handle := cat.GetHandle()
as.RemoveAtom(handle)
```

## Atom Types

### Node Types
- `ConceptNode`: Represents concepts
- `PredicateNode`: Represents predicates/relations
- `VariableNode`: Represents variables in patterns
- `NumberNode`: Represents numeric values

### Link Types
- `InheritanceLink`: Represents inheritance/subclass relationships
- `SimilarityLink`: Represents similarity relationships
- `EvaluationLink`: Represents predicate evaluations
- `ListLink`: Represents ordered lists
- `AndLink`: Represents logical AND
- `OrLink`: Represents logical OR

## Architecture

### Thread Safety

The AtomSpace uses `sync.RWMutex` for concurrent access:
- Multiple readers can access simultaneously
- Writers have exclusive access
- Individual atoms have their own locks for truth value updates

### Indexing

Nodes are indexed by `(type, name)` for O(1) lookup:
```
nodeIndex: map[AtomType]map[string]Handle
```

### Handles

Each atom has a unique `Handle` (uint64) assigned sequentially. Handles are immutable and can be used to retrieve atoms efficiently.

## Testing

```bash
go test -v
```

Run benchmarks:
```bash
go test -bench=.
```

## Example: Knowledge Base

```go
package main

import (
	"fmt"
	"github.com/e9-o9/ro9se/atomspace"
)

func main() {
	as := atomspace.NewAtomSpace()
	
	// Create taxonomy
	cat := as.AddNode(atomspace.ConceptNode, "cat")
	dog := as.AddNode(atomspace.ConceptNode, "dog")
	mammal := as.AddNode(atomspace.ConceptNode, "mammal")
	animal := as.AddNode(atomspace.ConceptNode, "animal")
	
	// Create inheritance hierarchy
	as.AddLink(atomspace.InheritanceLink, []atomspace.Atom{cat, mammal})
	as.AddLink(atomspace.InheritanceLink, []atomspace.Atom{dog, mammal})
	as.AddLink(atomspace.InheritanceLink, []atomspace.Atom{mammal, animal})
	
	// Add properties with truth values
	fluffy := as.AddNode(atomspace.ConceptNode, "fluffy")
	catFluffy := as.AddLink(atomspace.InheritanceLink, []atomspace.Atom{cat, fluffy})
	catFluffy.SetTruthValue(atomspace.NewTruthValue(0.7, 0.8))
	
	fmt.Printf("AtomSpace size: %d atoms\n", as.Size())
	fmt.Println(cat.String())
	fmt.Println(catFluffy.String())
}
```

## Comparison with C++ Implementation

| Feature | Go | C++ |
|---------|-----|-----|
| Memory Management | Garbage collected | Manual/RAII |
| Concurrency | Goroutines + channels | Threads + mutexes |
| Type System | Interfaces | Templates |
| Performance | ~80% of C++ | Baseline |
| Ease of Use | High | Medium |

## Future Enhancements

- [ ] Pattern matching engine
- [ ] Query language (similar to Atomese)
- [ ] Persistence (RocksDB, PostgreSQL)
- [ ] Network protocol (gRPC)
- [ ] Attention allocation
- [ ] Probabilistic logic networks (PLN)

## Contributing

Contributions welcome! Please ensure:
- All tests pass
- Code is formatted with `gofmt`
- New features have tests
- Documentation is updated

## License

Same as parent ro9se repository.

## See Also

- [OpenCog AtomSpace Documentation](https://wiki.opencog.org/w/AtomSpace)
- [C++ Implementation](../c++)
- [Python Implementation](../Python)
- [Haskell Implementation](../Haskell)
