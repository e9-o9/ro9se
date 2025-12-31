# OpenCog AtomSpace - Scheme Implementation

## Overview

This is a single-file Scheme implementation of the OpenCog AtomSpace hypergraph knowledge representation system.

## Features

- **Hypergraph Database**: Store knowledge as nodes and links
- **Truth Values**: Probabilistic truth with strength and confidence
- **S-Expressions**: Natural representation for hypergraph structures
- **Functional Programming**: Immutable data structures and pure functions
- **Closures**: Encapsulation of state

## Scheme Strengths Demonstrated

- **S-Expressions**: Code and data have the same representation
- **First-Class Functions**: Functions are values
- **Closures**: Functions capture their environment
- **Homoiconicity**: Code is data, enabling powerful metaprogramming
- **Record Types**: Structured data with type checking
- **Tail Recursion**: Efficient iterative processes

## Usage

### Running the Demo

```bash
# With Racket
racket atomspace.scm

# With Guile
guile atomspace.scm

# With Chez Scheme
scheme --script atomspace.scm
```

### As a Library

```scheme
(load "atomspace.scm")

;; Create AtomSpace
(define as (make-atomspace))

;; Add nodes
(define human (atomspace-add-node! as CONCEPT-NODE "human"))
(define mortal (atomspace-add-node! as CONCEPT-NODE "mortal"))

;; Add links
(define inheritance (atomspace-add-link! as INHERITANCE-LINK (list human mortal)))

;; Truth values
(define tv (make-truth-value 0.95 0.90))
(atom-set-truth-value! inheritance tv)

;; Query
(display (atomspace-size as))
(newline)
(display (node->string human))
(newline)
```

## Core Types

### Atom Types
- Node types: `CONCEPT-NODE`, `PREDICATE-NODE`, `VARIABLE-NODE`, `NUMBER-NODE`
- Link types: `INHERITANCE-LINK`, `SIMILARITY-LINK`, `EVALUATION-LINK`, `LIST-LINK`, `AND-LINK`, `OR-LINK`

### Truth Value
Record type with `strength` and `confidence` (both [0.0, 1.0])

### Node
Record type representing a named node in the hypergraph

### Link
Record type representing connections between atoms

### AtomSpace
Record type for the main hypergraph database

## API Reference

### Node Operations
- `(atomspace-add-node! as type name)` - Add/retrieve node
- `(atomspace-get-node as type name)` - Get existing node
- `(node->string node)` - Convert to string representation

### Link Operations
- `(atomspace-add-link! as type outgoing)` - Add link
- `(link-outgoing link)` - Get connected atoms
- `(link-arity link)` - Get number of connections
- `(link->string link)` - Convert to string representation

### Atom Operations (Polymorphic)
- `(atom? x)` - Check if value is an atom
- `(atom-handle atom)` - Get unique identifier
- `(atom-type atom)` - Get atom type
- `(atom-truth-value atom)` - Get truth value
- `(atom-set-truth-value! atom tv)` - Set truth value
- `(atom->string atom)` - String representation

### AtomSpace Operations
- `(atomspace-size as)` - Number of atoms
- `(atomspace-empty? as)` - Check if empty
- `(atomspace-clear! as)` - Remove all atoms
- `(atomspace-get-atom as handle)` - Get by handle
- `(atomspace-get-atoms-by-type as type)` - Filter by type
- `(atomspace-remove-atom! as handle)` - Delete atom
- `(atomspace-get-all-atoms as)` - Get all atoms

### Convenience Functions
- `(concept-node as name)` - Create concept node
- `(inheritance-link as from to)` - Create inheritance link

## Architecture

```
AtomSpace (record)
├── atoms: hash-table (handle -> atom)
├── node-index: hash-table ((type . name) -> handle)
└── next-handle: integer

Atom (polymorphic via predicates)
├── Node (record)
│   ├── handle
│   ├── atom-type
│   ├── name
│   └── truth-value
└── Link (record)
    ├── handle
    ├── atom-type
    ├── outgoing (list of atoms)
    └── truth-value
```

## Example: Knowledge Base

```scheme
;; Create knowledge base
(define kb (make-atomspace))

;; Define entities
(define human (concept-node kb "human"))
(define mortal (concept-node kb "mortal"))
(define socrates (concept-node kb "Socrates"))
(define greek (concept-node kb "Greek"))

;; Define relationships
(inheritance-link kb human mortal)
(inheritance-link kb socrates human)
(inheritance-link kb socrates greek)

;; Query
(display "Total atoms: ")
(display (atomspace-size kb))
(newline)

(define concepts (atomspace-get-atoms-by-type kb CONCEPT-NODE))
(display "Concepts: ")
(display (length concepts))
(newline)

;; Print all concepts
(for-each (lambda (c)
            (display "  ")
            (display (atom->string c))
            (newline))
          concepts)
```

## Scheme-Specific Features

### S-Expression Representation
```scheme
;; Atoms are naturally represented as S-expressions
(ConceptNode "human")                      ; Node
(InheritanceLink                           ; Link
  (ConceptNode "human")
  (ConceptNode "mortal"))
```

### Record Types
```scheme
;; Define structured data types
(define-record-type node
  (make-node-impl handle atom-type name truth-value)
  node?
  (handle node-handle node-set-handle!)
  (atom-type node-atom-type)
  (name node-name)
  (truth-value node-truth-value node-set-truth-value!))
```

### Polymorphic Operations
```scheme
;; Multiple implementations dispatch based on predicates
(define (atom->string a)
  (cond
    ((node? a) (node->string a))
    ((link? a) (link->string a))
    (else (error "Not an atom" a))))
```

### Tail Recursion
```scheme
;; Efficient iteration through tail calls
(define (atomspace-get-atoms-by-type as atom-type)
  (let ((type-name (atom-type-name atom-type)))
    (filter (lambda (atom)
              (equal? (atom-type-name (atom-type atom)) type-name))
            (atomspace-get-all-atoms as))))
```

### Closures
```scheme
;; Functions capture their environment
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))
```

## Performance Characteristics

- **Tail Call Optimization**: Iterative processes don't grow stack
- **Hash Tables**: O(1) average case lookup
- **Garbage Collection**: Automatic memory management
- **Functional Style**: Encourages referential transparency

## Requirements

Compatible with most Scheme implementations:
- **Racket**: Full R6RS/R7RS support
- **Guile**: GNU's Scheme implementation
- **Chez Scheme**: Fast native code compiler
- **MIT Scheme**: Classic implementation
- **Chicken Scheme**: Compiles to C

## Compatibility Notes

This implementation uses standard Scheme features:
- `define-record-type` from SRFI-9
- Basic hash tables (custom implementation provided)
- Standard list operations

For full compatibility, ensure your Scheme implementation supports:
- SRFI-9 (Defining Record Types)
- SRFI-19 (Time Data Types) for `current-time`

## Functional Programming Patterns

### Immutability
```scheme
;; Prefer pure functions
(define (atomspace-get-all-atoms as)
  (hash-table-values (atomspace-atoms as)))
```

### Higher-Order Functions
```scheme
;; Functions that operate on functions
(define (atomspace-get-atoms-by-type as atom-type)
  (filter (lambda (atom) ...)
          (atomspace-get-all-atoms as)))
```

### Composition
```scheme
;; Combine simple functions
(define (process-atoms as)
  (map atom->string
       (filter node?
               (atomspace-get-all-atoms as))))
```

## Philosophy

Scheme embodies the philosophy that:
- **Code is Data**: Programs can manipulate programs
- **Simplicity**: Minimal core with powerful abstractions
- **Elegance**: Beautiful solutions through functional composition
- **Flexibility**: Macros enable custom language extensions

## License

Part of the RosettaCog project - see main repository for license details.
