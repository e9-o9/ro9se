// Package atomspace provides a Go implementation of the OpenCog AtomSpace
// knowledge representation system.
//
// The AtomSpace is a hypergraph database for storing and manipulating
// knowledge in the form of Atoms (Nodes and Links).
package atomspace

import (
	"fmt"
	"sync"
)

// AtomType represents the type of an Atom
type AtomType string

const (
	// Node types
	ConceptNode   AtomType = "ConceptNode"
	PredicateNode AtomType = "PredicateNode"
	VariableNode  AtomType = "VariableNode"
	NumberNode    AtomType = "NumberNode"
	
	// Link types
	InheritanceLink AtomType = "InheritanceLink"
	SimilarityLink  AtomType = "SimilarityLink"
	EvaluationLink  AtomType = "EvaluationLink"
	ListLink        AtomType = "ListLink"
	AndLink         AtomType = "AndLink"
	OrLink          AtomType = "OrLink"
)

// TruthValue represents the truth value of an Atom
type TruthValue struct {
	Strength   float64 // [0.0, 1.0]
	Confidence float64 // [0.0, 1.0]
}

// NewTruthValue creates a new TruthValue with validation
func NewTruthValue(strength, confidence float64) TruthValue {
	if strength < 0.0 {
		strength = 0.0
	} else if strength > 1.0 {
		strength = 1.0
	}
	
	if confidence < 0.0 {
		confidence = 0.0
	} else if confidence > 1.0 {
		confidence = 1.0
	}
	
	return TruthValue{Strength: strength, Confidence: confidence}
}

// DefaultTruthValue returns a default truth value
func DefaultTruthValue() TruthValue {
	return TruthValue{Strength: 1.0, Confidence: 1.0}
}

// Atom is the base interface for all atoms in the AtomSpace
type Atom interface {
	GetType() AtomType
	GetHandle() Handle
	GetTruthValue() TruthValue
	SetTruthValue(tv TruthValue)
	String() string
}

// Handle is a unique identifier for an Atom
type Handle uint64

// Node represents a node in the AtomSpace
type Node struct {
	handle     Handle
	atomType   AtomType
	name       string
	truthValue TruthValue
	mu         sync.RWMutex
}

// NewNode creates a new Node
func NewNode(atomType AtomType, name string) *Node {
	return &Node{
		atomType:   atomType,
		name:       name,
		truthValue: DefaultTruthValue(),
	}
}

// GetType returns the atom type
func (n *Node) GetType() AtomType {
	return n.atomType
}

// GetHandle returns the atom handle
func (n *Node) GetHandle() Handle {
	return n.handle
}

// GetName returns the node name
func (n *Node) GetName() string {
	return n.name
}

// GetTruthValue returns the truth value
func (n *Node) GetTruthValue() TruthValue {
	n.mu.RLock()
	defer n.mu.RUnlock()
	return n.truthValue
}

// SetTruthValue sets the truth value
func (n *Node) SetTruthValue(tv TruthValue) {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.truthValue = tv
}

// String returns a string representation
func (n *Node) String() string {
	return fmt.Sprintf("(%s \"%s\")", n.atomType, n.name)
}

// Link represents a link between atoms in the AtomSpace
type Link struct {
	handle     Handle
	atomType   AtomType
	outgoing   []Atom
	truthValue TruthValue
	mu         sync.RWMutex
}

// NewLink creates a new Link
func NewLink(atomType AtomType, outgoing []Atom) *Link {
	return &Link{
		atomType:   atomType,
		outgoing:   outgoing,
		truthValue: DefaultTruthValue(),
	}
}

// GetType returns the atom type
func (l *Link) GetType() AtomType {
	return l.atomType
}

// GetHandle returns the atom handle
func (l *Link) GetHandle() Handle {
	return l.handle
}

// GetOutgoing returns the outgoing set
func (l *Link) GetOutgoing() []Atom {
	return l.outgoing
}

// GetTruthValue returns the truth value
func (l *Link) GetTruthValue() TruthValue {
	l.mu.RLock()
	defer l.mu.RUnlock()
	return l.truthValue
}

// SetTruthValue sets the truth value
func (l *Link) SetTruthValue(tv TruthValue) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.truthValue = tv
}

// String returns a string representation
func (l *Link) String() string {
	outStr := ""
	for i, atom := range l.outgoing {
		if i > 0 {
			outStr += " "
		}
		outStr += atom.String()
	}
	return fmt.Sprintf("(%s %s)", l.atomType, outStr)
}

// AtomSpace is the main knowledge representation database
type AtomSpace struct {
	atoms       map[Handle]Atom
	nodeIndex   map[string]map[string]Handle // type -> name -> handle
	nextHandle  Handle
	mu          sync.RWMutex
}

// NewAtomSpace creates a new AtomSpace
func NewAtomSpace() *AtomSpace {
	return &AtomSpace{
		atoms:      make(map[Handle]Atom),
		nodeIndex:  make(map[string]map[string]Handle),
		nextHandle: 1,
	}
}

// AddNode adds a node to the AtomSpace
func (as *AtomSpace) AddNode(atomType AtomType, name string) *Node {
	as.mu.Lock()
	defer as.mu.Unlock()
	
	// Check if node already exists
	if typeMap, exists := as.nodeIndex[string(atomType)]; exists {
		if handle, found := typeMap[name]; found {
			if node, ok := as.atoms[handle].(*Node); ok {
				return node
			}
		}
	}
	
	// Create new node
	node := NewNode(atomType, name)
	handle := as.nextHandle
	as.nextHandle++
	
	node.handle = handle
	as.atoms[handle] = node
	
	// Update index
	if as.nodeIndex[string(atomType)] == nil {
		as.nodeIndex[string(atomType)] = make(map[string]Handle)
	}
	as.nodeIndex[string(atomType)][name] = handle
	
	return node
}

// AddLink adds a link to the AtomSpace
func (as *AtomSpace) AddLink(atomType AtomType, outgoing []Atom) *Link {
	as.mu.Lock()
	defer as.mu.Unlock()
	
	// TODO: Check for duplicate links
	
	link := NewLink(atomType, outgoing)
	handle := as.nextHandle
	as.nextHandle++
	
	link.handle = handle
	as.atoms[handle] = link
	
	return link
}

// GetAtom retrieves an atom by handle
func (as *AtomSpace) GetAtom(handle Handle) Atom {
	as.mu.RLock()
	defer as.mu.RUnlock()
	return as.atoms[handle]
}

// GetNode retrieves a node by type and name
func (as *AtomSpace) GetNode(atomType AtomType, name string) *Node {
	as.mu.RLock()
	defer as.mu.RUnlock()
	
	if typeMap, exists := as.nodeIndex[string(atomType)]; exists {
		if handle, found := typeMap[name]; found {
			if node, ok := as.atoms[handle].(*Node); ok {
				return node
			}
		}
	}
	return nil
}

// RemoveAtom removes an atom from the AtomSpace
func (as *AtomSpace) RemoveAtom(handle Handle) bool {
	as.mu.Lock()
	defer as.mu.Unlock()
	
	atom, exists := as.atoms[handle]
	if !exists {
		return false
	}
	
	// Remove from index if it's a node
	if node, ok := atom.(*Node); ok {
		if typeMap, exists := as.nodeIndex[string(node.atomType)]; exists {
			delete(typeMap, node.name)
		}
	}
	
	delete(as.atoms, handle)
	return true
}

// GetAllAtoms returns all atoms in the AtomSpace
func (as *AtomSpace) GetAllAtoms() []Atom {
	as.mu.RLock()
	defer as.mu.RUnlock()
	
	atoms := make([]Atom, 0, len(as.atoms))
	for _, atom := range as.atoms {
		atoms = append(atoms, atom)
	}
	return atoms
}

// GetAtomsByType returns all atoms of a specific type
func (as *AtomSpace) GetAtomsByType(atomType AtomType) []Atom {
	as.mu.RLock()
	defer as.mu.RUnlock()
	
	atoms := make([]Atom, 0)
	for _, atom := range as.atoms {
		if atom.GetType() == atomType {
			atoms = append(atoms, atom)
		}
	}
	return atoms
}

// Size returns the number of atoms in the AtomSpace
func (as *AtomSpace) Size() int {
	as.mu.RLock()
	defer as.mu.RUnlock()
	return len(as.atoms)
}

// Clear removes all atoms from the AtomSpace
func (as *AtomSpace) Clear() {
	as.mu.Lock()
	defer as.mu.Unlock()
	
	as.atoms = make(map[Handle]Atom)
	as.nodeIndex = make(map[string]map[string]Handle)
	as.nextHandle = 1
}
