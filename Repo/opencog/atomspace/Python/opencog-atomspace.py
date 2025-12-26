#!/usr/bin/env python3
"""
opencog-atomspace.py

OpenCog AtomSpace - Hypergraph Knowledge Representation in Python

This single-file implementation demonstrates Python's strengths for AI:
- Object-oriented programming with inheritance
- Dynamic typing for flexible data structures
- Operator overloading for intuitive syntax
- Collections (dict, set, list) for graph structures
"""

from enum import Enum, auto
from typing import List, Set, Dict, Optional, Union
from dataclasses import dataclass, field
from abc import ABC, abstractmethod


class AtomType(Enum):
    """Atom types using Python's Enum"""
    ATOM = auto()
    NODE = auto()
    LINK = auto()
    CONCEPT_NODE = auto()
    PREDICATE_NODE = auto()
    VARIABLE_NODE = auto()
    EVALUATION_LINK = auto()
    INHERITANCE_LINK = auto()
    SIMILARITY_LINK = auto()
    LIST_LINK = auto()
    AND_LINK = auto()
    OR_LINK = auto()
    NOT_LINK = auto()


@dataclass
class TruthValue:
    """
    Truth value with strength and confidence
    Demonstrates: Dataclasses, value objects
    """
    strength: float = 1.0
    confidence: float = 1.0
    
    def __str__(self) -> str:
        return f"tv={self.strength:.2f} conf={self.confidence:.2f}"


class Atom(ABC):
    """
    Base Atom class using abstract base class
    Demonstrates: ABC, type hints, magic methods
    """
    
    def __init__(self, atom_type: AtomType):
        self.type = atom_type
        self.tv = TruthValue()
        self._id = id(self)  # Use Python's id for unique identifier
    
    def set_truth_value(self, strength: float, confidence: float = 1.0) -> 'Atom':
        """Set truth value (fluent interface)"""
        self.tv = TruthValue(strength, confidence)
        return self
    
    @abstractmethod
    def __str__(self) -> str:
        """String representation"""
        pass
    
    def __hash__(self) -> int:
        """Make atoms hashable for use in sets/dicts"""
        return self._id
    
    def __eq__(self, other) -> bool:
        """Equality based on identity"""
        return isinstance(other, Atom) and self._id == other._id


class Node(Atom):
    """
    Node class for named entities
    Demonstrates: Inheritance, property access
    """
    
    def __init__(self, atom_type: AtomType, name: str):
        super().__init__(atom_type)
        self.name = name
    
    def __str__(self) -> str:
        return f"({self.type.name} \"{self.name}\" {self.tv})"
    
    def __repr__(self) -> str:
        return f"Node({self.type.name}, '{self.name}')"


class Link(Atom):
    """
    Link class for relationships
    Demonstrates: Composition, list operations
    """
    
    def __init__(self, atom_type: AtomType, outgoing: List[Atom]):
        super().__init__(atom_type)
        self.outgoing = outgoing
    
    @property
    def arity(self) -> int:
        """Number of outgoing atoms"""
        return len(self.outgoing)
    
    def __str__(self) -> str:
        parts = [f"({self.type.name}"]
        for atom in self.outgoing:
            # Indent nested atoms
            atom_str = str(atom)
            indented = '\n  '.join(atom_str.split('\n'))
            parts.append(f"\n  {indented}")
        parts.append(")")
        return ''.join(parts)
    
    def __repr__(self) -> str:
        return f"Link({self.type.name}, {len(self.outgoing)} atoms)"


class AtomSpace:
    """
    AtomSpace - the hypergraph container
    Demonstrates: Dict/set operations, indexing, graph algorithms
    """
    
    def __init__(self):
        # Node index: (type, name) -> Node
        self._node_index: Dict[tuple, Node] = {}
        
        # All atoms
        self._atoms: List[Atom] = []
        
        # Incoming index: atom -> set of links containing it
        self._incoming: Dict[Atom, Set[Link]] = {}
        
        # Type index: type -> list of atoms
        self._type_index: Dict[AtomType, List[Atom]] = {}
    
    def add_node(self, atom_type: AtomType, name: str) -> Node:
        """
        Add a node to the AtomSpace
        Demonstrates: Dictionary operations, caching
        """
        key = (atom_type, name)
        
        # Return existing node if present
        if key in self._node_index:
            return self._node_index[key]
        
        # Create new node
        node = Node(atom_type, name)
        self._node_index[key] = node
        self._atoms.append(node)
        
        # Update type index
        if atom_type not in self._type_index:
            self._type_index[atom_type] = []
        self._type_index[atom_type].append(node)
        
        return node
    
    def add_link(self, atom_type: AtomType, outgoing: List[Atom]) -> Link:
        """
        Add a link to the AtomSpace
        Demonstrates: Set operations, graph indexing
        """
        link = Link(atom_type, outgoing)
        self._atoms.append(link)
        
        # Update type index
        if atom_type not in self._type_index:
            self._type_index[atom_type] = []
        self._type_index[atom_type].append(link)
        
        # Update incoming index
        for atom in outgoing:
            if atom not in self._incoming:
                self._incoming[atom] = set()
            self._incoming[atom].add(link)
        
        return link
    
    def get_incoming(self, atom: Atom) -> Set[Link]:
        """Get all links that contain this atom"""
        return self._incoming.get(atom, set())
    
    def get_atoms_by_type(self, atom_type: AtomType) -> List[Atom]:
        """Query atoms by type"""
        return self._type_index.get(atom_type, [])
    
    def get_node(self, atom_type: AtomType, name: str) -> Optional[Node]:
        """Retrieve a specific node"""
        return self._node_index.get((atom_type, name))
    
    def __len__(self) -> int:
        """Number of atoms"""
        return len(self._atoms)
    
    def __iter__(self):
        """Iterate over all atoms"""
        return iter(self._atoms)
    
    def __contains__(self, atom: Atom) -> bool:
        """Check if atom is in space"""
        return atom in self._atoms
    
    def print_all(self) -> None:
        """Print all atoms"""
        print(f"AtomSpace contains {len(self)} atoms:")
        for atom in self._atoms:
            print(f"  {atom}")
    
    def query(self, pattern: str) -> List[Atom]:
        """
        Simple pattern matching
        Demonstrates: String matching, filtering
        """
        results = []
        for atom in self._atoms:
            if isinstance(atom, Node) and pattern.lower() in atom.name.lower():
                results.append(atom)
        return results


def demonstrate_atomspace():
    """Main demonstration function"""
    print("=" * 70)
    print("OpenCog AtomSpace - Hypergraph Knowledge Representation in Python")
    print("Showcasing: OOP, dynamic typing, collections, operator overloading")
    print("=" * 70)
    print()
    
    # Create an AtomSpace
    atomspace = AtomSpace()
    
    print("1. Creating a Knowledge Base")
    print("-" * 50)
    
    # Create concept nodes
    human = atomspace.add_node(AtomType.CONCEPT_NODE, "human")
    mortal = atomspace.add_node(AtomType.CONCEPT_NODE, "mortal")
    socrates = atomspace.add_node(AtomType.CONCEPT_NODE, "Socrates")
    animal = atomspace.add_node(AtomType.CONCEPT_NODE, "animal")
    
    print("Created nodes:")
    for node in [human, mortal, socrates, animal]:
        print(f"  {node}")
    print()
    
    # Create inheritance relationships
    print("2. Creating Relationships")
    print("-" * 50)
    link1 = atomspace.add_link(AtomType.INHERITANCE_LINK, [socrates, human])
    link2 = atomspace.add_link(AtomType.INHERITANCE_LINK, [human, mortal])
    link3 = atomspace.add_link(AtomType.INHERITANCE_LINK, [human, animal])
    
    # Set truth values (fluent interface)
    link1.set_truth_value(1.0, 1.0)
    link2.set_truth_value(1.0, 1.0)
    link3.set_truth_value(1.0, 0.9)
    
    print("Created inheritance links:")
    for link in [link1, link2, link3]:
        print(f"  {link}")
    print()
    
    # Create predicates and evaluations
    print("3. Predicates and Evaluations")
    print("-" * 50)
    breathes = atomspace.add_node(AtomType.PREDICATE_NODE, "breathes")
    thinks = atomspace.add_node(AtomType.PREDICATE_NODE, "thinks")
    
    eval1 = atomspace.add_link(AtomType.EVALUATION_LINK, [breathes, socrates])
    eval2 = atomspace.add_link(AtomType.EVALUATION_LINK, [thinks, socrates])
    
    eval1.set_truth_value(1.0, 1.0)
    eval2.set_truth_value(1.0, 1.0)
    
    print(f"  {eval1}")
    print(f"  {eval2}")
    print()
    
    # Query by type
    print("4. Querying by Type")
    print("-" * 50)
    print("All concept nodes:")
    for node in atomspace.get_atoms_by_type(AtomType.CONCEPT_NODE):
        print(f"  {node}")
    print()
    
    print("All inheritance links:")
    for link in atomspace.get_atoms_by_type(AtomType.INHERITANCE_LINK):
        print(f"  {link}")
    print()
    
    # Query incoming links
    print("5. Incoming Links (Graph Traversal)")
    print("-" * 50)
    print(f"Links involving 'human':")
    for link in atomspace.get_incoming(human):
        print(f"  {link}")
    print()
    
    # Pattern matching
    print("6. Pattern Matching")
    print("-" * 50)
    results = atomspace.query("mor")
    print(f"Nodes matching 'mor': {[str(n) for n in results]}")
    print()
    
    # Logical operations
    print("7. Logical Expressions")
    print("-" * 50)
    prop1 = atomspace.add_node(AtomType.PREDICATE_NODE, "is_alive")
    prop2 = atomspace.add_node(AtomType.PREDICATE_NODE, "is_wise")
    
    and_link = atomspace.add_link(AtomType.AND_LINK, [prop1, prop2])
    or_link = atomspace.add_link(AtomType.OR_LINK, [prop1, prop2])
    
    print(f"  AND: {and_link}")
    print(f"  OR: {or_link}")
    print()
    
    # Complete dump
    print("8. Complete AtomSpace")
    print("-" * 50)
    atomspace.print_all()
    print()
    
    # Statistics
    print("9. Statistics")
    print("-" * 50)
    print(f"Total atoms: {len(atomspace)}")
    print(f"Concept nodes: {len(atomspace.get_atoms_by_type(AtomType.CONCEPT_NODE))}")
    print(f"Inheritance links: {len(atomspace.get_atoms_by_type(AtomType.INHERITANCE_LINK))}")
    print(f"Predicate nodes: {len(atomspace.get_atoms_by_type(AtomType.PREDICATE_NODE))}")
    print()
    
    # Python-specific features
    print("10. Python-Specific Features")
    print("-" * 50)
    print(f"AtomSpace is iterable: {list(atomspace)[:3]}...")
    print(f"Membership test: {human in atomspace}")
    print(f"Length operator: len(atomspace) = {len(atomspace)}")
    print()
    
    print("=" * 70)
    print("Python AtomSpace strengths:")
    print("  ✓ Dynamic typing for flexible graph structures")
    print("  ✓ Dict/Set for efficient indexing")
    print("  ✓ Operator overloading (__len__, __iter__, __contains__)")
    print("  ✓ Dataclasses for clean data objects")
    print("  ✓ Properties for computed attributes")
    print("  ✓ ABC for enforcing interfaces")
    print("  ✓ Fluent interface for method chaining")
    print("=" * 70)


if __name__ == "__main__":
    demonstrate_atomspace()
