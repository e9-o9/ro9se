/*
 * atomspace.rs
 * 
 * OpenCog AtomSpace - Rust Implementation
 * Hypergraph-based knowledge representation system
 * 
 * This implementation demonstrates Rust's strengths:
 * - Memory safety without garbage collection
 * - Ownership and borrowing for safe concurrency
 * - Zero-cost abstractions
 * - Pattern matching and Result/Option types
 * - Trait-based polymorphism
 */

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::fmt;

// ===== Atom Types =====
// Demonstrates: Enums with discriminants, Copy trait

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum AtomType {
    // Node types
    ConceptNode = 1,
    PredicateNode = 2,
    VariableNode = 3,
    NumberNode = 4,
    
    // Link types
    InheritanceLink = 5,
    SimilarityLink = 6,
    EvaluationLink = 7,
    ListLink = 8,
    AndLink = 9,
    OrLink = 10,
}

impl fmt::Display for AtomType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AtomType::ConceptNode => write!(f, "ConceptNode"),
            AtomType::PredicateNode => write!(f, "PredicateNode"),
            AtomType::VariableNode => write!(f, "VariableNode"),
            AtomType::NumberNode => write!(f, "NumberNode"),
            AtomType::InheritanceLink => write!(f, "InheritanceLink"),
            AtomType::SimilarityLink => write!(f, "SimilarityLink"),
            AtomType::EvaluationLink => write!(f, "EvaluationLink"),
            AtomType::ListLink => write!(f, "ListLink"),
            AtomType::AndLink => write!(f, "AndLink"),
            AtomType::OrLink => write!(f, "OrLink"),
        }
    }
}

// ===== Handle Type =====

pub type Handle = u64;

// ===== Truth Value =====
// Demonstrates: Struct with validation, immutability

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TruthValue {
    strength: f64,     // [0.0, 1.0]
    confidence: f64,   // [0.0, 1.0]
}

impl TruthValue {
    pub fn new(strength: f64, confidence: f64) -> Self {
        TruthValue {
            strength: strength.clamp(0.0, 1.0),
            confidence: confidence.clamp(0.0, 1.0),
        }
    }

    pub fn default() -> Self {
        TruthValue {
            strength: 1.0,
            confidence: 1.0,
        }
    }

    pub fn strength(&self) -> f64 {
        self.strength
    }

    pub fn confidence(&self) -> f64 {
        self.confidence
    }
}

impl Default for TruthValue {
    fn default() -> Self {
        Self::default()
    }
}

impl fmt::Display for TruthValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{:.2}, {:.2}>", self.strength, self.confidence)
    }
}

// ===== Atom Trait =====
// Demonstrates: Trait definition for polymorphism

pub trait Atom: Send + Sync {
    fn handle(&self) -> Handle;
    fn atom_type(&self) -> AtomType;
    fn truth_value(&self) -> TruthValue;
    fn set_truth_value(&mut self, tv: TruthValue);
    fn to_string(&self) -> String;
}

// ===== Node Implementation =====
// Demonstrates: Struct with trait implementation

#[derive(Debug, Clone)]
pub struct Node {
    handle: Handle,
    atom_type: AtomType,
    name: String,
    truth_value: TruthValue,
}

impl Node {
    pub fn new(atom_type: AtomType, name: String) -> Self {
        Node {
            handle: 0,
            atom_type,
            name,
            truth_value: TruthValue::default(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn set_handle(&mut self, handle: Handle) {
        self.handle = handle;
    }
}

impl Atom for Node {
    fn handle(&self) -> Handle {
        self.handle
    }

    fn atom_type(&self) -> AtomType {
        self.atom_type
    }

    fn truth_value(&self) -> TruthValue {
        self.truth_value
    }

    fn set_truth_value(&mut self, tv: TruthValue) {
        self.truth_value = tv;
    }

    fn to_string(&self) -> String {
        format!("({} \"{}\")", self.atom_type, self.name)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

// ===== Link Implementation =====
// Demonstrates: Recursive structures with Arc for shared ownership

#[derive(Debug, Clone)]
pub struct Link {
    handle: Handle,
    atom_type: AtomType,
    outgoing: Vec<AtomRef>,
    truth_value: TruthValue,
}

// AtomRef is an Arc-wrapped trait object for shared ownership
pub type AtomRef = Arc<RwLock<dyn Atom>>;

impl Link {
    pub fn new(atom_type: AtomType, outgoing: Vec<AtomRef>) -> Self {
        Link {
            handle: 0,
            atom_type,
            outgoing,
            truth_value: TruthValue::default(),
        }
    }

    pub fn outgoing(&self) -> &[AtomRef] {
        &self.outgoing
    }

    pub fn arity(&self) -> usize {
        self.outgoing.len()
    }

    pub fn set_handle(&mut self, handle: Handle) {
        self.handle = handle;
    }
}

impl Atom for Link {
    fn handle(&self) -> Handle {
        self.handle
    }

    fn atom_type(&self) -> AtomType {
        self.atom_type
    }

    fn truth_value(&self) -> TruthValue {
        self.truth_value
    }

    fn set_truth_value(&mut self, tv: TruthValue) {
        self.truth_value = tv;
    }

    fn to_string(&self) -> String {
        let out_strs: Vec<String> = self.outgoing
            .iter()
            .map(|a| {
                let atom = a.read().unwrap();
                atom.to_string()
            })
            .collect();
        
        format!("({} {})", self.atom_type, out_strs.join(" "))
    }
}

impl fmt::Display for Link {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

// ===== AtomSpace Implementation =====
// Demonstrates: Concurrent data structure with Arc and RwLock

pub struct AtomSpace {
    atoms: Arc<RwLock<HashMap<Handle, AtomRef>>>,
    node_index: Arc<RwLock<HashMap<(AtomType, String), Handle>>>,
    next_handle: Arc<RwLock<Handle>>,
}

impl AtomSpace {
    pub fn new() -> Self {
        AtomSpace {
            atoms: Arc::new(RwLock::new(HashMap::new())),
            node_index: Arc::new(RwLock::new(HashMap::new())),
            next_handle: Arc::new(RwLock::new(1)),
        }
    }

    // Add node to AtomSpace
    pub fn add_node(&self, atom_type: AtomType, name: String) -> AtomRef {
        let key = (atom_type, name.clone());
        
        // Check if node already exists
        {
            let index = self.node_index.read().unwrap();
            if let Some(&handle) = index.get(&key) {
                let atoms = self.atoms.read().unwrap();
                if let Some(atom_ref) = atoms.get(&handle) {
                    return Arc::clone(atom_ref);
                }
            }
        }
        
        // Create new node
        let mut node = Node::new(atom_type, name.clone());
        
        // Get next handle
        let handle = {
            let mut next = self.next_handle.write().unwrap();
            let h = *next;
            *next += 1;
            h
        };
        
        node.set_handle(handle);
        let atom_ref = Arc::new(RwLock::new(node)) as AtomRef;
        
        // Store in maps
        {
            let mut atoms = self.atoms.write().unwrap();
            atoms.insert(handle, Arc::clone(&atom_ref));
        }
        
        {
            let mut index = self.node_index.write().unwrap();
            index.insert(key, handle);
        }
        
        atom_ref
    }

    // Add link to AtomSpace
    pub fn add_link(&self, atom_type: AtomType, outgoing: Vec<AtomRef>) -> AtomRef {
        // TODO: Check for duplicate links
        
        let mut link = Link::new(atom_type, outgoing);
        
        // Get next handle
        let handle = {
            let mut next = self.next_handle.write().unwrap();
            let h = *next;
            *next += 1;
            h
        };
        
        link.set_handle(handle);
        let atom_ref = Arc::new(RwLock::new(link)) as AtomRef;
        
        // Store in map
        {
            let mut atoms = self.atoms.write().unwrap();
            atoms.insert(handle, Arc::clone(&atom_ref));
        }
        
        atom_ref
    }

    // Get atom by handle
    pub fn get_atom(&self, handle: Handle) -> Option<AtomRef> {
        let atoms = self.atoms.read().unwrap();
        atoms.get(&handle).map(Arc::clone)
    }

    // Get node by type and name
    pub fn get_node(&self, atom_type: AtomType, name: &str) -> Option<AtomRef> {
        let key = (atom_type, name.to_string());
        let index = self.node_index.read().unwrap();
        
        if let Some(&handle) = index.get(&key) {
            let atoms = self.atoms.read().unwrap();
            atoms.get(&handle).map(Arc::clone)
        } else {
            None
        }
    }

    // Remove atom
    pub fn remove_atom(&self, handle: Handle) -> bool {
        let mut atoms = self.atoms.write().unwrap();
        
        if let Some(atom_ref) = atoms.remove(&handle) {
            // Remove from node index if it's a node
            let atom = atom_ref.read().unwrap();
            let atom_type = atom.atom_type();
            drop(atom);
            
            // Try to downcast to Node to get name
            // This is a simplified version - in production would need better type checking
            let mut index = self.node_index.write().unwrap();
            index.retain(|_, &mut v| v != handle);
            
            true
        } else {
            false
        }
    }

    // Get all atoms
    pub fn get_all_atoms(&self) -> Vec<AtomRef> {
        let atoms = self.atoms.read().unwrap();
        atoms.values().map(Arc::clone).collect()
    }

    // Get atoms by type
    pub fn get_atoms_by_type(&self, atom_type: AtomType) -> Vec<AtomRef> {
        let atoms = self.atoms.read().unwrap();
        atoms.values()
            .filter(|atom_ref| {
                let atom = atom_ref.read().unwrap();
                atom.atom_type() == atom_type
            })
            .map(Arc::clone)
            .collect()
    }

    // Get size
    pub fn size(&self) -> usize {
        let atoms = self.atoms.read().unwrap();
        atoms.len()
    }

    // Check if empty
    pub fn is_empty(&self) -> bool {
        self.size() == 0
    }

    // Clear all atoms
    pub fn clear(&self) {
        let mut atoms = self.atoms.write().unwrap();
        atoms.clear();
        
        let mut index = self.node_index.write().unwrap();
        index.clear();
        
        let mut next = self.next_handle.write().unwrap();
        *next = 1;
    }
}

impl Default for AtomSpace {
    fn default() -> Self {
        Self::new()
    }
}

// ===== Convenience Functions =====

impl AtomSpace {
    pub fn concept_node(&self, name: String) -> AtomRef {
        self.add_node(AtomType::ConceptNode, name)
    }

    pub fn inheritance_link(&self, from: AtomRef, to: AtomRef) -> AtomRef {
        self.add_link(AtomType::InheritanceLink, vec![from, to])
    }
}

// ===== Demo Function =====

#[cfg(not(test))]
fn main() {
    println!("{}", "=".repeat(70));
    println!("OpenCog AtomSpace - Rust Implementation Demo");
    println!("Showcasing memory-safe hypergraph knowledge representation");
    println!("{}", "=".repeat(70));
    println!();

    // Create AtomSpace
    println!("1. Creating AtomSpace");
    println!("{}", "-".repeat(70));
    let atomspace = AtomSpace::new();
    println!("  Created empty AtomSpace: size = {}", atomspace.size());
    println!();

    // Add nodes
    println!("2. Adding Nodes");
    println!("{}", "-".repeat(70));
    let human = atomspace.add_node(AtomType::ConceptNode, "human".to_string());
    let mortal = atomspace.add_node(AtomType::ConceptNode, "mortal".to_string());
    let socrates = atomspace.add_node(AtomType::ConceptNode, "Socrates".to_string());

    println!("  Added nodes:");
    println!("    {}", human.read().unwrap().to_string());
    println!("    {}", mortal.read().unwrap().to_string());
    println!("    {}", socrates.read().unwrap().to_string());
    println!("  AtomSpace size: {}", atomspace.size());
    println!();

    // Add links
    println!("3. Adding Links");
    println!("{}", "-".repeat(70));
    let link1 = atomspace.add_link(
        AtomType::InheritanceLink,
        vec![Arc::clone(&human), Arc::clone(&mortal)]
    );
    let link2 = atomspace.add_link(
        AtomType::InheritanceLink,
        vec![Arc::clone(&socrates), Arc::clone(&human)]
    );

    println!("  Added links:");
    println!("    {}", link1.read().unwrap().to_string());
    println!("    {}", link2.read().unwrap().to_string());
    println!("  AtomSpace size: {}", atomspace.size());
    println!();

    // Truth values
    println!("4. Truth Values");
    println!("{}", "-".repeat(70));
    let tv_strong = TruthValue::new(0.95, 0.90);
    let tv_weak = TruthValue::new(0.60, 0.50);

    link1.write().unwrap().set_truth_value(tv_strong);
    link2.write().unwrap().set_truth_value(tv_weak);

    println!("  Link 1 TV: {}", link1.read().unwrap().truth_value());
    println!("  Link 2 TV: {}", link2.read().unwrap().truth_value());
    println!();

    // Query operations
    println!("5. Query Operations");
    println!("{}", "-".repeat(70));
    if let Some(retrieved) = atomspace.get_node(AtomType::ConceptNode, "human") {
        println!("  Retrieved node: {}", retrieved.read().unwrap().to_string());
    }

    let concepts = atomspace.get_atoms_by_type(AtomType::ConceptNode);
    println!("  Concept nodes ({}):", concepts.len());
    for c in &concepts {
        println!("    {}", c.read().unwrap().to_string());
    }

    let links = atomspace.get_atoms_by_type(AtomType::InheritanceLink);
    println!("  Inheritance links ({}):", links.len());
    for l in &links {
        println!("    {}", l.read().unwrap().to_string());
    }
    println!();

    // Performance demonstration
    println!("6. Performance - Batch Operations");
    println!("{}", "-".repeat(70));
    let start = std::time::Instant::now();
    for i in 0..1000 {
        atomspace.add_node(AtomType::ConceptNode, format!("concept_{}", i));
    }
    let elapsed = start.elapsed();
    println!("  Created 1000 nodes in {:.6}s", elapsed.as_secs_f64());
    println!("  Final size: {}", atomspace.size());
    println!();

    println!("{}", "=".repeat(70));
    println!("Rust AtomSpace strengths demonstrated:");
    println!("  ✓ Memory safety without garbage collection");
    println!("  ✓ Arc and RwLock for safe shared ownership");
    println!("  ✓ Zero-cost abstractions");
    println!("  ✓ Trait-based polymorphism");
    println!("  ✓ Pattern matching and type safety");
    println!("  ✓ Performance with compile-time guarantees");
    println!("{}", "=".repeat(70));
}
