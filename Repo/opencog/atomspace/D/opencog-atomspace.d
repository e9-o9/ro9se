#!/usr/bin/env rdmd
// opencog-atomspace.d - Hypergraph in D
import std.stdio, std.string, std.algorithm, std.array;

enum AtomType { ATOM, NODE, LINK, CONCEPT_NODE, PREDICATE_NODE, INHERITANCE_LINK, EVALUATION_LINK }

struct TruthValue { double strength = 1.0; double confidence = 1.0; }

interface IAtom {
    AtomType getType();
    TruthValue getTV();
    void setTV(TruthValue tv);
    size_t getId();
}

class Node : IAtom {
    private AtomType type;
    private string name;
    private TruthValue tv;
    private size_t id;
    
    this(AtomType type, string name, size_t id) {
        this.type = type; this.name = name; this.id = id;
    }
    
    AtomType getType() { return type; }
    TruthValue getTV() { return tv; }
    void setTV(TruthValue tv) { this.tv = tv; }
    size_t getId() { return id; }
    string getName() { return name; }
    
    override string toString() {
        return format("(%s \"%s\" tv=%.2f)", type, name, tv.strength);
    }
}

class Link : IAtom {
    private AtomType type;
    private IAtom[] outgoing;
    private TruthValue tv;
    private size_t id;
    
    this(AtomType type, IAtom[] outgoing, size_t id) {
        this.type = type; this.outgoing = outgoing; this.id = id;
    }
    
    AtomType getType() { return type; }
    TruthValue getTV() { return tv; }
    void setTV(TruthValue tv) { this.tv = tv; }
    size_t getId() { return id; }
    IAtom[] getOutgoing() { return outgoing; }
    
    override string toString() {
        return format("(%s [%d atoms])", type, outgoing.length);
    }
}

class AtomSpace {
    private IAtom[] atoms;
    private Node[string][AtomType] nodeIndex;
    private IAtom[][AtomType] typeIndex;
    private Link[][size_t] incomingIndex;
    private size_t idCounter = 0;
    
    Node addNode(AtomType type, string name) {
        if (auto existing = name in nodeIndex.get(type, null)) {
            return *existing;
        }
        
        auto node = new Node(type, name, idCounter++);
        atoms ~= node;
        nodeIndex[type][name] = node;
        typeIndex[type] ~= node;
        return node;
    }
    
    Link addLink(AtomType type, IAtom[] outgoing) {
        auto link = new Link(type, outgoing, idCounter++);
        atoms ~= link;
        typeIndex[type] ~= link;
        
        foreach (atom; outgoing) {
            incomingIndex[atom.getId()] ~= link;
        }
        
        return link;
    }
    
    size_t size() { return atoms.length; }
    IAtom[] getByType(AtomType type) { return typeIndex.get(type, []); }
}

void demonstrateAtomspace() {
    writeln("==================================================================");
    writeln("OpenCog AtomSpace - D Hypergraph Implementation");
    writeln("==================================================================\n");
    
    auto as = new AtomSpace();
    auto human = as.addNode(AtomType.CONCEPT_NODE, "human");
    auto mortal = as.addNode(AtomType.CONCEPT_NODE, "mortal");
    auto socrates = as.addNode(AtomType.CONCEPT_NODE, "Socrates");
    
    auto link1 = as.addLink(AtomType.INHERITANCE_LINK, [socrates, human]);
    auto link2 = as.addLink(AtomType.INHERITANCE_LINK, [human, mortal]);
    
    writefln("AtomSpace size: %d", as.size());
    writefln("Concept nodes: %d", as.getByType(AtomType.CONCEPT_NODE).length);
    
    writeln("\nD AtomSpace: Templates, interfaces, ranges, associative arrays");
}

void main() { demonstrateAtomspace(); }
