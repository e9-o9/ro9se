/*
 * opencog-atomspace.cpp
 * 
 * OpenCog AtomSpace - Hypergraph Knowledge Representation
 * A single-file implementation of a hypergraph database for storing
 * and manipulating knowledge in a graph structure with typed nodes and links.
 * 
 * The AtomSpace is the core knowledge representation system in OpenCog,
 * allowing flexible representation of semantic networks, logical relationships,
 * and complex knowledge structures.
 */

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <algorithm>

namespace opencog {
namespace atomspace {

// Forward declarations
class Atom;
class Node;
class Link;
class AtomSpace;

using AtomPtr = std::shared_ptr<Atom>;
using NodePtr = std::shared_ptr<Node>;
using LinkPtr = std::shared_ptr<Link>;

// Atom types enumeration
enum AtomType {
    ATOM,
    NODE,
    LINK,
    CONCEPT_NODE,
    PREDICATE_NODE,
    VARIABLE_NODE,
    EVALUATION_LINK,
    INHERITANCE_LINK,
    SIMILARITY_LINK,
    LIST_LINK,
    AND_LINK,
    OR_LINK,
    NOT_LINK
};

// Base Atom class - represents any entity in the hypergraph
class Atom {
protected:
    AtomType type;
    float truthValue;  // Simple truth value [0.0, 1.0]
    float confidence;  // Confidence in truth value [0.0, 1.0]

public:
    Atom(AtomType t) : type(t), truthValue(1.0f), confidence(1.0f) {}
    virtual ~Atom() {}

    AtomType getType() const { return type; }
    float getTruthValue() const { return truthValue; }
    float getConfidence() const { return confidence; }

    void setTruthValue(float tv, float conf = 1.0f) {
        truthValue = tv;
        confidence = conf;
    }

    virtual std::string toString() const = 0;
    virtual bool isNode() const { return false; }
    virtual bool isLink() const { return false; }

    static std::string typeToString(AtomType type) {
        switch(type) {
            case ATOM: return "Atom";
            case NODE: return "Node";
            case LINK: return "Link";
            case CONCEPT_NODE: return "ConceptNode";
            case PREDICATE_NODE: return "PredicateNode";
            case VARIABLE_NODE: return "VariableNode";
            case EVALUATION_LINK: return "EvaluationLink";
            case INHERITANCE_LINK: return "InheritanceLink";
            case SIMILARITY_LINK: return "SimilarityLink";
            case LIST_LINK: return "ListLink";
            case AND_LINK: return "AndLink";
            case OR_LINK: return "OrLink";
            case NOT_LINK: return "NotLink";
            default: return "Unknown";
        }
    }
};

// Node class - represents named entities
class Node : public Atom {
private:
    std::string name;

public:
    Node(AtomType type, const std::string& nodeName) 
        : Atom(type), name(nodeName) {}

    const std::string& getName() const { return name; }
    bool isNode() const override { return true; }

    std::string toString() const override {
        std::stringstream ss;
        ss << "(" << typeToString(type) << " \"" << name << "\" ";
        ss << "tv=" << truthValue << " conf=" << confidence << ")";
        return ss.str();
    }
};

// Link class - represents relationships between atoms
class Link : public Atom {
private:
    std::vector<AtomPtr> outgoing;

public:
    Link(AtomType type, const std::vector<AtomPtr>& atoms)
        : Atom(type), outgoing(atoms) {}

    const std::vector<AtomPtr>& getOutgoing() const { return outgoing; }
    size_t getArity() const { return outgoing.size(); }
    bool isLink() const override { return true; }

    std::string toString() const override {
        std::stringstream ss;
        ss << "(" << typeToString(type);
        for (const auto& atom : outgoing) {
            ss << "\n  " << atom->toString();
        }
        ss << ")";
        return ss.str();
    }
};

// AtomSpace class - the hypergraph container
class AtomSpace {
private:
    std::map<std::string, NodePtr> nodeIndex;  // Index nodes by name+type
    std::vector<AtomPtr> atoms;  // All atoms in the space
    std::map<AtomPtr, std::set<LinkPtr>> incomingIndex;  // Incoming links per atom

    std::string makeNodeKey(AtomType type, const std::string& name) const {
        return Atom::typeToString(type) + ":" + name;
    }

public:
    AtomSpace() {}

    // Add a node to the AtomSpace
    NodePtr addNode(AtomType type, const std::string& name) {
        std::string key = makeNodeKey(type, name);
        
        // Return existing node if already present
        auto it = nodeIndex.find(key);
        if (it != nodeIndex.end()) {
            return it->second;
        }

        // Create new node
        NodePtr node = std::make_shared<Node>(type, name);
        nodeIndex[key] = node;
        atoms.push_back(node);
        return node;
    }

    // Add a link to the AtomSpace
    LinkPtr addLink(AtomType type, const std::vector<AtomPtr>& outgoing) {
        LinkPtr link = std::make_shared<Link>(type, outgoing);
        atoms.push_back(link);

        // Update incoming index
        for (const auto& atom : outgoing) {
            incomingIndex[atom].insert(link);
        }

        return link;
    }

    // Get incoming links for an atom
    std::set<LinkPtr> getIncoming(const AtomPtr& atom) const {
        auto it = incomingIndex.find(atom);
        return (it != incomingIndex.end()) ? it->second : std::set<LinkPtr>();
    }

    // Query atoms by type
    std::vector<AtomPtr> getAtomsByType(AtomType type) const {
        std::vector<AtomPtr> result;
        for (const auto& atom : atoms) {
            if (atom->getType() == type) {
                result.push_back(atom);
            }
        }
        return result;
    }

    // Get node by name and type
    NodePtr getNode(AtomType type, const std::string& name) const {
        std::string key = makeNodeKey(type, name);
        auto it = nodeIndex.find(key);
        return (it != nodeIndex.end()) ? it->second : nullptr;
    }

    // Get all atoms
    const std::vector<AtomPtr>& getAllAtoms() const {
        return atoms;
    }

    // Get count of atoms
    size_t getSize() const {
        return atoms.size();
    }

    // Print all atoms
    void print() const {
        std::cout << "AtomSpace contains " << atoms.size() << " atoms:" << std::endl;
        for (const auto& atom : atoms) {
            std::cout << "  " << atom->toString() << std::endl;
        }
    }

    // Clear the AtomSpace
    void clear() {
        atoms.clear();
        nodeIndex.clear();
        incomingIndex.clear();
    }
};

} // namespace atomspace
} // namespace opencog

// Demonstration of AtomSpace functionality
int main() {
    using namespace opencog::atomspace;

    std::cout << "OpenCog AtomSpace - Hypergraph Knowledge Representation Demo" << std::endl;
    std::cout << "============================================================" << std::endl << std::endl;

    // Create an AtomSpace
    AtomSpace atomspace;

    std::cout << "Creating a simple knowledge base..." << std::endl << std::endl;

    // Create concept nodes
    auto human = atomspace.addNode(CONCEPT_NODE, "human");
    auto mortal = atomspace.addNode(CONCEPT_NODE, "mortal");
    auto socrates = atomspace.addNode(CONCEPT_NODE, "Socrates");
    auto animal = atomspace.addNode(CONCEPT_NODE, "animal");

    std::cout << "Created nodes:" << std::endl;
    std::cout << "  " << human->toString() << std::endl;
    std::cout << "  " << mortal->toString() << std::endl;
    std::cout << "  " << socrates->toString() << std::endl;
    std::cout << "  " << animal->toString() << std::endl << std::endl;

    // Create inheritance relationships
    auto link1 = atomspace.addLink(INHERITANCE_LINK, {socrates, human});
    auto link2 = atomspace.addLink(INHERITANCE_LINK, {human, mortal});
    auto link3 = atomspace.addLink(INHERITANCE_LINK, {human, animal});

    std::cout << "Created inheritance links:" << std::endl;
    std::cout << "  " << link1->toString() << std::endl;
    std::cout << "  " << link2->toString() << std::endl;
    std::cout << "  " << link3->toString() << std::endl << std::endl;

    // Add truth values
    link1->setTruthValue(1.0f, 1.0f);  // Socrates is definitely human
    link2->setTruthValue(1.0f, 1.0f);  // All humans are mortal
    link3->setTruthValue(1.0f, 0.9f);  // Humans are animals (high confidence)

    // Create predicates
    auto breathes = atomspace.addNode(PREDICATE_NODE, "breathes");
    auto thinks = atomspace.addNode(PREDICATE_NODE, "thinks");

    // Create evaluation links
    auto eval1 = atomspace.addLink(EVALUATION_LINK, {breathes, socrates});
    auto eval2 = atomspace.addLink(EVALUATION_LINK, {thinks, socrates});

    eval1->setTruthValue(1.0f, 1.0f);
    eval2->setTruthValue(1.0f, 1.0f);

    std::cout << "Created evaluation links:" << std::endl;
    std::cout << "  " << eval1->toString() << std::endl;
    std::cout << "  " << eval2->toString() << std::endl << std::endl;

    // Query the knowledge base
    std::cout << "Querying the AtomSpace..." << std::endl << std::endl;

    std::cout << "All concept nodes:" << std::endl;
    auto concepts = atomspace.getAtomsByType(CONCEPT_NODE);
    for (const auto& atom : concepts) {
        std::cout << "  " << atom->toString() << std::endl;
    }
    std::cout << std::endl;

    std::cout << "All inheritance links:" << std::endl;
    auto inheritances = atomspace.getAtomsByType(INHERITANCE_LINK);
    for (const auto& atom : inheritances) {
        std::cout << "  " << atom->toString() << std::endl;
    }
    std::cout << std::endl;

    // Query incoming links
    std::cout << "Links involving 'human':" << std::endl;
    auto incoming = atomspace.getIncoming(human);
    for (const auto& link : incoming) {
        std::cout << "  " << link->toString() << std::endl;
    }
    std::cout << std::endl;

    // Display complete knowledge base
    std::cout << "Complete AtomSpace contents:" << std::endl;
    atomspace.print();
    std::cout << std::endl;

    // Demonstrate logical operations
    std::cout << "Creating logical expressions..." << std::endl;
    auto prop1 = atomspace.addNode(PREDICATE_NODE, "is_alive");
    auto prop2 = atomspace.addNode(PREDICATE_NODE, "is_wise");
    
    auto andLink = atomspace.addLink(AND_LINK, {prop1, prop2});
    std::cout << "  AND expression: " << andLink->toString() << std::endl;

    auto orLink = atomspace.addLink(OR_LINK, {prop1, prop2});
    std::cout << "  OR expression: " << orLink->toString() << std::endl;

    std::cout << std::endl;
    std::cout << "Final AtomSpace size: " << atomspace.getSize() << " atoms" << std::endl;
    std::cout << "AtomSpace demonstration complete!" << std::endl;

    return 0;
}
