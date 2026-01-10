#!/usr/bin/env lua
--[[
opencog-atomspace.lua

OpenCog AtomSpace - Hypergraph Knowledge Representation in Lua

This single-file implementation demonstrates Lua's strengths for AI:
- Tables as flexible data structures for graphs
- Metatables for operator overloading (__len, __tostring)
- First-class functions for pattern matching
- Lightweight object system via prototypes
- Simple yet powerful semantics
]]

-- ===== Atom Types =====
-- Demonstrates: Tables as enums

local AtomType = {
    ATOM = "Atom",
    NODE = "Node",
    LINK = "Link",
    CONCEPT_NODE = "ConceptNode",
    PREDICATE_NODE = "PredicateNode",
    VARIABLE_NODE = "VariableNode",
    EVALUATION_LINK = "EvaluationLink",
    INHERITANCE_LINK = "InheritanceLink",
    SIMILARITY_LINK = "SimilarityLink",
    LIST_LINK = "ListLink",
    AND_LINK = "AndLink",
    OR_LINK = "OrLink",
    NOT_LINK = "NotLink"
}

-- ===== Truth Value =====
-- Demonstrates: Simple data objects

local TruthValue = {}
TruthValue.__index = TruthValue

function TruthValue:new(strength, confidence)
    local self = setmetatable({}, TruthValue)
    self.strength = strength or 1.0
    self.confidence = confidence or 1.0
    return self
end

function TruthValue:__tostring()
    return string.format("tv=%.2f conf=%.2f", self.strength, self.confidence)
end

-- ===== Atom (Base Class) =====
-- Demonstrates: Prototype-based inheritance

local Atom = {}
Atom.__index = Atom

local atom_id_counter = 0

function Atom:new(atom_type, tv)
    local self = setmetatable({}, Atom)
    atom_id_counter = atom_id_counter + 1
    self.id = atom_id_counter
    self.atom_type = atom_type
    self.tv = tv or TruthValue:new()
    return self
end

function Atom:get_type()
    return self.atom_type
end

function Atom:get_tv()
    return self.tv
end

function Atom:set_tv(tv)
    self.tv = tv
end

function Atom:__tostring()
    return string.format("Atom(%d, %s, %s)", self.id, self.atom_type, tostring(self.tv))
end

-- ===== Node =====
-- Demonstrates: Inheritance via metatables

local Node = setmetatable({}, {__index = Atom})
Node.__index = Node

function Node:new(atom_type, name, tv)
    local self = Atom:new(atom_type, tv)
    setmetatable(self, Node)
    self.name = name
    return self
end

function Node:get_name()
    return self.name
end

function Node:__tostring()
    return string.format("%s(\"%s\", %s)", self.atom_type, self.name, tostring(self.tv))
end

-- ===== Link =====
-- Demonstrates: Tables for collections

local Link = setmetatable({}, {__index = Atom})
Link.__index = Link

function Link:new(atom_type, outgoing, tv)
    local self = Atom:new(atom_type, tv)
    setmetatable(self, Link)
    self.outgoing = outgoing or {}
    return self
end

function Link:get_outgoing()
    return self.outgoing
end

function Link:get_arity()
    return #self.outgoing
end

function Link:__tostring()
    local names = {}
    for _, atom in ipairs(self.outgoing) do
        table.insert(names, tostring(atom))
    end
    return string.format("%s([%s], %s)", self.atom_type, table.concat(names, ", "), tostring(self.tv))
end

-- ===== AtomSpace =====
-- Demonstrates: Tables as hash maps, operator overloading

local AtomSpace = {}
AtomSpace.__index = AtomSpace

function AtomSpace:new()
    local self = setmetatable({}, AtomSpace)
    self.atoms = {}  -- atom_id -> atom
    self.nodes = {}  -- (type, name) -> atom
    self.links = {}  -- atom -> list of atoms
    return self
end

function AtomSpace:add_node(atom_type, name, tv)
    local key = atom_type .. ":" .. name
    
    if self.nodes[key] then
        return self.nodes[key]
    end
    
    local node = Node:new(atom_type, name, tv)
    self.atoms[node.id] = node
    self.nodes[key] = node
    
    return node
end

function AtomSpace:add_link(atom_type, outgoing, tv)
    local link = Link:new(atom_type, outgoing, tv)
    self.atoms[link.id] = link
    
    -- Build incoming sets
    for _, atom in ipairs(outgoing) do
        if not self.links[atom.id] then
            self.links[atom.id] = {}
        end
        table.insert(self.links[atom.id], link)
    end
    
    return link
end

function AtomSpace:get_atom(atom_id)
    return self.atoms[atom_id]
end

function AtomSpace:contains(atom)
    return self.atoms[atom.id] ~= nil
end

function AtomSpace:remove(atom)
    self.atoms[atom.id] = nil
    
    if atom.get_name then
        local key = atom.atom_type .. ":" .. atom:get_name()
        self.nodes[key] = nil
    end
    
    if atom.get_outgoing then
        for _, out_atom in ipairs(atom:get_outgoing()) do
            if self.links[out_atom.id] then
                local filtered = {}
                for _, link in ipairs(self.links[out_atom.id]) do
                    if link.id ~= atom.id then
                        table.insert(filtered, link)
                    end
                end
                self.links[out_atom.id] = filtered
            end
        end
    end
end

function AtomSpace:get_incoming(atom)
    return self.links[atom.id] or {}
end

function AtomSpace:clear()
    self.atoms = {}
    self.nodes = {}
    self.links = {}
    atom_id_counter = 0
end

function AtomSpace:size()
    local count = 0
    for _ in pairs(self.atoms) do
        count = count + 1
    end
    return count
end

function AtomSpace:get_all_atoms()
    local result = {}
    for _, atom in pairs(self.atoms) do
        table.insert(result, atom)
    end
    return result
end

function AtomSpace:dump()
    print(string.format("AtomSpace (size=%d):", self:size()))
    for _, atom in pairs(self.atoms) do
        print("  " .. tostring(atom))
    end
end

-- Operator overloading
function AtomSpace:__len()
    return self:size()
end

function AtomSpace:__tostring()
    return string.format("AtomSpace(size=%d)", self:size())
end

-- ===== Pattern Matching =====
-- Demonstrates: First-class functions, closures

function pattern_match(atomspace, pattern, callback)
    local matches = {}
    
    for _, atom in pairs(atomspace.atoms) do
        if pattern(atom) then
            table.insert(matches, atom)
            if callback then
                callback(atom)
            end
        end
    end
    
    return matches
end

-- ===== Query Functions =====

local function find_by_type(atomspace, atom_type)
    return pattern_match(atomspace, function(atom)
        return atom.atom_type == atom_type
    end)
end

local function find_by_name(atomspace, name)
    return pattern_match(atomspace, function(atom)
        return atom.get_name and atom:get_name() == name
    end)
end

local function find_links_with(atomspace, target_atom)
    return pattern_match(atomspace, function(atom)
        if atom.get_outgoing then
            for _, out_atom in ipairs(atom:get_outgoing()) do
                if out_atom.id == target_atom.id then
                    return true
                end
            end
        end
        return false
    end)
end

-- ===== Inference =====
-- Demonstrates: Graph traversal

local function transitive_closure(atomspace, start_atom)
    local visited = {}
    local result = {}
    
    local function visit(atom)
        if visited[atom.id] then
            return
        end
        
        visited[atom.id] = true
        table.insert(result, atom)
        
        local incoming = atomspace:get_incoming(atom)
        for _, link in ipairs(incoming) do
            if link.atom_type == AtomType.INHERITANCE_LINK then
                local outgoing = link:get_outgoing()
                if outgoing[1].id == atom.id and #outgoing >= 2 then
                    visit(outgoing[2])
                end
            end
        end
    end
    
    visit(start_atom)
    return result
end

-- ===== Demonstration Function =====

local function demonstrate_atomspace()
    print("=" .. string.rep("=", 69))
    print("OpenCog AtomSpace - Lua Implementation")
    print("=" .. string.rep("=", 69))
    print()
    
    -- Create atomspace
    print("1. Creating AtomSpace")
    print(string.rep("-", 50))
    local atomspace = AtomSpace:new()
    print(string.format("Created: %s", tostring(atomspace)))
    print()
    
    -- Add nodes
    print("2. Adding Nodes")
    print(string.rep("-", 50))
    local human = atomspace:add_node(AtomType.CONCEPT_NODE, "human")
    local mortal = atomspace:add_node(AtomType.CONCEPT_NODE, "mortal")
    local socrates = atomspace:add_node(AtomType.CONCEPT_NODE, "Socrates")
    local philosopher = atomspace:add_node(AtomType.CONCEPT_NODE, "philosopher")
    
    print(tostring(human))
    print(tostring(mortal))
    print(tostring(socrates))
    print(tostring(philosopher))
    print()
    
    -- Add links
    print("3. Adding Links (Relationships)")
    print(string.rep("-", 50))
    local link1 = atomspace:add_link(AtomType.INHERITANCE_LINK, {human, mortal})
    local link2 = atomspace:add_link(AtomType.INHERITANCE_LINK, {socrates, human})
    local link3 = atomspace:add_link(AtomType.INHERITANCE_LINK, {socrates, philosopher})
    
    print(tostring(link1))
    print(tostring(link2))
    print(tostring(link3))
    print()
    
    -- Size and containment
    print("4. AtomSpace Operations")
    print(string.rep("-", 50))
    print(string.format("AtomSpace size: %d atoms", atomspace:size()))
    print(string.format("Contains Socrates: %s", tostring(atomspace:contains(socrates))))
    print()
    
    -- Pattern matching
    print("5. Pattern Matching")
    print(string.rep("-", 50))
    local concept_nodes = find_by_type(atomspace, AtomType.CONCEPT_NODE)
    print(string.format("Found %d concept nodes:", #concept_nodes))
    for _, node in ipairs(concept_nodes) do
        print("  " .. tostring(node))
    end
    print()
    
    local inheritance_links = find_by_type(atomspace, AtomType.INHERITANCE_LINK)
    print(string.format("Found %d inheritance links:", #inheritance_links))
    for _, link in ipairs(inheritance_links) do
        print("  " .. tostring(link))
    end
    print()
    
    -- Incoming sets
    print("6. Incoming Sets (Graph Navigation)")
    print(string.rep("-", 50))
    local socrates_incoming = atomspace:get_incoming(socrates)
    print(string.format("Links pointing to Socrates (%d):", #socrates_incoming))
    for _, link in ipairs(socrates_incoming) do
        print("  " .. tostring(link))
    end
    print()
    
    -- Transitive closure
    print("7. Transitive Inference")
    print(string.rep("-", 50))
    print("Transitive closure starting from Socrates:")
    local closure = transitive_closure(atomspace, socrates)
    for _, atom in ipairs(closure) do
        print("  " .. tostring(atom))
    end
    print("→ Therefore, Socrates is mortal (via transitive inference)")
    print()
    
    -- Truth values
    print("8. Truth Values (Probabilistic Logic)")
    print(string.rep("-", 50))
    local uncertain = atomspace:add_node(AtomType.CONCEPT_NODE, "uncertain",
        TruthValue:new(0.7, 0.8))
    print(string.format("Node with uncertainty: %s", tostring(uncertain)))
    
    local weak_link = atomspace:add_link(AtomType.SIMILARITY_LINK,
        {human, philosopher}, TruthValue:new(0.5, 0.6))
    print(string.format("Link with weak similarity: %s", tostring(weak_link)))
    print()
    
    -- Dump atomspace
    print("9. Complete AtomSpace Dump")
    print(string.rep("-", 50))
    atomspace:dump()
    print()
    
    print("AtomSpace demonstration complete!")
    print("=" .. string.rep("=", 70))
    print("Lua strengths demonstrated:")
    print("  ✓ Tables as flexible graph structure")
    print("  ✓ Metatables for OOP and operator overloading")
    print("  ✓ First-class functions for pattern matching")
    print("  ✓ Closures for graph algorithms")
    print("  ✓ Lightweight and efficient implementation")
    print("  ✓ Prototype-based inheritance")
    print("=" .. string.rep("=", 70))
end

-- Run demonstration if executed directly
if arg and arg[0]:match("opencog%-atomspace%.lua$") then
    demonstrate_atomspace()
end

-- Export module
return {
    AtomType = AtomType,
    TruthValue = TruthValue,
    Atom = Atom,
    Node = Node,
    Link = Link,
    AtomSpace = AtomSpace,
    pattern_match = pattern_match,
    find_by_type = find_by_type,
    find_by_name = find_by_name,
    find_links_with = find_links_with,
    transitive_closure = transitive_closure
}
