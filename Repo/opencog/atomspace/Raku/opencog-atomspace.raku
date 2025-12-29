#!/usr/bin/env raku
# opencog-atomspace.raku - Hypergraph Knowledge Representation in Raku
use v6;

enum AtomType <ATOM NODE LINK CONCEPT_NODE PREDICATE_NODE VARIABLE_NODE 
               EVALUATION_LINK INHERITANCE_LINK SIMILARITY_LINK LIST_LINK AND_LINK OR_LINK NOT_LINK>;

class TruthValue {
    has Real $.strength = 1.0;
    has Real $.confidence = 1.0;
    method Str { "tv={$.strength.fmt('%.2f')} conf={$.confidence.fmt('%.2f')}" }
}

role Atom {
    has AtomType $.type;
    has TruthValue $.tv is rw = TruthValue.new;
    has Int $.id = state $ = 0++;
    
    method set-tv(Real $s, Real $c) { $!tv = TruthValue.new(strength => $s, confidence => $c); self }
}

class Node does Atom {
    has Str $.name;
    method Str { "({$.type} \"{$.name}\" {$.tv})" }
}

class Link does Atom {
    has @.outgoing;
    method arity { @!outgoing.elems }
    method Str { "({$.type}\n{@!outgoing.map({'  ' ~ .Str}).join("\n")}\n)" }
}

class AtomSpace {
    has @.atoms;
    has %.node-index;  # {type}{name} => node
    has %.type-index;  # {type} => [atoms]
    has %.incoming-index;  # {id} => [links]
    
    method add-node(AtomType $type, Str $name) returns Node {
        with %!node-index{$type}{$name} { return $_ }
        
        my $node = Node.new(:$type, :$name);
        @!atoms.push: $node;
        %!node-index{$type}{$name} = $node;
        %!type-index{$type}.push: $node;
        return $node;
    }
    
    method add-link(AtomType $type, @outgoing) returns Link {
        my $link = Link.new(:$type, :@outgoing);
        @!atoms.push: $link;
        %!type-index{$type}.push: $link;
        
        for @outgoing -> $atom {
            %!incoming-index{$atom.id}.push: $link;
        }
        
        return $link;
    }
    
    method get-incoming($atom) { %!incoming-index{$atom.id} // [] }
    method get-by-type(AtomType $type) { %!type-index{$type} // [] }
    method size { @!atoms.elems }
    method query(Str $pattern) { @!atoms.grep({ .?name && .name.lc.contains($pattern.lc) }) }
    method print-all { say "AtomSpace contains {self.size} atoms:"; .say for @!atoms }
}

sub demonstrate-atomspace {
    say '=' x 70;
    say "OpenCog AtomSpace - Raku Implementation";
    say '=' x 70;
    
    my $as = AtomSpace.new;
    
    say "\n1. Creating Knowledge Base";
    my $human = $as.add-node(CONCEPT_NODE, "human");
    my $mortal = $as.add-node(CONCEPT_NODE, "mortal");
    my $socrates = $as.add-node(CONCEPT_NODE, "Socrates");
    
    say "\n2. Creating Relationships";
    my $link1 = $as.add-link(INHERITANCE_LINK, [$socrates, $human]).set-tv(1.0, 1.0);
    my $link2 = $as.add-link(INHERITANCE_LINK, [$human, $mortal]).set-tv(1.0, 1.0);
    
    say "\n3. Query by Type";
    say "Concept nodes: {$as.get-by-type(CONCEPT_NODE).elems}";
    
    say "\n4. Pattern Matching";
    say "Nodes matching 'mor': {$as.query('mor').map(*.name)}";
    
    say "\n5. Complete AtomSpace";
    $as.print-all;
    
    say "\nRaku AtomSpace: Gradual typing, roles, smart matching";
}

demonstrate-atomspace();
