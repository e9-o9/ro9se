#!/usr/bin/env perl
# opencog-atomspace.pl
#
# OpenCog AtomSpace - Hypergraph Knowledge Representation in Perl
#
# This single-file implementation demonstrates Perl's strengths for AI:
# - Hash tables for efficient indexing (O(1) lookups)
# - References for complex data structures
# - Blessed objects for OOP
# - Regular expressions for pattern matching
# - Flexible data manipulation

use strict;
use warnings;
use v5.10;
use feature qw(say);
use Data::Dumper;

# ===== Atom Types =====
# Demonstrates: Package variables, enumerations

package AtomType {
    use strict;
    use warnings;
    use Exporter 'import';
    our @EXPORT_OK = qw(@ATOM_TYPES %ATOM_TYPE_MAP);
    
    our @ATOM_TYPES = qw(
        ATOM NODE LINK
        CONCEPT_NODE PREDICATE_NODE VARIABLE_NODE
        EVALUATION_LINK INHERITANCE_LINK SIMILARITY_LINK
        LIST_LINK AND_LINK OR_LINK NOT_LINK
    );
    
    our %ATOM_TYPE_MAP;
    @ATOM_TYPE_MAP{@ATOM_TYPES} = (1) x @ATOM_TYPES;
    
    sub is_valid {
        my ($type) = @_;
        return exists $ATOM_TYPE_MAP{$type};
    }
}

# ===== Truth Value =====
# Demonstrates: Simple blessed hash reference

package TruthValue {
    use strict;
    use warnings;
    
    sub new {
        my ($class, $strength, $confidence) = @_;
        $strength   //= 1.0;
        $confidence //= 1.0;
        
        my $self = {
            strength   => $strength,
            confidence => $confidence,
        };
        
        return bless $self, $class;
    }
    
    sub to_string {
        my ($self) = @_;
        return sprintf("tv=%.2f conf=%.2f", 
                      $self->{strength}, $self->{confidence});
    }
}

# ===== Atom Base Class =====
# Demonstrates: Inheritance, blessed references

package Atom {
    use strict;
    use warnings;
    
    our $ID_COUNTER = 0;
    
    sub new {
        my ($class, $type) = @_;
        
        die "Invalid atom type: $type" unless AtomType::is_valid($type);
        
        my $self = {
            type => $type,
            tv   => TruthValue->new(),
            id   => ++$ID_COUNTER,
        };
        
        return bless $self, $class;
    }
    
    sub set_tv {
        my ($self, $strength, $confidence) = @_;
        $self->{tv} = TruthValue->new($strength, $confidence);
        return $self;
    }
    
    sub get_type { $_[0]->{type} }
    sub get_id   { $_[0]->{id} }
    sub get_tv   { $_[0]->{tv} }
}

# ===== Node =====
# Demonstrates: Inheritance, method overriding

package Node {
    use strict;
    use warnings;
    use parent 'Atom';
    
    sub new {
        my ($class, $type, $name) = @_;
        my $self = $class->SUPER::new($type);
        $self->{name} = $name;
        return $self;
    }
    
    sub get_name { $_[0]->{name} }
    
    sub to_string {
        my ($self) = @_;
        return sprintf("(%s \"%s\" %s)", 
                      $self->{type}, 
                      $self->{name}, 
                      $self->{tv}->to_string());
    }
}

# ===== Link =====
# Demonstrates: Array references, complex structures

package Link {
    use strict;
    use warnings;
    use parent 'Atom';
    
    sub new {
        my ($class, $type, $outgoing) = @_;
        my $self = $class->SUPER::new($type);
        $self->{outgoing} = $outgoing;  # Array reference
        return $self;
    }
    
    sub get_outgoing { $_[0]->{outgoing} }
    sub get_arity    { scalar @{$_[0]->{outgoing}} }
    
    sub to_string {
        my ($self) = @_;
        my $result = "($self->{type}\n";
        for my $atom (@{$self->{outgoing}}) {
            my $atom_str = $atom->to_string();
            $atom_str =~ s/^/  /gm;  # Indent each line
            $result .= "$atom_str\n";
        }
        $result .= ")";
        return $result;
    }
}

# ===== AtomSpace =====
# Demonstrates: Multiple hash indexes, graph operations

package AtomSpace {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        my $self = {
            atoms          => [],     # All atoms
            node_index     => {},     # {type}{name} -> node
            type_index     => {},     # {type} -> [atoms]
            incoming_index => {},     # {atom_id} -> [links]
        };
        return bless $self, $class;
    }
    
    sub add_node {
        my ($self, $type, $name) = @_;
        
        # Check if node already exists (using nested hash)
        if (exists $self->{node_index}{$type} && 
            exists $self->{node_index}{$type}{$name}) {
            return $self->{node_index}{$type}{$name};
        }
        
        # Create new node
        my $node = Node->new($type, $name);
        
        # Add to atoms list
        push @{$self->{atoms}}, $node;
        
        # Add to node index (autovivification)
        $self->{node_index}{$type}{$name} = $node;
        
        # Add to type index
        push @{$self->{type_index}{$type}}, $node;
        
        return $node;
    }
    
    sub add_link {
        my ($self, $type, $outgoing) = @_;
        
        my $link = Link->new($type, $outgoing);
        
        # Add to atoms list
        push @{$self->{atoms}}, $link;
        
        # Add to type index
        push @{$self->{type_index}{$type}}, $link;
        
        # Update incoming index for each outgoing atom
        for my $atom (@$outgoing) {
            my $atom_id = $atom->get_id();
            push @{$self->{incoming_index}{$atom_id}}, $link;
        }
        
        return $link;
    }
    
    sub get_incoming {
        my ($self, $atom) = @_;
        my $atom_id = $atom->get_id();
        return exists $self->{incoming_index}{$atom_id} 
            ? $self->{incoming_index}{$atom_id} 
            : [];
    }
    
    sub get_atoms_by_type {
        my ($self, $type) = @_;
        return exists $self->{type_index}{$type} 
            ? $self->{type_index}{$type} 
            : [];
    }
    
    sub get_node {
        my ($self, $type, $name) = @_;
        return exists $self->{node_index}{$type} && 
               exists $self->{node_index}{$type}{$name}
            ? $self->{node_index}{$type}{$name}
            : undef;
    }
    
    sub size {
        my ($self) = @_;
        return scalar @{$self->{atoms}};
    }
    
    sub contains {
        my ($self, $atom) = @_;
        # Using grep to check membership
        return scalar grep { $_ == $atom } @{$self->{atoms}};
    }
    
    sub query {
        my ($self, $pattern) = @_;
        $pattern = lc($pattern);
        
        # Grep with regex for pattern matching
        return grep { 
            ref($_) eq 'Node' && lc($_->get_name()) =~ /\Q$pattern\E/
        } @{$self->{atoms}};
    }
    
    sub print_all {
        my ($self) = @_;
        say "AtomSpace contains " . $self->size() . " atoms:";
        for my $atom (@{$self->{atoms}}) {
            say "  " . $atom->to_string();
        }
    }
    
    sub stats {
        my ($self) = @_;
        say "AtomSpace Statistics:";
        say "  Total atoms: " . $self->size();
        
        for my $type (sort keys %{$self->{type_index}}) {
            my $count = scalar @{$self->{type_index}{$type}};
            say "  $type: $count";
        }
    }
}

# ===== Main Demonstration =====

package main;

sub demonstrate_atomspace {
    say '=' x 70;
    say "OpenCog AtomSpace - Hypergraph Knowledge Representation in Perl";
    say "Showcasing: Hash tables, references, OOP, regex pattern matching";
    say '=' x 70;
    say '';
    
    # Create AtomSpace
    my $atomspace = AtomSpace->new();
    
    say "1. Creating a Knowledge Base";
    say '-' x 50;
    
    # Create concept nodes
    my $human    = $atomspace->add_node('CONCEPT_NODE', 'human');
    my $mortal   = $atomspace->add_node('CONCEPT_NODE', 'mortal');
    my $socrates = $atomspace->add_node('CONCEPT_NODE', 'Socrates');
    my $animal   = $atomspace->add_node('CONCEPT_NODE', 'animal');
    
    say "Created nodes:";
    say "  " . $_->to_string() for ($human, $mortal, $socrates, $animal);
    say '';
    
    # Create inheritance relationships
    say "2. Creating Relationships";
    say '-' x 50;
    my $link1 = $atomspace->add_link('INHERITANCE_LINK', [$socrates, $human]);
    my $link2 = $atomspace->add_link('INHERITANCE_LINK', [$human, $mortal]);
    my $link3 = $atomspace->add_link('INHERITANCE_LINK', [$human, $animal]);
    
    # Set truth values (fluent interface)
    $link1->set_tv(1.0, 1.0);
    $link2->set_tv(1.0, 1.0);
    $link3->set_tv(1.0, 0.9);
    
    say "Created inheritance links:";
    say "  " . $_->to_string() for ($link1, $link2, $link3);
    say '';
    
    # Create predicates and evaluations
    say "3. Predicates and Evaluations";
    say '-' x 50;
    my $breathes = $atomspace->add_node('PREDICATE_NODE', 'breathes');
    my $thinks   = $atomspace->add_node('PREDICATE_NODE', 'thinks');
    
    my $eval1 = $atomspace->add_link('EVALUATION_LINK', [$breathes, $socrates]);
    my $eval2 = $atomspace->add_link('EVALUATION_LINK', [$thinks, $socrates]);
    
    $eval1->set_tv(1.0, 1.0);
    $eval2->set_tv(1.0, 1.0);
    
    say "  " . $eval1->to_string();
    say "  " . $eval2->to_string();
    say '';
    
    # Query by type
    say "4. Querying by Type";
    say '-' x 50;
    say "All concept nodes:";
    my $concept_nodes = $atomspace->get_atoms_by_type('CONCEPT_NODE');
    say "  " . $_->to_string() for @$concept_nodes;
    say '';
    
    say "All inheritance links:";
    my $inheritance_links = $atomspace->get_atoms_by_type('INHERITANCE_LINK');
    say "  " . $_->to_string() for @$inheritance_links;
    say '';
    
    # Query incoming links
    say "5. Incoming Links (Graph Traversal)";
    say '-' x 50;
    say "Links involving 'human':";
    my $incoming = $atomspace->get_incoming($human);
    say "  " . $_->to_string() for @$incoming;
    say '';
    
    # Pattern matching with regex
    say "6. Pattern Matching (Regex Power)";
    say '-' x 50;
    my @results = $atomspace->query('mor');
    say "Nodes matching 'mor': " . join(", ", map { $_->get_name() } @results);
    say '';
    
    # Logical expressions
    say "7. Logical Expressions";
    say '-' x 50;
    my $prop1 = $atomspace->add_node('PREDICATE_NODE', 'is_alive');
    my $prop2 = $atomspace->add_node('PREDICATE_NODE', 'is_wise');
    
    my $and_link = $atomspace->add_link('AND_LINK', [$prop1, $prop2]);
    my $or_link  = $atomspace->add_link('OR_LINK', [$prop1, $prop2]);
    
    say "  AND: " . $and_link->to_string();
    say "  OR: " . $or_link->to_string();
    say '';
    
    # Hash-based graph traversal
    say "8. Hash-Based Graph Operations";
    say '-' x 50;
    say "Demonstrating efficient O(1) lookups:";
    
    # Direct node lookup
    my $found_socrates = $atomspace->get_node('CONCEPT_NODE', 'Socrates');
    say "  Found Socrates: " . ($found_socrates ? "YES" : "NO");
    
    # Type-based retrieval
    my $predicate_count = scalar @{$atomspace->get_atoms_by_type('PREDICATE_NODE')};
    say "  Predicate node count: $predicate_count";
    say '';
    
    # Complete dump
    say "9. Complete AtomSpace";
    say '-' x 50;
    $atomspace->print_all();
    say '';
    
    # Statistics
    say "10. Statistics";
    say '-' x 50;
    $atomspace->stats();
    say '';
    
    # Perl-specific features
    say "11. Perl-Specific Features";
    say '-' x 50;
    
    # Reference counting
    my $ref_count = @$incoming;
    say "  Array reference size: $ref_count";
    
    # Hash key existence (using exists)
    my $has_human = exists $atomspace->{node_index}{CONCEPT_NODE}{human};
    say "  Has 'human' node: " . ($has_human ? "YES" : "NO");
    
    # Autovivification demonstration
    say "  Autovivification: Perl auto-creates intermediate hashes";
    say '';
    
    say '=' x 70;
    say "Perl AtomSpace strengths:";
    say "  ✓ Hash tables for O(1) lookups";
    say "  ✓ References for complex data structures";
    say "  ✓ Autovivification for easy nested structures";
    say "  ✓ Regular expressions for pattern matching";
    say "  ✓ Flexible object system (blessed references)";
    say "  ✓ Grep/map for functional operations";
    say "  ✓ TIMTOWTDI (multiple ways to accomplish tasks)";
    say '=' x 70;
}

# Run demonstration
demonstrate_atomspace();
