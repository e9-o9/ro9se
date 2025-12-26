% opencog-atomspace.pl
%
% OpenCog AtomSpace - Hypergraph Knowledge Representation in Prolog
%
% This single-file implementation demonstrates Prolog's strengths:
% - Logic programming and unification
% - Knowledge representation with facts and rules
% - Backtracking for search
% - Pattern matching and query resolution
% - Horn clauses for logical inference

:- dynamic atom_node/3.      % atom_node(Type, Name, TruthValue)
:- dynamic atom_link/4.      % atom_link(Type, Atom1, Atom2, TruthValue)
:- dynamic atom_link_n/3.    % atom_link_n(Type, AtomList, TruthValue)

% ============================================================================
% CORE ATOM OPERATIONS
% ============================================================================

% Add a node to the AtomSpace
% Demonstrates: Assertion of facts
add_node(Type, Name) :-
    add_node(Type, Name, tv(1.0, 1.0)).

add_node(Type, Name, TruthValue) :-
    \+ atom_node(Type, Name, _),
    assertz(atom_node(Type, Name, TruthValue)).

add_node(Type, Name, _) :-
    atom_node(Type, Name, _),
    !.  % Node already exists

% Add a binary link
% Demonstrates: Structured terms, pattern matching
add_link(Type, Atom1, Atom2) :-
    add_link(Type, Atom1, Atom2, tv(1.0, 1.0)).

add_link(Type, Atom1, Atom2, TruthValue) :-
    assertz(atom_link(Type, Atom1, Atom2, TruthValue)).

% Add an n-ary link
add_link_n(Type, AtomList) :-
    add_link_n(Type, AtomList, tv(1.0, 1.0)).

add_link_n(Type, AtomList, TruthValue) :-
    assertz(atom_link_n(Type, AtomList, TruthValue)).

% ============================================================================
% QUERIES AND PATTERN MATCHING
% ============================================================================

% Query nodes by type
% Demonstrates: Backtracking to find all solutions
find_nodes(Type, Nodes) :-
    findall(node(Type, Name, TV), atom_node(Type, Name, TV), Nodes).

% Query links by type
find_links(Type, Links) :-
    findall(link(Type, A1, A2, TV), atom_link(Type, A1, A2, TV), Links).

% Find all incoming links for an atom
% Demonstrates: Pattern matching with variables
incoming_links(Node, Links) :-
    findall(link(Type, Node, Target, TV), 
            atom_link(Type, Node, Target, TV), 
            OutLinks),
    findall(link(Type, Source, Node, TV),
            atom_link(Type, Source, Node, TV),
            InLinks),
    append(OutLinks, InLinks, Links).

% Check if two nodes are directly connected
% Demonstrates: Unification
connected(Node1, Node2) :-
    atom_link(_, Node1, Node2, _).
connected(Node1, Node2) :-
    atom_link(_, Node2, Node1, _).

% ============================================================================
% LOGICAL INFERENCE RULES
% ============================================================================

% Inheritance transitivity: If A inherits from B and B inherits from C,
% then A inherits from C
% Demonstrates: Horn clauses, logical rules
inherits_from(A, B) :-
    atom_link(inheritance, node(concept, A, _), node(concept, B, _), _).

inherits_from(A, C) :-
    inherits_from(A, B),
    inherits_from(B, C).

% Check if a node has a property
has_property(Node, Property) :-
    atom_link(evaluation, 
              node(predicate, Property, _),
              node(concept, Node, _), _).

% Similarity relation (symmetric)
% Demonstrates: Multiple clauses for same predicate
similar_to(A, B) :-
    atom_link(similarity, node(concept, A, _), node(concept, B, _), _).

similar_to(A, B) :-
    atom_link(similarity, node(concept, B, _), node(concept, A, _), _).

% ============================================================================
% KNOWLEDGE BASE REASONING
% ============================================================================

% Check if an entity is mortal (transitively through inheritance)
is_mortal(Entity) :-
    inherits_from(Entity, 'human'),
    inherits_from('human', 'mortal').

is_mortal(Entity) :-
    inherits_from(Entity, 'mortal').

% Find all entities with a given property
entities_with_property(Property, Entities) :-
    findall(Entity, has_property(Entity, Property), Entities).

% Count atoms by type
% Demonstrates: Aggregation
count_atoms(Type, Count) :-
    findall(1, atom_node(Type, _, _), Nodes),
    length(Nodes, Count).

% ============================================================================
% UTILITY PREDICATES
% ============================================================================

% Print a node
print_node(node(Type, Name, tv(S, C))) :-
    format('(~w "~w" tv=~2f conf=~2f)~n', [Type, Name, S, C]).

% Print a link
print_link(link(Type, Node1, Node2, tv(S, C))) :-
    format('(~w~n', [Type]),
    format('  '),
    print_node(Node1),
    format('  '),
    print_node(Node2),
    format(')~n').

% Print all atoms
print_all_atoms :-
    write('All nodes:'), nl,
    atom_node(Type, Name, TV),
    print_node(node(Type, Name, TV)),
    fail.
print_all_atoms :-
    nl,
    write('All links:'), nl,
    atom_link(Type, N1, N2, TV),
    print_link(link(Type, N1, N2, TV)),
    fail.
print_all_atoms.

% Clear the atomspace
% Demonstrates: Retraction of facts
clear_atomspace :-
    retractall(atom_node(_, _, _)),
    retractall(atom_link(_, _, _, _)),
    retractall(atom_link_n(_, _, _)).

% ============================================================================
% DEMONSTRATION KNOWLEDGE BASE
% ============================================================================

% Initialize a sample knowledge base
init_demo_kb :-
    write('Initializing demonstration knowledge base...'), nl, nl,
    
    % Add concept nodes
    add_node(concept, 'Socrates'),
    add_node(concept, 'human'),
    add_node(concept, 'mortal'),
    add_node(concept, 'animal'),
    add_node(concept, 'Plato'),
    
    % Add predicate nodes
    add_node(predicate, 'breathes'),
    add_node(predicate, 'thinks'),
    add_node(predicate, 'teaches'),
    
    % Add inheritance relationships
    add_link(inheritance,
             node(concept, 'Socrates', tv(1.0, 1.0)),
             node(concept, 'human', tv(1.0, 1.0)),
             tv(1.0, 1.0)),
    
    add_link(inheritance,
             node(concept, 'human', tv(1.0, 1.0)),
             node(concept, 'mortal', tv(1.0, 1.0)),
             tv(1.0, 1.0)),
    
    add_link(inheritance,
             node(concept, 'human', tv(1.0, 1.0)),
             node(concept, 'animal', tv(1.0, 1.0)),
             tv(1.0, 0.9)),
    
    add_link(inheritance,
             node(concept, 'Plato', tv(1.0, 1.0)),
             node(concept, 'human', tv(1.0, 1.0)),
             tv(1.0, 1.0)),
    
    % Add property evaluations
    add_link(evaluation,
             node(predicate, 'breathes', tv(1.0, 1.0)),
             node(concept, 'Socrates', tv(1.0, 1.0)),
             tv(1.0, 1.0)),
    
    add_link(evaluation,
             node(predicate, 'thinks', tv(1.0, 1.0)),
             node(concept, 'Socrates', tv(1.0, 1.0)),
             tv(1.0, 1.0)),
    
    add_link(evaluation,
             node(predicate, 'teaches', tv(1.0, 1.0)),
             node(concept, 'Socrates', tv(1.0, 1.0)),
             tv(1.0, 1.0)),
    
    write('Knowledge base initialized!'), nl, nl.

% ============================================================================
% DEMONSTRATION QUERIES
% ============================================================================

demo :-
    write('======================================================================'), nl,
    write('OpenCog AtomSpace - Prolog Knowledge Representation'), nl,
    write('Showcasing: Logic programming, unification, inference'), nl,
    write('======================================================================'), nl, nl,
    
    % Initialize
    clear_atomspace,
    init_demo_kb,
    
    % Query 1: Find all concept nodes
    write('1. Query: Find all concept nodes'), nl,
    write('----------------------------------------------------------------------'), nl,
    find_nodes(concept, ConceptNodes),
    length(ConceptNodes, NumConcepts),
    format('Found ~w concept nodes:~n', [NumConcepts]),
    maplist(print_node, ConceptNodes),
    nl,
    
    % Query 2: Find inheritance links
    write('2. Query: Find all inheritance relationships'), nl,
    write('----------------------------------------------------------------------'), nl,
    find_links(inheritance, InheritanceLinks),
    maplist(print_link, InheritanceLinks),
    nl,
    
    % Query 3: Transitive inheritance
    write('3. Logical Inference: Transitive inheritance'), nl,
    write('----------------------------------------------------------------------'), nl,
    ( inherits_from('Socrates', 'mortal') ->
        write('✓ Socrates inherits from mortal (transitively through human)'), nl
    ;   write('✗ Failed to infer transitive inheritance'), nl
    ),
    nl,
    
    % Query 4: Check mortality
    write('4. Logical Inference: Check if Socrates is mortal'), nl,
    write('----------------------------------------------------------------------'), nl,
    ( is_mortal('Socrates') ->
        write('✓ Socrates is mortal (deduced from inheritance chain)'), nl
    ;   write('✗ Could not determine mortality'), nl
    ),
    nl,
    
    % Query 5: Find properties
    write('5. Query: Find entities that think'), nl,
    write('----------------------------------------------------------------------'), nl,
    entities_with_property('thinks', Thinkers),
    format('Entities with "thinks" property: ~w~n', [Thinkers]),
    nl,
    
    % Query 6: Find connected nodes
    write('6. Query: Check connectivity'), nl,
    write('----------------------------------------------------------------------'), nl,
    ( connected(node(concept, 'Socrates', _), node(concept, 'human', _)) ->
        write('✓ Socrates and human are connected'), nl
    ;   write('✗ Not connected'), nl
    ),
    nl,
    
    % Query 7: Count atoms
    write('7. Statistics: Count atoms by type'), nl,
    write('----------------------------------------------------------------------'), nl,
    count_atoms(concept, ConceptCount),
    count_atoms(predicate, PredicateCount),
    format('Concept nodes: ~w~n', [ConceptCount]),
    format('Predicate nodes: ~w~n', [PredicateCount]),
    nl,
    
    % Print all
    write('8. Complete AtomSpace'), nl,
    write('----------------------------------------------------------------------'), nl,
    print_all_atoms,
    nl,
    
    write('======================================================================'), nl,
    write('Prolog strengths demonstrated:'), nl,
    write('  ✓ Logic programming with facts and rules'), nl,
    write('  ✓ Pattern matching and unification'), nl,
    write('  ✓ Backtracking for exhaustive search'), nl,
    write('  ✓ Horn clauses for logical inference'), nl,
    write('  ✓ Transitive closure (inherits_from)'), nl,
    write('  ✓ Declarative knowledge representation'), nl,
    write('  ✓ Query resolution through logical deduction'), nl,
    write('======================================================================'), nl.

% Run the demonstration
:- initialization(demo, main).
