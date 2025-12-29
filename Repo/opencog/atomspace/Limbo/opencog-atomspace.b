# opencog-atomspace.b - Hypergraph in Limbo
implement AtomSpace;

include "sys.m";
    sys: Sys;
    print: import sys;
include "draw.m";

AtomSpace: module {
    init: fn(nil: ref Draw->Context, nil: list of string);
};

AtomType: adt {
    CONCEPT_NODE, PREDICATE_NODE, INHERITANCE_LINK, EVALUATION_LINK: con;
};

TruthValue: adt {
    strength: real;
    confidence: real;
};

Atom: adt {
    pick {
    Node =>
        atype: int;
        name: string;
        tv: ref TruthValue;
        id: int;
    Link =>
        ltype: int;
        outgoing: list of ref Atom;
        tv: ref TruthValue;
        id: int;
    }
};

AtomSpaceData: adt {
    atoms: list of ref Atom;
    idCounter: int;
    
    addNode: fn(as: self ref AtomSpaceData, atype: int, name: string): ref Atom;
    addLink: fn(as: self ref AtomSpaceData, ltype: int, outgoing: list of ref Atom): ref Atom;
    size: fn(as: self ref AtomSpaceData): int;
};

AtomSpaceData.addNode(as: self ref AtomSpaceData, atype: int, name: string): ref Atom {
    node := ref Atom.Node(atype, name, ref TruthValue(1.0, 1.0), as.idCounter++);
    as.atoms = node :: as.atoms;
    return node;
}

AtomSpaceData.addLink(as: self ref AtomSpaceData, ltype: int, outgoing: list of ref Atom): ref Atom {
    link := ref Atom.Link(ltype, outgoing, ref TruthValue(1.0, 1.0), as.idCounter++);
    as.atoms = link :: as.atoms;
    return link;
}

AtomSpaceData.size(as: self ref AtomSpaceData): int {
    count := 0;
    for (l := as.atoms; l != nil; l = tl l)
        count++;
    return count;
}

init(nil: ref Draw->Context, nil: list of string) {
    sys = load Sys Sys->PATH;
    
    print("==================================================================\n");
    print("OpenCog AtomSpace - Limbo Hypergraph Implementation\n");
    print("==================================================================\n\n");
    
    as := ref AtomSpaceData(nil, 0);
    
    human := as.addNode(0, "human");
    mortal := as.addNode(0, "mortal");
    socrates := as.addNode(0, "Socrates");
    
    link1 := as.addLink(1, socrates :: human :: nil);
    link2 := as.addLink(1, human :: mortal :: nil);
    
    print("AtomSpace size: %d\n", as.size());
    print("\nLimbo AtomSpace: ADTs with pick, pattern matching, CSP channels\n");
}
