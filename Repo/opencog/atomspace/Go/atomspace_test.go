package atomspace

import (
	"testing"
)

func TestNewAtomSpace(t *testing.T) {
	as := NewAtomSpace()
	if as == nil {
		t.Fatal("NewAtomSpace returned nil")
	}
	if as.Size() != 0 {
		t.Errorf("New AtomSpace should be empty, got size %d", as.Size())
	}
}

func TestAddNode(t *testing.T) {
	as := NewAtomSpace()
	
	node := as.AddNode(ConceptNode, "cat")
	if node == nil {
		t.Fatal("AddNode returned nil")
	}
	
	if node.GetName() != "cat" {
		t.Errorf("Expected name 'cat', got '%s'", node.GetName())
	}
	
	if node.GetType() != ConceptNode {
		t.Errorf("Expected type ConceptNode, got %s", node.GetType())
	}
	
	if as.Size() != 1 {
		t.Errorf("Expected size 1, got %d", as.Size())
	}
}

func TestAddNodeIdempotent(t *testing.T) {
	as := NewAtomSpace()
	
	node1 := as.AddNode(ConceptNode, "cat")
	node2 := as.AddNode(ConceptNode, "cat")
	
	if node1.GetHandle() != node2.GetHandle() {
		t.Error("Adding same node twice should return same handle")
	}
	
	if as.Size() != 1 {
		t.Errorf("Expected size 1, got %d", as.Size())
	}
}

func TestGetNode(t *testing.T) {
	as := NewAtomSpace()
	
	as.AddNode(ConceptNode, "cat")
	node := as.GetNode(ConceptNode, "cat")
	
	if node == nil {
		t.Fatal("GetNode returned nil")
	}
	
	if node.GetName() != "cat" {
		t.Errorf("Expected name 'cat', got '%s'", node.GetName())
	}
}

func TestGetNodeNotFound(t *testing.T) {
	as := NewAtomSpace()
	
	node := as.GetNode(ConceptNode, "nonexistent")
	if node != nil {
		t.Error("GetNode should return nil for nonexistent node")
	}
}

func TestAddLink(t *testing.T) {
	as := NewAtomSpace()
	
	cat := as.AddNode(ConceptNode, "cat")
	mammal := as.AddNode(ConceptNode, "mammal")
	
	link := as.AddLink(InheritanceLink, []Atom{cat, mammal})
	
	if link == nil {
		t.Fatal("AddLink returned nil")
	}
	
	if link.GetType() != InheritanceLink {
		t.Errorf("Expected type InheritanceLink, got %s", link.GetType())
	}
	
	outgoing := link.GetOutgoing()
	if len(outgoing) != 2 {
		t.Errorf("Expected 2 outgoing atoms, got %d", len(outgoing))
	}
	
	if as.Size() != 3 {
		t.Errorf("Expected size 3, got %d", as.Size())
	}
}

func TestTruthValue(t *testing.T) {
	as := NewAtomSpace()
	node := as.AddNode(ConceptNode, "test")
	
	tv := node.GetTruthValue()
	if tv.Strength != 1.0 || tv.Confidence != 1.0 {
		t.Error("Default truth value should be (1.0, 1.0)")
	}
	
	newTV := NewTruthValue(0.8, 0.9)
	node.SetTruthValue(newTV)
	
	tv = node.GetTruthValue()
	if tv.Strength != 0.8 || tv.Confidence != 0.9 {
		t.Errorf("Expected TV (0.8, 0.9), got (%f, %f)", tv.Strength, tv.Confidence)
	}
}

func TestTruthValueValidation(t *testing.T) {
	tv := NewTruthValue(-0.5, 1.5)
	
	if tv.Strength != 0.0 {
		t.Errorf("Negative strength should be clamped to 0.0, got %f", tv.Strength)
	}
	
	if tv.Confidence != 1.0 {
		t.Errorf("Confidence > 1.0 should be clamped to 1.0, got %f", tv.Confidence)
	}
}

func TestRemoveAtom(t *testing.T) {
	as := NewAtomSpace()
	
	node := as.AddNode(ConceptNode, "test")
	handle := node.GetHandle()
	
	if as.Size() != 1 {
		t.Errorf("Expected size 1, got %d", as.Size())
	}
	
	removed := as.RemoveAtom(handle)
	if !removed {
		t.Error("RemoveAtom should return true")
	}
	
	if as.Size() != 0 {
		t.Errorf("Expected size 0 after removal, got %d", as.Size())
	}
	
	node = as.GetNode(ConceptNode, "test")
	if node != nil {
		t.Error("Node should not be found after removal")
	}
}

func TestGetAtomsByType(t *testing.T) {
	as := NewAtomSpace()
	
	as.AddNode(ConceptNode, "cat")
	as.AddNode(ConceptNode, "dog")
	as.AddNode(PredicateNode, "likes")
	
	concepts := as.GetAtomsByType(ConceptNode)
	if len(concepts) != 2 {
		t.Errorf("Expected 2 ConceptNodes, got %d", len(concepts))
	}
	
	predicates := as.GetAtomsByType(PredicateNode)
	if len(predicates) != 1 {
		t.Errorf("Expected 1 PredicateNode, got %d", len(predicates))
	}
}

func TestClear(t *testing.T) {
	as := NewAtomSpace()
	
	as.AddNode(ConceptNode, "cat")
	as.AddNode(ConceptNode, "dog")
	
	if as.Size() != 2 {
		t.Errorf("Expected size 2, got %d", as.Size())
	}
	
	as.Clear()
	
	if as.Size() != 0 {
		t.Errorf("Expected size 0 after clear, got %d", as.Size())
	}
}

func TestConcurrency(t *testing.T) {
	as := NewAtomSpace()
	
	// Test concurrent additions
	done := make(chan bool)
	
	for i := 0; i < 10; i++ {
		go func(id int) {
			for j := 0; j < 100; j++ {
				as.AddNode(ConceptNode, "concurrent")
			}
			done <- true
		}(i)
	}
	
	for i := 0; i < 10; i++ {
		<-done
	}
	
	// Should only have one node due to deduplication
	if as.Size() != 1 {
		t.Errorf("Expected size 1 after concurrent additions, got %d", as.Size())
	}
}

func TestNodeString(t *testing.T) {
	node := NewNode(ConceptNode, "cat")
	expected := "(ConceptNode \"cat\")"
	
	if node.String() != expected {
		t.Errorf("Expected '%s', got '%s'", expected, node.String())
	}
}

func TestLinkString(t *testing.T) {
	as := NewAtomSpace()
	
	cat := as.AddNode(ConceptNode, "cat")
	mammal := as.AddNode(ConceptNode, "mammal")
	link := as.AddLink(InheritanceLink, []Atom{cat, mammal})
	
	str := link.String()
	if str == "" {
		t.Error("Link string representation should not be empty")
	}
}
