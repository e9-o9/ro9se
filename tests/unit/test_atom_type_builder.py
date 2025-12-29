"""
Unit tests for atom_type_builder.py

Tests the AtomTypeBuilder class which populates the atom type system
from RosettaCog data.
"""

import pytest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "opencog" / "lib"))

from atom_type_builder import AtomTypeBuilder
from atom_types import AtomTypeSystem, CognitiveDomainAtom, LanguageParadigmAtom


class TestAtomTypeBuilderInit:
    """Tests for AtomTypeBuilder initialization."""

    def test_initialization(self, root_dir):
        """Test that builder initializes correctly."""
        builder = AtomTypeBuilder(str(root_dir))
        assert builder.root_dir == root_dir
        assert hasattr(builder, "analyzer")
        assert hasattr(builder, "atom_system")
        assert isinstance(builder.atom_system, AtomTypeSystem)

    def test_loads_categories_data(self, root_dir):
        """Test that categories data is loaded."""
        builder = AtomTypeBuilder(str(root_dir))
        assert hasattr(builder, "categories_data")
        assert isinstance(builder.categories_data, dict)


class TestBuildCognitiveDomainAtoms:
    """Tests for build_cognitive_domain_atoms method."""

    def test_builds_atoms(self, root_dir):
        """Test that atoms are built."""
        builder = AtomTypeBuilder(str(root_dir))
        builder.build_cognitive_domain_atoms()
        assert len(builder.atom_system.cognitive_domains) > 0

    def test_atoms_are_correct_type(self, root_dir):
        """Test that built atoms are CognitiveDomainAtom instances."""
        builder = AtomTypeBuilder(str(root_dir))
        builder.build_cognitive_domain_atoms()
        for atom in builder.atom_system.cognitive_domains.values():
            assert isinstance(atom, CognitiveDomainAtom)

    def test_atoms_have_processes(self, root_dir):
        """Test that atoms have cognitive processes."""
        builder = AtomTypeBuilder(str(root_dir))
        builder.build_cognitive_domain_atoms()
        # At least some atoms should have processes defined
        has_processes = any(
            len(atom.cognitive_processes) > 0
            for atom in builder.atom_system.cognitive_domains.values()
        )
        assert has_processes

    def test_atoms_have_relationships(self, root_dir):
        """Test that atoms have relationships."""
        builder = AtomTypeBuilder(str(root_dir))
        builder.build_cognitive_domain_atoms()
        # At least some atoms should have relationships
        has_relationships = any(
            len(atom.relationships) > 0
            for atom in builder.atom_system.cognitive_domains.values()
        )
        assert has_relationships


class TestBuildLanguageParadigmAtoms:
    """Tests for build_language_paradigm_atoms method."""

    def test_builds_atoms(self, root_dir):
        """Test that atoms are built."""
        builder = AtomTypeBuilder(str(root_dir))
        builder.build_language_paradigm_atoms()
        assert len(builder.atom_system.language_paradigms) > 0

    def test_atoms_are_correct_type(self, root_dir):
        """Test that built atoms are LanguageParadigmAtom instances."""
        builder = AtomTypeBuilder(str(root_dir))
        builder.build_language_paradigm_atoms()
        for atom in builder.atom_system.language_paradigms.values():
            assert isinstance(atom, LanguageParadigmAtom)

    def test_atoms_have_features(self, root_dir):
        """Test that atoms have paradigm features."""
        builder = AtomTypeBuilder(str(root_dir))
        builder.build_language_paradigm_atoms()
        # At least some atoms should have features
        has_features = any(
            len(atom.features) > 0
            for atom in builder.atom_system.language_paradigms.values()
        )
        assert has_features

    def test_atoms_have_computational_model(self, root_dir):
        """Test that atoms have computational models."""
        builder = AtomTypeBuilder(str(root_dir))
        builder.build_language_paradigm_atoms()
        # At least some atoms should have computational models
        has_models = any(
            atom.computational_model != ""
            for atom in builder.atom_system.language_paradigms.values()
        )
        assert has_models


class TestGetCognitiveProcesses:
    """Tests for _get_cognitive_processes helper."""

    def test_symbolic_reasoning_processes(self, root_dir):
        """Test processes for symbolic reasoning."""
        builder = AtomTypeBuilder(str(root_dir))
        processes = builder._get_cognitive_processes("symbolic_reasoning")
        assert len(processes) > 0
        assert "deduction" in processes or "inference" in processes

    def test_pattern_recognition_processes(self, root_dir):
        """Test processes for pattern recognition."""
        builder = AtomTypeBuilder(str(root_dir))
        processes = builder._get_cognitive_processes("pattern_recognition")
        assert len(processes) > 0
        assert "matching" in processes or "search" in processes

    def test_unknown_domain_returns_empty(self, root_dir):
        """Test that unknown domain returns empty list."""
        builder = AtomTypeBuilder(str(root_dir))
        processes = builder._get_cognitive_processes("unknown_domain_xyz")
        assert processes == []


class TestGetParadigmFeatures:
    """Tests for _get_paradigm_features helper."""

    def test_functional_features(self, root_dir):
        """Test features for functional paradigm."""
        builder = AtomTypeBuilder(str(root_dir))
        features = builder._get_paradigm_features("functional")
        assert len(features) > 0
        assert "immutability" in features or "higher_order_functions" in features

    def test_object_oriented_features(self, root_dir):
        """Test features for OO paradigm."""
        builder = AtomTypeBuilder(str(root_dir))
        features = builder._get_paradigm_features("object_oriented")
        assert len(features) > 0
        assert "encapsulation" in features or "inheritance" in features

    def test_unknown_paradigm_returns_empty(self, root_dir):
        """Test that unknown paradigm returns empty list."""
        builder = AtomTypeBuilder(str(root_dir))
        features = builder._get_paradigm_features("unknown_paradigm_xyz")
        assert features == []


class TestGetComputationalModel:
    """Tests for _get_computational_model helper."""

    def test_functional_model(self, root_dir):
        """Test model for functional paradigm."""
        builder = AtomTypeBuilder(str(root_dir))
        model = builder._get_computational_model("functional")
        assert "Lambda" in model or "lambda" in model.lower()

    def test_imperative_model(self, root_dir):
        """Test model for imperative paradigm."""
        builder = AtomTypeBuilder(str(root_dir))
        model = builder._get_computational_model("imperative")
        assert "Von Neumann" in model or "state" in model.lower()

    def test_unknown_paradigm_returns_unspecified(self, root_dir):
        """Test that unknown paradigm returns 'Unspecified'."""
        builder = AtomTypeBuilder(str(root_dir))
        model = builder._get_computational_model("unknown_paradigm_xyz")
        assert model == "Unspecified"


class TestComputeFitScore:
    """Tests for _compute_fit_score helper."""

    def test_functional_symbolic_high_affinity(self, root_dir):
        """Test high affinity between functional and symbolic reasoning."""
        builder = AtomTypeBuilder(str(root_dir))
        score = builder._compute_fit_score("functional", "symbolic_reasoning", set())
        assert score > 0.7  # Should be high affinity

    def test_logic_symbolic_highest_affinity(self, root_dir):
        """Test logic paradigm has highest affinity with symbolic reasoning."""
        builder = AtomTypeBuilder(str(root_dir))
        score = builder._compute_fit_score("logic", "symbolic_reasoning", set())
        assert score == 1.0  # Perfect affinity

    def test_score_in_valid_range(self, root_dir):
        """Test that scores are in [0, 1] range."""
        builder = AtomTypeBuilder(str(root_dir))
        paradigms = ["imperative", "functional", "object_oriented"]
        domains = ["machine_learning", "symbolic_reasoning", "natural_language"]
        for p in paradigms:
            for d in domains:
                score = builder._compute_fit_score(p, d, set())
                assert 0 <= score <= 1


class TestBuild:
    """Tests for build method."""

    def test_build_returns_atom_system(self, root_dir):
        """Test that build returns an AtomTypeSystem."""
        builder = AtomTypeBuilder(str(root_dir))
        system = builder.build()
        assert isinstance(system, AtomTypeSystem)

    def test_build_populates_both_categories(self, root_dir):
        """Test that build populates both cognitive domains and paradigms."""
        builder = AtomTypeBuilder(str(root_dir))
        system = builder.build()
        assert len(system.cognitive_domains) > 0
        assert len(system.language_paradigms) > 0

    def test_build_creates_10_cognitive_domains(self, root_dir):
        """Test that build creates 10 cognitive domain atoms."""
        builder = AtomTypeBuilder(str(root_dir))
        system = builder.build()
        # According to framework, there should be 10 cognitive domains
        assert len(system.cognitive_domains) == 10

    def test_build_creates_9_paradigms(self, root_dir):
        """Test that build creates 9 language paradigm atoms."""
        builder = AtomTypeBuilder(str(root_dir))
        system = builder.build()
        # According to framework, there should be 9 paradigms
        assert len(system.language_paradigms) == 9
