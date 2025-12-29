"""
Unit tests for atom_types.py

Tests the atom type system including CognitiveDomainAtom, LanguageParadigmAtom,
and AtomTypeSystem classes.
"""

import pytest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "opencog" / "lib"))

from atom_types import (
    AtomTypeCategory,
    AtomType,
    CognitiveDomainAtom,
    LanguageParadigmAtom,
    AtomTypeSystem,
)


class TestAtomTypeCategory:
    """Tests for AtomTypeCategory enum."""

    def test_has_cognitive_domain(self):
        """Test COGNITIVE_DOMAIN category exists."""
        assert AtomTypeCategory.COGNITIVE_DOMAIN.value == "cognitive_domain"

    def test_has_language_paradigm(self):
        """Test LANGUAGE_PARADIGM category exists."""
        assert AtomTypeCategory.LANGUAGE_PARADIGM.value == "language_paradigm"


class TestCognitiveDomainAtom:
    """Tests for CognitiveDomainAtom class."""

    def test_initialization(self):
        """Test basic initialization."""
        atom = CognitiveDomainAtom(
            name="test_domain",
            description="A test domain",
            task_universe={"task1", "task2"},
            subcategories={"subcat1": {}},
            cognitive_processes=["process1"],
            metrics={"metric1": 0.5},
        )
        assert atom.name == "test_domain"
        assert atom.description == "A test domain"
        assert atom.category == AtomTypeCategory.COGNITIVE_DOMAIN

    def test_default_values(self):
        """Test default values for optional fields."""
        atom = CognitiveDomainAtom(name="minimal", description="Minimal atom")
        assert atom.task_universe == set()
        assert atom.subcategories == {}
        assert atom.cognitive_processes == []
        assert atom.metrics == {}

    def test_get_expression(self):
        """Test formal expression generation."""
        atom = CognitiveDomainAtom(name="symbolic_reasoning", description="Test")
        expr = atom.get_expression()
        assert "CD(symbolic_reasoning)" in expr
        assert "Omega" in expr or "Ω" in expr

    def test_get_detailed_expression(self):
        """Test detailed expression breakdown."""
        atom = CognitiveDomainAtom(
            name="test",
            description="Test",
            task_universe={"t1", "t2"},
            subcategories={"s1": {}, "s2": {}},
            cognitive_processes=["p1", "p2"],
            metrics={"m1": 1.0},
        )
        details = atom.get_detailed_expression()
        assert "canonical" in details
        assert "omega" in details
        assert "sigma" in details
        assert "psi" in details
        assert "phi" in details

    def test_compute_complexity(self):
        """Test complexity computation."""
        atom = CognitiveDomainAtom(
            name="test",
            description="Test",
            task_universe={"t1", "t2", "t3"},
            subcategories={"s1": {}, "s2": {}},
            cognitive_processes=["p1", "p2"],
        )
        complexity = atom.compute_complexity()
        assert complexity > 0
        assert isinstance(complexity, float)

    def test_to_dict(self):
        """Test dictionary conversion."""
        atom = CognitiveDomainAtom(name="test", description="Test")
        d = atom.to_dict()
        assert d["name"] == "test"
        assert d["category"] == "cognitive_domain"
        assert "expression" in d


class TestLanguageParadigmAtom:
    """Tests for LanguageParadigmAtom class."""

    def test_initialization(self):
        """Test basic initialization."""
        atom = LanguageParadigmAtom(
            name="functional",
            description="Functional programming",
            language_set={"Haskell", "Lisp"},
            features=["immutability"],
            computational_model="Lambda calculus",
            domain_applicability={"symbolic_reasoning": 0.9},
        )
        assert atom.name == "functional"
        assert atom.category == AtomTypeCategory.LANGUAGE_PARADIGM

    def test_default_values(self):
        """Test default values for optional fields."""
        atom = LanguageParadigmAtom(name="minimal", description="Minimal")
        assert atom.language_set == set()
        assert atom.features == []
        assert atom.computational_model == ""
        assert atom.domain_applicability == {}

    def test_get_expression(self):
        """Test formal expression generation."""
        atom = LanguageParadigmAtom(name="imperative", description="Test")
        expr = atom.get_expression()
        assert "LP(imperative)" in expr
        assert "Lambda" in expr or "Λ" in expr

    def test_get_detailed_expression(self):
        """Test detailed expression breakdown."""
        atom = LanguageParadigmAtom(
            name="test",
            description="Test",
            language_set={"Python", "Ruby"},
            features=["dynamic_typing"],
            computational_model="Interpreted",
            domain_applicability={"ml": 0.8},
        )
        details = atom.get_detailed_expression()
        assert "canonical" in details
        assert "lambda" in details
        assert "pi" in details
        assert "theta" in details
        assert "xi" in details

    def test_compute_versatility(self):
        """Test versatility computation."""
        atom = LanguageParadigmAtom(
            name="test",
            description="Test",
            language_set={"Lang1", "Lang2"},
            features=["f1", "f2"],
            domain_applicability={"d1": 0.8, "d2": 0.7},
        )
        versatility = atom.compute_versatility()
        assert versatility > 0
        assert isinstance(versatility, float)

    def test_to_dict(self):
        """Test dictionary conversion."""
        atom = LanguageParadigmAtom(name="test", description="Test")
        d = atom.to_dict()
        assert d["name"] == "test"
        assert d["category"] == "language_paradigm"
        assert "expression" in d


class TestAtomTypeSystem:
    """Tests for AtomTypeSystem class."""

    def test_initialization(self):
        """Test system initialization."""
        system = AtomTypeSystem()
        assert system.cognitive_domains == {}
        assert system.language_paradigms == {}

    def test_register_cognitive_domain(self):
        """Test registering cognitive domain atoms."""
        system = AtomTypeSystem()
        atom = CognitiveDomainAtom(name="test", description="Test")
        system.register_cognitive_domain(atom)
        assert "test" in system.cognitive_domains
        assert system.cognitive_domains["test"] == atom

    def test_register_language_paradigm(self):
        """Test registering language paradigm atoms."""
        system = AtomTypeSystem()
        atom = LanguageParadigmAtom(name="test", description="Test")
        system.register_language_paradigm(atom)
        assert "test" in system.language_paradigms
        assert system.language_paradigms["test"] == atom

    def test_get_generalized_expressions(self):
        """Test getting generalized expressions."""
        system = AtomTypeSystem()
        exprs = system.get_generalized_expressions()
        assert "cognitive_domain" in exprs
        assert "language_paradigm" in exprs
        assert "expression" in exprs["cognitive_domain"]
        assert "expression" in exprs["language_paradigm"]

    def test_get_all_expressions(self):
        """Test getting all expressions."""
        system = AtomTypeSystem()
        atom1 = CognitiveDomainAtom(name="cd1", description="Test")
        atom2 = LanguageParadigmAtom(name="lp1", description="Test")
        system.register_cognitive_domain(atom1)
        system.register_language_paradigm(atom2)

        all_exprs = system.get_all_expressions()
        assert "generalized" in all_exprs
        assert "cognitive_domains" in all_exprs
        assert "language_paradigms" in all_exprs
        assert "cd1" in all_exprs["cognitive_domains"]
        assert "lp1" in all_exprs["language_paradigms"]

    def test_compute_paradigm_domain_affinity(self):
        """Test affinity computation."""
        system = AtomTypeSystem()
        cd = CognitiveDomainAtom(name="reasoning", description="Test")
        lp = LanguageParadigmAtom(
            name="functional",
            description="Test",
            domain_applicability={"reasoning": 0.9},
        )
        system.register_cognitive_domain(cd)
        system.register_language_paradigm(lp)

        affinity = system.compute_paradigm_domain_affinity("functional", "reasoning")
        assert affinity == 0.9

    def test_affinity_nonexistent_returns_zero(self):
        """Test affinity for nonexistent paradigm/domain."""
        system = AtomTypeSystem()
        affinity = system.compute_paradigm_domain_affinity("nonexistent", "also_none")
        assert affinity == 0.0

    def test_generate_comparison_insights(self):
        """Test insight generation."""
        system = AtomTypeSystem()
        cd = CognitiveDomainAtom(
            name="cd1",
            description="Test",
            task_universe={"t1"},
            subcategories={"s1": {}},
        )
        lp = LanguageParadigmAtom(
            name="lp1", description="Test", language_set={"Lang1"}, features=["f1"]
        )
        system.register_cognitive_domain(cd)
        system.register_language_paradigm(lp)

        insights = system.generate_comparison_insights()
        assert "structural_comparison" in insights
        assert "complexity_analysis" in insights
        assert "coverage_analysis" in insights
        assert "affinity_patterns" in insights

    def test_export_to_json(self, temp_dir):
        """Test JSON export."""
        system = AtomTypeSystem()
        cd = CognitiveDomainAtom(name="test_cd", description="Test")
        system.register_cognitive_domain(cd)

        output_file = temp_dir / "test_atoms.json"
        system.export_to_json(str(output_file))

        assert output_file.exists()

        import json

        with open(output_file) as f:
            data = json.load(f)
        assert "cognitive_domains" in data
        assert "test_cd" in data["cognitive_domains"]
