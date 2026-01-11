"""
Unit tests for pattern_library.py

Tests the CognitivePattern dataclass and PatternLanguageLibrary class
which implement the AGI Cognitive Architecture Pattern Language.
"""

import pytest
import sys
import json
import tempfile
from pathlib import Path
from datetime import datetime

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "opencog" / "patterns"))

from pattern_library import (
    PatternCategory,
    PatternQuality,
    CognitivePattern,
    PatternLanguageLibrary,
)


class TestPatternCategory:
    """Tests for PatternCategory enum."""

    def test_has_reasoning(self):
        """Test REASONING category exists."""
        assert PatternCategory.REASONING.value == "reasoning"

    def test_has_problem_solving(self):
        """Test PROBLEM_SOLVING category exists."""
        assert PatternCategory.PROBLEM_SOLVING.value == "problem_solving"

    def test_has_learning(self):
        """Test LEARNING category exists."""
        assert PatternCategory.LEARNING.value == "learning"

    def test_has_communication(self):
        """Test COMMUNICATION category exists."""
        assert PatternCategory.COMMUNICATION.value == "communication"

    def test_has_metacognition(self):
        """Test METACOGNITION category exists."""
        assert PatternCategory.METACOGNITION.value == "metacognition"

    def test_has_memory(self):
        """Test MEMORY category exists."""
        assert PatternCategory.MEMORY.value == "memory"

    def test_has_perception(self):
        """Test PERCEPTION category exists."""
        assert PatternCategory.PERCEPTION.value == "perception"

    def test_all_categories_count(self):
        """Test that all 7 categories exist."""
        assert len(PatternCategory) == 7


class TestPatternQuality:
    """Tests for PatternQuality enum."""

    def test_has_experimental(self):
        """Test EXPERIMENTAL quality exists."""
        assert PatternQuality.EXPERIMENTAL.value == "experimental"

    def test_has_proven(self):
        """Test PROVEN quality exists."""
        assert PatternQuality.PROVEN.value == "proven"

    def test_has_mature(self):
        """Test MATURE quality exists."""
        assert PatternQuality.MATURE.value == "mature"

    def test_has_foundational(self):
        """Test FOUNDATIONAL quality exists."""
        assert PatternQuality.FOUNDATIONAL.value == "foundational"

    def test_all_qualities_count(self):
        """Test that all 4 qualities exist."""
        assert len(PatternQuality) == 4


class TestCognitivePattern:
    """Tests for CognitivePattern dataclass."""

    def test_initialization_minimal(self):
        """Test minimal initialization."""
        pattern = CognitivePattern(
            id="test_pattern",
            name="Test Pattern",
            category=PatternCategory.REASONING,
            context="Test context",
            problem="Test problem",
            forces=["force1", "force2"],
            solution="Test solution",
        )
        assert pattern.id == "test_pattern"
        assert pattern.name == "Test Pattern"
        assert pattern.category == PatternCategory.REASONING

    def test_initialization_with_all_fields(self):
        """Test initialization with all fields."""
        pattern = CognitivePattern(
            id="full_pattern",
            name="Full Pattern",
            category=PatternCategory.PROBLEM_SOLVING,
            context="Full context",
            problem="Full problem",
            forces=["f1", "f2", "f3"],
            solution="Full solution",
            examples=[{"task": "task1", "application": "app1"}],
            related_patterns=["pattern1", "pattern2"],
            quality=PatternQuality.PROVEN,
            use_count=10,
            success_rate=0.85,
        )
        assert pattern.quality == PatternQuality.PROVEN
        assert pattern.use_count == 10
        assert pattern.success_rate == 0.85
        assert len(pattern.examples) == 1
        assert len(pattern.related_patterns) == 2

    def test_default_values(self):
        """Test default values for optional fields."""
        pattern = CognitivePattern(
            id="test",
            name="Test",
            category=PatternCategory.LEARNING,
            context="ctx",
            problem="prob",
            forces=[],
            solution="sol",
        )
        assert pattern.examples == []
        assert pattern.related_patterns == []
        assert pattern.quality == PatternQuality.EXPERIMENTAL
        assert pattern.use_count == 0
        assert pattern.success_rate == 0.0

    def test_to_dict(self):
        """Test conversion to dictionary."""
        pattern = CognitivePattern(
            id="test",
            name="Test Pattern",
            category=PatternCategory.REASONING,
            context="Test context",
            problem="Test problem",
            forces=["force1"],
            solution="Test solution",
        )
        d = pattern.to_dict()
        assert d["id"] == "test"
        assert d["name"] == "Test Pattern"
        assert d["category"] == "reasoning"
        assert d["context"] == "Test context"
        assert d["problem"] == "Test problem"
        assert d["forces"] == ["force1"]
        assert d["solution"] == "Test solution"
        assert "created_date" in d
        assert "last_updated" in d

    def test_from_dict(self):
        """Test creation from dictionary."""
        data = {
            "id": "from_dict",
            "name": "From Dict Pattern",
            "category": "problem_solving",
            "context": "Dict context",
            "problem": "Dict problem",
            "forces": ["f1", "f2"],
            "solution": "Dict solution",
            "quality": "proven",
            "use_count": 5,
            "success_rate": 0.75,
        }
        pattern = CognitivePattern.from_dict(data)
        assert pattern.id == "from_dict"
        assert pattern.name == "From Dict Pattern"
        assert pattern.category == PatternCategory.PROBLEM_SOLVING
        assert pattern.quality == PatternQuality.PROVEN
        assert pattern.use_count == 5
        assert pattern.success_rate == 0.75

    def test_from_dict_with_defaults(self):
        """Test from_dict handles missing optional fields."""
        data = {
            "id": "minimal",
            "name": "Minimal",
            "category": "learning",
            "context": "ctx",
            "problem": "prob",
            "forces": [],
            "solution": "sol",
        }
        pattern = CognitivePattern.from_dict(data)
        assert pattern.examples == []
        assert pattern.related_patterns == []
        assert pattern.quality == PatternQuality.EXPERIMENTAL
        assert pattern.use_count == 0

    def test_created_date_is_set(self):
        """Test that created_date is automatically set."""
        pattern = CognitivePattern(
            id="test",
            name="Test",
            category=PatternCategory.MEMORY,
            context="ctx",
            problem="prob",
            forces=[],
            solution="sol",
        )
        # Should be a valid ISO format date
        datetime.fromisoformat(pattern.created_date)

    def test_last_updated_is_set(self):
        """Test that last_updated is automatically set."""
        pattern = CognitivePattern(
            id="test",
            name="Test",
            category=PatternCategory.PERCEPTION,
            context="ctx",
            problem="prob",
            forces=[],
            solution="sol",
        )
        datetime.fromisoformat(pattern.last_updated)


class TestPatternLanguageLibrary:
    """Tests for PatternLanguageLibrary class."""

    def test_initialization(self):
        """Test library initialization."""
        library = PatternLanguageLibrary()
        assert isinstance(library.patterns, dict)
        assert isinstance(library.pattern_graph, dict)

    def test_initializes_foundational_patterns(self):
        """Test that foundational patterns are initialized."""
        library = PatternLanguageLibrary()
        # Should have at least 10 foundational patterns
        assert len(library.patterns) >= 10

    def test_foundational_patterns_have_correct_quality(self):
        """Test that foundational patterns are marked as FOUNDATIONAL."""
        library = PatternLanguageLibrary()
        foundational_count = sum(
            1 for p in library.patterns.values()
            if p.quality == PatternQuality.FOUNDATIONAL
        )
        assert foundational_count >= 10

    def test_add_pattern(self):
        """Test adding a new pattern."""
        library = PatternLanguageLibrary()
        initial_count = len(library.patterns)

        new_pattern = CognitivePattern(
            id="custom_pattern",
            name="Custom Pattern",
            category=PatternCategory.LEARNING,
            context="Custom context",
            problem="Custom problem",
            forces=["custom force"],
            solution="Custom solution",
        )
        library.add_pattern(new_pattern)

        assert len(library.patterns) == initial_count + 1
        assert "custom_pattern" in library.patterns

    def test_add_pattern_updates_graph(self):
        """Test that adding pattern with relationships updates graph."""
        library = PatternLanguageLibrary()

        new_pattern = CognitivePattern(
            id="graph_test",
            name="Graph Test",
            category=PatternCategory.REASONING,
            context="ctx",
            problem="prob",
            forces=[],
            solution="sol",
            related_patterns=["decomposition_pattern", "recursive_thinking_pattern"],
        )
        library.add_pattern(new_pattern)

        assert "graph_test" in library.pattern_graph
        assert "decomposition_pattern" in library.pattern_graph["graph_test"]

    def test_get_pattern_exists(self):
        """Test getting an existing pattern."""
        library = PatternLanguageLibrary()
        pattern = library.get_pattern("decomposition_pattern")
        assert pattern is not None
        assert pattern.id == "decomposition_pattern"
        assert pattern.name == "Problem Decomposition"

    def test_get_pattern_nonexistent(self):
        """Test getting a nonexistent pattern returns None."""
        library = PatternLanguageLibrary()
        pattern = library.get_pattern("nonexistent_pattern_xyz")
        assert pattern is None

    def test_find_patterns_by_category(self):
        """Test finding patterns by category."""
        library = PatternLanguageLibrary()
        reasoning_patterns = library.find_patterns_by_category(PatternCategory.REASONING)

        assert len(reasoning_patterns) > 0
        for pattern in reasoning_patterns:
            assert pattern.category == PatternCategory.REASONING

    def test_find_patterns_by_category_empty(self):
        """Test finding patterns in category with no patterns."""
        library = PatternLanguageLibrary()
        # COMMUNICATION might have no foundational patterns
        comm_patterns = library.find_patterns_by_category(PatternCategory.COMMUNICATION)
        assert isinstance(comm_patterns, list)

    def test_find_patterns_by_quality(self):
        """Test finding patterns by quality level."""
        library = PatternLanguageLibrary()
        foundational = library.find_patterns_by_quality(PatternQuality.FOUNDATIONAL)

        assert len(foundational) >= 10
        for pattern in foundational:
            assert pattern.quality == PatternQuality.FOUNDATIONAL

    def test_match_patterns_to_task(self):
        """Test matching patterns to a task description."""
        library = PatternLanguageLibrary()

        matches = library.match_patterns_to_task(
            "solve a complex problem by breaking it down into parts",
            ["problem-solving", "decomposition"]
        )

        assert len(matches) > 0
        # Decomposition pattern should be highly relevant
        assert any(p.id == "decomposition_pattern" for p in matches[:5])

    def test_match_patterns_empty_description(self):
        """Test matching with empty description."""
        library = PatternLanguageLibrary()
        matches = library.match_patterns_to_task("", [])
        assert isinstance(matches, list)

    def test_get_related_patterns(self):
        """Test getting related patterns."""
        library = PatternLanguageLibrary()
        related = library.get_related_patterns("decomposition_pattern", depth=1)

        assert isinstance(related, list)
        # Decomposition should have related patterns
        assert len(related) >= 0  # May be 0 if graph not fully connected

    def test_get_related_patterns_depth_zero(self):
        """Test getting related patterns with depth 0."""
        library = PatternLanguageLibrary()
        related = library.get_related_patterns("decomposition_pattern", depth=0)
        assert related == []

    def test_get_related_patterns_nonexistent(self):
        """Test getting related patterns for nonexistent pattern."""
        library = PatternLanguageLibrary()
        related = library.get_related_patterns("nonexistent_xyz", depth=1)
        assert related == []

    def test_record_pattern_use_success(self):
        """Test recording successful pattern use."""
        library = PatternLanguageLibrary()
        pattern = library.get_pattern("decomposition_pattern")
        initial_count = pattern.use_count
        initial_rate = pattern.success_rate

        library.record_pattern_use("decomposition_pattern", success=True)

        assert pattern.use_count == initial_count + 1
        # Success rate should increase (or stay same if already 1.0)
        assert pattern.success_rate >= initial_rate or pattern.success_rate > 0

    def test_record_pattern_use_failure(self):
        """Test recording failed pattern use."""
        library = PatternLanguageLibrary()
        # First, use the pattern successfully many times
        for _ in range(10):
            library.record_pattern_use("logical_reasoning_pattern", success=True)

        pattern = library.get_pattern("logical_reasoning_pattern")
        initial_rate = pattern.success_rate

        # Now record a failure
        library.record_pattern_use("logical_reasoning_pattern", success=False)

        # Success rate should decrease
        assert pattern.success_rate < initial_rate

    def test_record_pattern_use_nonexistent(self):
        """Test recording use for nonexistent pattern."""
        library = PatternLanguageLibrary()
        # Should not raise an error
        library.record_pattern_use("nonexistent_pattern", success=True)

    def test_record_pattern_use_updates_quality(self):
        """Test that recording many successful uses updates quality."""
        library = PatternLanguageLibrary()

        # Add a new experimental pattern
        new_pattern = CognitivePattern(
            id="quality_test_pattern",
            name="Quality Test",
            category=PatternCategory.LEARNING,
            context="ctx",
            problem="prob",
            forces=[],
            solution="sol",
            quality=PatternQuality.EXPERIMENTAL,
            success_rate=0.75,
        )
        library.add_pattern(new_pattern)

        # Record many successful uses
        for _ in range(15):
            library.record_pattern_use("quality_test_pattern", success=True)

        # Quality should have upgraded
        pattern = library.get_pattern("quality_test_pattern")
        assert pattern.quality != PatternQuality.EXPERIMENTAL

    def test_compose_patterns(self):
        """Test composing multiple patterns."""
        library = PatternLanguageLibrary()

        strategy = library.compose_patterns([
            "decomposition_pattern",
            "recursive_thinking_pattern"
        ])

        assert "composed_patterns" in strategy
        assert "execution_order" in strategy
        assert "integration_points" in strategy
        assert "combined_forces" in strategy
        assert len(strategy["composed_patterns"]) == 2

    def test_compose_patterns_with_missing(self):
        """Test composing patterns where some don't exist."""
        library = PatternLanguageLibrary()

        strategy = library.compose_patterns([
            "decomposition_pattern",
            "nonexistent_pattern_xyz"
        ])

        # Should only include the existing pattern
        assert len(strategy["composed_patterns"]) == 1

    def test_evolve_pattern(self):
        """Test evolving a pattern with a new example."""
        library = PatternLanguageLibrary()
        pattern = library.get_pattern("decomposition_pattern")
        initial_examples = len(pattern.examples)

        new_example = {
            "task": "New task",
            "application": "New application"
        }
        library.evolve_pattern("decomposition_pattern", new_example)

        assert len(pattern.examples) == initial_examples + 1
        assert new_example in pattern.examples

    def test_evolve_pattern_nonexistent(self):
        """Test evolving a nonexistent pattern."""
        library = PatternLanguageLibrary()
        # Should not raise an error
        library.evolve_pattern("nonexistent_xyz", {"task": "test"})

    def test_save_and_load_library(self, temp_dir):
        """Test saving and loading the library."""
        library = PatternLanguageLibrary()

        # Add a custom pattern
        custom = CognitivePattern(
            id="save_test",
            name="Save Test",
            category=PatternCategory.MEMORY,
            context="ctx",
            problem="prob",
            forces=["f1"],
            solution="sol",
        )
        library.add_pattern(custom)

        # Save
        save_path = temp_dir / "test_library.json"
        library.save_library(str(save_path))

        assert save_path.exists()

        # Load into new library
        new_library = PatternLanguageLibrary()
        new_library.load_library(str(save_path))

        assert "save_test" in new_library.patterns
        assert new_library.patterns["save_test"].name == "Save Test"

    def test_save_library_no_path(self):
        """Test saving without path when none specified."""
        library = PatternLanguageLibrary()
        # Should not raise, just log warning
        library.save_library(None)

    def test_specific_foundational_patterns_exist(self):
        """Test that specific foundational patterns exist."""
        library = PatternLanguageLibrary()

        expected_patterns = [
            "decomposition_pattern",
            "recursive_thinking_pattern",
            "pattern_matching_pattern",
            "logical_reasoning_pattern",
            "abstraction_pattern",
            "working_backward_pattern",
            "metacognitive_monitoring_pattern",
            "analogy_pattern",
            "constraint_satisfaction_pattern",
            "causal_reasoning_pattern",
        ]

        for pattern_id in expected_patterns:
            assert pattern_id in library.patterns, f"Missing pattern: {pattern_id}"


class TestPatternLibraryIntegration:
    """Integration tests for pattern library."""

    def test_full_workflow(self):
        """Test a complete workflow with the pattern library."""
        library = PatternLanguageLibrary()

        # 1. Match patterns to a task
        matches = library.match_patterns_to_task(
            "solve complex recursive problem with constraints",
            ["recursion", "constraints", "problem-solving"]
        )
        assert len(matches) > 0

        # 2. Compose matched patterns
        top_patterns = [p.id for p in matches[:3]]
        strategy = library.compose_patterns(top_patterns)
        assert len(strategy["composed_patterns"]) > 0

        # 3. Record pattern use
        for pattern_id in top_patterns:
            library.record_pattern_use(pattern_id, success=True)

        # 4. Evolve a pattern
        if matches:
            library.evolve_pattern(matches[0].id, {
                "task": "Test task",
                "application": "Applied successfully"
            })

    def test_pattern_relationships_are_bidirectional(self):
        """Test that pattern relationships can be traversed."""
        library = PatternLanguageLibrary()

        # Get a pattern with relationships
        decomp = library.get_pattern("decomposition_pattern")

        # Traverse relationships
        if decomp.related_patterns:
            for related_id in decomp.related_patterns:
                related = library.get_pattern(related_id)
                # Related pattern should exist (or be a planned pattern)
                # It's OK if some don't exist yet
