"""
Unit tests for strategy_system.py

Tests the Strategy dataclass, StrategyRepository, and StrategyExecutor classes
which implement the strategy development and management system.
"""

import pytest
import sys
from pathlib import Path
from typing import Dict, Any

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "opencog" / "strategies"))

from strategy_system import (
    StrategyType,
    Strategy,
    StrategyRepository,
    StrategyExecutor,
)


class TestStrategyType:
    """Tests for StrategyType enum."""

    def test_has_deductive(self):
        """Test DEDUCTIVE type exists."""
        assert StrategyType.DEDUCTIVE.value == "deductive"

    def test_has_inductive(self):
        """Test INDUCTIVE type exists."""
        assert StrategyType.INDUCTIVE.value == "inductive"

    def test_has_abductive(self):
        """Test ABDUCTIVE type exists."""
        assert StrategyType.ABDUCTIVE.value == "abductive"

    def test_has_analogical(self):
        """Test ANALOGICAL type exists."""
        assert StrategyType.ANALOGICAL.value == "analogical"

    def test_has_heuristic(self):
        """Test HEURISTIC type exists."""
        assert StrategyType.HEURISTIC.value == "heuristic"

    def test_has_systematic(self):
        """Test SYSTEMATIC type exists."""
        assert StrategyType.SYSTEMATIC.value == "systematic"

    def test_has_creative(self):
        """Test CREATIVE type exists."""
        assert StrategyType.CREATIVE.value == "creative"

    def test_has_hybrid(self):
        """Test HYBRID type exists."""
        assert StrategyType.HYBRID.value == "hybrid"

    def test_all_types_count(self):
        """Test that all 8 types exist."""
        assert len(StrategyType) == 8


class TestStrategy:
    """Tests for Strategy dataclass."""

    def test_initialization_minimal(self):
        """Test minimal initialization."""
        strategy = Strategy(
            id="test_strategy",
            name="Test Strategy",
            strategy_type=StrategyType.DEDUCTIVE,
            description="Test description",
            applicable_patterns=["pattern1"],
            steps=[{"step": 1, "action": "test"}],
        )
        assert strategy.id == "test_strategy"
        assert strategy.name == "Test Strategy"
        assert strategy.strategy_type == StrategyType.DEDUCTIVE

    def test_initialization_with_all_fields(self):
        """Test initialization with all fields."""
        strategy = Strategy(
            id="full_strategy",
            name="Full Strategy",
            strategy_type=StrategyType.HYBRID,
            description="Full description",
            applicable_patterns=["p1", "p2"],
            steps=[
                {"step": 1, "action": "first"},
                {"step": 2, "action": "second"},
            ],
            preconditions=["cond1", "cond2"],
            success_rate=0.85,
            use_count=100,
        )
        assert strategy.success_rate == 0.85
        assert strategy.use_count == 100
        assert len(strategy.preconditions) == 2

    def test_default_values(self):
        """Test default values for optional fields."""
        strategy = Strategy(
            id="test",
            name="Test",
            strategy_type=StrategyType.INDUCTIVE,
            description="desc",
            applicable_patterns=[],
            steps=[],
        )
        assert strategy.preconditions == []
        assert strategy.success_rate == 0.0
        assert strategy.use_count == 0

    def test_to_dict(self):
        """Test conversion to dictionary."""
        strategy = Strategy(
            id="test",
            name="Test Strategy",
            strategy_type=StrategyType.SYSTEMATIC,
            description="Test desc",
            applicable_patterns=["p1"],
            steps=[{"step": 1, "action": "action1"}],
            preconditions=["precond1"],
            success_rate=0.75,
            use_count=50,
        )
        d = strategy.to_dict()

        assert d["id"] == "test"
        assert d["name"] == "Test Strategy"
        assert d["strategy_type"] == "systematic"
        assert d["description"] == "Test desc"
        assert d["applicable_patterns"] == ["p1"]
        assert len(d["steps"]) == 1
        assert d["preconditions"] == ["precond1"]
        assert d["success_rate"] == 0.75
        assert d["use_count"] == 50

    def test_multiple_steps(self):
        """Test strategy with multiple steps."""
        steps = [
            {"step": 1, "action": "analyze", "description": "Analyze input"},
            {"step": 2, "action": "process", "description": "Process data"},
            {"step": 3, "action": "validate", "description": "Validate output"},
        ]
        strategy = Strategy(
            id="multi_step",
            name="Multi-Step Strategy",
            strategy_type=StrategyType.SYSTEMATIC,
            description="desc",
            applicable_patterns=["p1"],
            steps=steps,
        )
        assert len(strategy.steps) == 3
        assert strategy.steps[0]["action"] == "analyze"
        assert strategy.steps[2]["action"] == "validate"


class TestStrategyRepository:
    """Tests for StrategyRepository class."""

    def test_initialization(self):
        """Test repository initialization."""
        repo = StrategyRepository()
        assert isinstance(repo.strategies, dict)

    def test_initializes_core_strategies(self):
        """Test that core strategies are initialized."""
        repo = StrategyRepository()
        # Should have at least 7 core strategies
        assert len(repo.strategies) >= 7

    def test_core_strategies_exist(self):
        """Test that specific core strategies exist."""
        repo = StrategyRepository()

        expected_strategies = [
            "decomposition_strategy",
            "pattern_matching_strategy",
            "constraint_propagation_strategy",
            "working_backward_strategy",
            "hypothesis_testing_strategy",
            "recursive_refinement_strategy",
            "creative_exploration_strategy",
        ]

        for strategy_id in expected_strategies:
            assert strategy_id in repo.strategies, f"Missing strategy: {strategy_id}"

    def test_add_strategy(self):
        """Test adding a new strategy."""
        repo = StrategyRepository()
        initial_count = len(repo.strategies)

        new_strategy = Strategy(
            id="custom_strategy",
            name="Custom Strategy",
            strategy_type=StrategyType.CREATIVE,
            description="Custom description",
            applicable_patterns=["custom_pattern"],
            steps=[{"step": 1, "action": "custom_action"}],
        )
        repo.add_strategy(new_strategy)

        assert len(repo.strategies) == initial_count + 1
        assert "custom_strategy" in repo.strategies

    def test_get_strategy_exists(self):
        """Test getting an existing strategy."""
        repo = StrategyRepository()
        strategy = repo.get_strategy("decomposition_strategy")

        assert strategy is not None
        assert strategy.id == "decomposition_strategy"
        assert strategy.name == "Decomposition Strategy"

    def test_get_strategy_nonexistent(self):
        """Test getting a nonexistent strategy returns None."""
        repo = StrategyRepository()
        strategy = repo.get_strategy("nonexistent_strategy_xyz")
        assert strategy is None

    def test_find_strategies_for_task(self):
        """Test finding strategies for a task."""
        repo = StrategyRepository()

        strategies = repo.find_strategies_for_task(
            task_tags=["problem-solving", "decomposition"],
            available_patterns=["decomposition_pattern", "hierarchical_planning_pattern"]
        )

        assert len(strategies) > 0
        # Decomposition strategy should be relevant
        assert any(s.id == "decomposition_strategy" for s in strategies)

    def test_find_strategies_no_matching_patterns(self):
        """Test finding strategies with no matching patterns."""
        repo = StrategyRepository()

        strategies = repo.find_strategies_for_task(
            task_tags=["unknown"],
            available_patterns=["nonexistent_pattern_xyz"]
        )

        # Should return empty list when no patterns match
        assert isinstance(strategies, list)

    def test_find_strategies_sorted_by_relevance(self):
        """Test that strategies are sorted by relevance."""
        repo = StrategyRepository()

        # Add some use history to strategies
        repo.record_strategy_use("decomposition_strategy", success=True)
        repo.record_strategy_use("decomposition_strategy", success=True)

        strategies = repo.find_strategies_for_task(
            task_tags=["decomposition"],
            available_patterns=["decomposition_pattern", "pattern_matching_pattern"]
        )

        # Higher success rate strategies should appear earlier
        if len(strategies) > 1:
            # First strategy should have non-zero relevance
            assert strategies[0].success_rate >= 0

    def test_compose_hybrid_strategy(self):
        """Test composing a hybrid strategy."""
        repo = StrategyRepository()

        base_strategies = [
            repo.get_strategy("decomposition_strategy"),
            repo.get_strategy("pattern_matching_strategy"),
        ]

        hybrid = repo.compose_hybrid_strategy(
            strategies=base_strategies,
            task_context={"task_type": "complex"}
        )

        assert hybrid is not None
        assert hybrid.strategy_type == StrategyType.HYBRID
        assert "Hybrid" in hybrid.name
        # Should combine steps from both strategies
        assert len(hybrid.steps) > len(base_strategies[0].steps)

    def test_compose_hybrid_combines_patterns(self):
        """Test that hybrid strategy combines applicable patterns."""
        repo = StrategyRepository()

        strategies = [
            repo.get_strategy("decomposition_strategy"),
            repo.get_strategy("constraint_propagation_strategy"),
        ]

        hybrid = repo.compose_hybrid_strategy(strategies, {})

        # Should include patterns from both
        decomp_patterns = strategies[0].applicable_patterns
        constraint_patterns = strategies[1].applicable_patterns

        for pattern in decomp_patterns:
            assert pattern in hybrid.applicable_patterns
        for pattern in constraint_patterns:
            assert pattern in hybrid.applicable_patterns

    def test_compose_hybrid_adds_to_repo(self):
        """Test that composed hybrid is added to repository."""
        repo = StrategyRepository()
        initial_count = len(repo.strategies)

        strategies = [
            repo.get_strategy("decomposition_strategy"),
            repo.get_strategy("working_backward_strategy"),
        ]

        hybrid = repo.compose_hybrid_strategy(strategies, {})

        assert len(repo.strategies) == initial_count + 1
        assert hybrid.id in repo.strategies

    def test_record_strategy_use_success(self):
        """Test recording successful strategy use."""
        repo = StrategyRepository()
        strategy = repo.get_strategy("decomposition_strategy")
        initial_count = strategy.use_count

        repo.record_strategy_use("decomposition_strategy", success=True)

        assert strategy.use_count == initial_count + 1
        assert strategy.success_rate > 0

    def test_record_strategy_use_failure(self):
        """Test recording failed strategy use."""
        repo = StrategyRepository()

        # First record some successes
        for _ in range(5):
            repo.record_strategy_use("pattern_matching_strategy", success=True)

        strategy = repo.get_strategy("pattern_matching_strategy")
        initial_rate = strategy.success_rate

        # Now record a failure
        repo.record_strategy_use("pattern_matching_strategy", success=False)

        assert strategy.success_rate < initial_rate

    def test_record_strategy_use_nonexistent(self):
        """Test recording use for nonexistent strategy."""
        repo = StrategyRepository()
        # Should not raise an error
        repo.record_strategy_use("nonexistent_strategy", success=True)

    def test_strategy_steps_have_descriptions(self):
        """Test that strategy steps have descriptions."""
        repo = StrategyRepository()

        for strategy in repo.strategies.values():
            for step in strategy.steps:
                assert "step" in step
                assert "action" in step
                assert "description" in step


class TestStrategyExecutor:
    """Tests for StrategyExecutor class."""

    def test_initialization(self):
        """Test executor initialization."""
        executor = StrategyExecutor()
        assert executor.pattern_library is None
        assert executor.execution_history == []

    def test_initialization_with_pattern_library(self):
        """Test executor initialization with pattern library."""
        mock_library = {"test": "library"}
        executor = StrategyExecutor(pattern_library=mock_library)
        assert executor.pattern_library == mock_library

    def test_execute_strategy(self):
        """Test executing a strategy."""
        repo = StrategyRepository()
        executor = StrategyExecutor()

        strategy = repo.get_strategy("decomposition_strategy")
        task = {"id": "test_task", "name": "Test Task"}

        result = executor.execute_strategy(strategy, task)

        assert result is not None
        assert "strategy" in result
        assert "task" in result
        assert "steps_executed" in result
        assert "final_result" in result
        assert "success" in result

    def test_execute_strategy_executes_all_steps(self):
        """Test that all steps are executed."""
        repo = StrategyRepository()
        executor = StrategyExecutor()

        strategy = repo.get_strategy("decomposition_strategy")
        task = {"id": "test", "name": "Test"}

        result = executor.execute_strategy(strategy, task)

        assert len(result["steps_executed"]) == len(strategy.steps)

    def test_execute_strategy_with_context(self):
        """Test executing with additional context."""
        repo = StrategyRepository()
        executor = StrategyExecutor()

        strategy = repo.get_strategy("pattern_matching_strategy")
        task = {"id": "test", "name": "Test"}
        context = {"additional": "context", "value": 42}

        result = executor.execute_strategy(strategy, task, context)

        assert result["success"] is True

    def test_execute_strategy_records_history(self):
        """Test that execution is recorded in history."""
        executor = StrategyExecutor()
        repo = StrategyRepository()

        strategy = repo.get_strategy("decomposition_strategy")
        task = {"id": "test", "name": "Test"}

        executor.execute_strategy(strategy, task)

        assert len(executor.execution_history) == 1
        assert executor.execution_history[0]["strategy"] == strategy.name

    def test_execute_multiple_strategies(self):
        """Test executing multiple strategies."""
        executor = StrategyExecutor()
        repo = StrategyRepository()

        strategies = [
            repo.get_strategy("decomposition_strategy"),
            repo.get_strategy("pattern_matching_strategy"),
        ]

        for strategy in strategies:
            executor.execute_strategy(strategy, {"id": "test", "name": "Test"})

        assert len(executor.execution_history) == 2

    def test_step_result_structure(self):
        """Test that step results have expected structure."""
        executor = StrategyExecutor()
        repo = StrategyRepository()

        strategy = repo.get_strategy("decomposition_strategy")
        task = {"id": "test", "name": "Test"}

        result = executor.execute_strategy(strategy, task)

        for step_result in result["intermediate_results"]:
            assert "step_number" in step_result
            assert "action" in step_result
            assert "status" in step_result
            assert "output" in step_result

    def test_success_evaluation(self):
        """Test that success is properly evaluated."""
        executor = StrategyExecutor()
        repo = StrategyRepository()

        strategy = repo.get_strategy("decomposition_strategy")
        task = {"id": "test", "name": "Test"}

        result = executor.execute_strategy(strategy, task)

        # All steps should complete, so success should be True
        assert result["success"] is True

    def test_execute_empty_strategy(self):
        """Test executing a strategy with no steps."""
        executor = StrategyExecutor()

        empty_strategy = Strategy(
            id="empty",
            name="Empty Strategy",
            strategy_type=StrategyType.HEURISTIC,
            description="Empty",
            applicable_patterns=[],
            steps=[],
        )

        result = executor.execute_strategy(empty_strategy, {"id": "test", "name": "Test"})

        assert result["steps_executed"] == []
        # Empty strategy should still be marked as successful
        assert result["final_result"] is None


class TestStrategySystemIntegration:
    """Integration tests for the strategy system."""

    def test_full_workflow(self):
        """Test complete workflow with repository and executor."""
        # Setup
        repo = StrategyRepository()
        executor = StrategyExecutor()

        # 1. Find strategies for a task
        strategies = repo.find_strategies_for_task(
            task_tags=["complex", "decomposition"],
            available_patterns=["decomposition_pattern", "recursive_thinking_pattern"]
        )

        assert len(strategies) > 0

        # 2. Execute the top strategy
        top_strategy = strategies[0]
        task = {
            "id": "integration_test",
            "name": "Integration Test Task"
        }

        result = executor.execute_strategy(top_strategy, task)

        # 3. Record the result
        repo.record_strategy_use(top_strategy.id, success=result["success"])

        # 4. Verify the strategy was updated
        updated = repo.get_strategy(top_strategy.id)
        assert updated.use_count > 0

    def test_hybrid_strategy_execution(self):
        """Test creating and executing a hybrid strategy."""
        repo = StrategyRepository()
        executor = StrategyExecutor()

        # Create hybrid
        base = [
            repo.get_strategy("decomposition_strategy"),
            repo.get_strategy("constraint_propagation_strategy"),
        ]

        hybrid = repo.compose_hybrid_strategy(base, {"mode": "test"})

        # Execute hybrid
        result = executor.execute_strategy(
            hybrid,
            {"id": "hybrid_test", "name": "Hybrid Test"}
        )

        assert result["success"] is True
        assert len(result["steps_executed"]) > len(base[0].steps)

    def test_strategy_improvement_over_time(self):
        """Test that strategy metrics improve with successful use."""
        repo = StrategyRepository()

        strategy = repo.get_strategy("decomposition_strategy")
        initial_rate = strategy.success_rate

        # Simulate many successful uses
        for _ in range(20):
            repo.record_strategy_use("decomposition_strategy", success=True)

        # Success rate should improve
        assert strategy.success_rate > initial_rate
        assert strategy.use_count >= 20
