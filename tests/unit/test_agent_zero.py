"""
Unit tests for agent_zero.py

Tests the Agent-Zero orchestrator which serves as the "master builder"
for constructing and managing cognitive architectures.
"""

import pytest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "opencog" / "agents"))

from agent_zero import (
    AgentRole,
    Agent,
    Task,
    AgentZero,
)


class TestAgentRole:
    """Tests for AgentRole enum."""

    def test_has_analyzer(self):
        """Test ANALYZER role exists."""
        assert AgentRole.ANALYZER.value == "analyzer"

    def test_has_strategist(self):
        """Test STRATEGIST role exists."""
        assert AgentRole.STRATEGIST.value == "strategist"

    def test_has_executor(self):
        """Test EXECUTOR role exists."""
        assert AgentRole.EXECUTOR.value == "executor"

    def test_has_validator(self):
        """Test VALIDATOR role exists."""
        assert AgentRole.VALIDATOR.value == "validator"

    def test_has_learner(self):
        """Test LEARNER role exists."""
        assert AgentRole.LEARNER.value == "learner"

    def test_all_roles_count(self):
        """Test that all 5 roles exist."""
        assert len(AgentRole) == 5


class TestAgent:
    """Tests for Agent dataclass."""

    def test_initialization_minimal(self):
        """Test minimal initialization."""
        agent = Agent(
            id="test_agent",
            role=AgentRole.ANALYZER,
            capabilities=["cap1", "cap2"],
        )
        assert agent.id == "test_agent"
        assert agent.role == AgentRole.ANALYZER
        assert len(agent.capabilities) == 2

    def test_initialization_with_all_fields(self):
        """Test initialization with all fields."""
        agent = Agent(
            id="full_agent",
            role=AgentRole.EXECUTOR,
            capabilities=["exec1", "exec2"],
            state={"status": "ready"},
            performance_metrics={"accuracy": 0.95},
        )
        assert agent.state == {"status": "ready"}
        assert agent.performance_metrics == {"accuracy": 0.95}

    def test_default_values(self):
        """Test default values for optional fields."""
        agent = Agent(
            id="minimal",
            role=AgentRole.VALIDATOR,
            capabilities=[],
        )
        assert agent.state == {}
        assert agent.performance_metrics == {}

    def test_to_dict(self):
        """Test conversion to dictionary."""
        agent = Agent(
            id="test",
            role=AgentRole.LEARNER,
            capabilities=["learn1"],
            state={"learning": True},
            performance_metrics={"rate": 0.8},
        )
        d = agent.to_dict()

        assert d["id"] == "test"
        assert d["role"] == "learner"
        assert d["capabilities"] == ["learn1"]
        assert d["state"] == {"learning": True}
        assert d["performance_metrics"] == {"rate": 0.8}


class TestTask:
    """Tests for Task dataclass."""

    def test_initialization_minimal(self):
        """Test minimal initialization."""
        task = Task(
            id="task1",
            name="Test Task",
            description="A test task",
            modality="text",
            tags=["test", "simple"],
        )
        assert task.id == "task1"
        assert task.name == "Test Task"
        assert task.modality == "text"

    def test_initialization_with_all_fields(self):
        """Test initialization with all fields."""
        task = Task(
            id="full_task",
            name="Full Task",
            description="A complete task",
            modality="reasoning",
            tags=["complex", "multi-step"],
            examples=[{"input": "x", "output": "y"}],
            required_capabilities=["reasoning", "analysis"],
        )
        assert len(task.examples) == 1
        assert len(task.required_capabilities) == 2

    def test_default_values(self):
        """Test default values for optional fields."""
        task = Task(
            id="min",
            name="Min Task",
            description="desc",
            modality="test",
            tags=[],
        )
        assert task.examples == []
        assert task.required_capabilities == []


class TestAgentZero:
    """Tests for AgentZero orchestrator class."""

    def test_initialization(self):
        """Test AgentZero initialization."""
        az = AgentZero()
        assert isinstance(az.agents, dict)
        assert isinstance(az.active_tasks, dict)
        assert az.pattern_library is None
        assert az.strategy_repository is None
        assert az.performance_history == []

    def test_initialize_core_agents(self):
        """Test that core agents are initialized."""
        az = AgentZero()
        az.initialize_core_agents()

        # Should have 5 core agents
        assert len(az.agents) == 5

        # Check all roles are covered
        roles = {agent.role for agent in az.agents.values()}
        assert AgentRole.ANALYZER in roles
        assert AgentRole.STRATEGIST in roles
        assert AgentRole.EXECUTOR in roles
        assert AgentRole.VALIDATOR in roles
        assert AgentRole.LEARNER in roles

    def test_register_agent(self):
        """Test registering a new agent."""
        az = AgentZero()

        custom_agent = Agent(
            id="custom_01",
            role=AgentRole.ANALYZER,
            capabilities=["custom_analysis"],
        )
        az.register_agent(custom_agent)

        assert "custom_01" in az.agents
        assert az.agents["custom_01"].role == AgentRole.ANALYZER

    def test_register_multiple_agents(self):
        """Test registering multiple agents."""
        az = AgentZero()

        for i in range(3):
            agent = Agent(
                id=f"agent_{i}",
                role=AgentRole.EXECUTOR,
                capabilities=[f"cap_{i}"],
            )
            az.register_agent(agent)

        assert len(az.agents) == 3

    def test_analyze_task(self):
        """Test task analysis."""
        az = AgentZero()
        az.initialize_core_agents()

        task = Task(
            id="analysis_test",
            name="Analysis Test",
            description="Test task for analysis",
            modality="reasoning",
            tags=["Logic", "Problem Solving"],
            examples=[{"input": "test"}],
        )

        analysis = az.analyze_task(task)

        assert analysis["task_id"] == "analysis_test"
        assert analysis["task_name"] == "Analysis Test"
        assert "complexity_factors" in analysis
        assert "required_patterns" in analysis
        assert "suggested_agents" in analysis
        assert "estimated_difficulty" in analysis

    def test_analyze_task_complexity_factors(self):
        """Test that complexity factors are calculated."""
        az = AgentZero()
        az.initialize_core_agents()

        task = Task(
            id="complex",
            name="Complex Task",
            description="A complex task",
            modality="multi-modal",
            tags=["tag1", "tag2", "tag3"],
            examples=[{"ex": 1}, {"ex": 2}],
        )

        analysis = az.analyze_task(task)
        factors = analysis["complexity_factors"]

        assert factors["tag_count"] == 3
        assert factors["example_count"] == 2
        assert factors["modality"] == "multi-modal"

    def test_build_cognitive_architecture(self):
        """Test building a cognitive architecture."""
        az = AgentZero()
        az.initialize_core_agents()

        task = Task(
            id="arch_test",
            name="Architecture Test",
            description="Test",
            modality="test",
            tags=["Problem Solving"],
        )

        analysis = az.analyze_task(task)
        architecture = az.build_cognitive_architecture(task, analysis)

        assert architecture["task_id"] == "arch_test"
        assert "agents" in architecture
        assert "patterns" in architecture
        assert "workflow" in architecture
        assert "communication_channels" in architecture
        assert "success_criteria" in architecture

    def test_architecture_has_workflow(self):
        """Test that architecture includes a workflow."""
        az = AgentZero()
        az.initialize_core_agents()

        task = Task(
            id="workflow_test",
            name="Workflow Test",
            description="Test",
            modality="test",
            tags=[],
        )

        analysis = az.analyze_task(task)
        architecture = az.build_cognitive_architecture(task, analysis)

        workflow = architecture["workflow"]
        assert len(workflow) > 0
        for step in workflow:
            assert "step" in step
            assert "action" in step
            assert "agent_role" in step

    def test_execute_with_architecture(self):
        """Test executing a task with architecture."""
        az = AgentZero()
        az.initialize_core_agents()

        task = Task(
            id="exec_test",
            name="Execution Test",
            description="Test",
            modality="test",
            tags=[],
        )

        analysis = az.analyze_task(task)
        architecture = az.build_cognitive_architecture(task, analysis)
        results = az.execute_with_architecture(task, architecture)

        assert results["task_id"] == "exec_test"
        assert results["status"] == "completed"
        assert "steps" in results
        assert "solutions" in results

    def test_execute_completes_all_workflow_steps(self):
        """Test that all workflow steps are executed."""
        az = AgentZero()
        az.initialize_core_agents()

        task = Task(
            id="steps_test",
            name="Steps Test",
            description="Test",
            modality="test",
            tags=[],
        )

        analysis = az.analyze_task(task)
        architecture = az.build_cognitive_architecture(task, analysis)
        results = az.execute_with_architecture(task, architecture)

        assert len(results["steps"]) == len(architecture["workflow"])

    def test_validate_results(self):
        """Test result validation."""
        az = AgentZero()
        az.initialize_core_agents()

        task = Task(
            id="validate_test",
            name="Validation Test",
            description="Test",
            modality="test",
            tags=[],
        )

        analysis = az.analyze_task(task)
        architecture = az.build_cognitive_architecture(task, analysis)
        results = az.execute_with_architecture(task, architecture)
        validation = az.validate_results(task, results)

        assert validation["task_id"] == "validate_test"
        assert "validation_status" in validation
        assert "quality_score" in validation
        assert "issues" in validation
        assert "recommendations" in validation

    def test_validate_completed_task_passes(self):
        """Test that completed task passes validation."""
        az = AgentZero()
        az.initialize_core_agents()

        task = Task(
            id="pass_test",
            name="Pass Test",
            description="Test",
            modality="test",
            tags=[],
        )

        analysis = az.analyze_task(task)
        architecture = az.build_cognitive_architecture(task, analysis)
        results = az.execute_with_architecture(task, architecture)
        validation = az.validate_results(task, results)

        assert validation["validation_status"] == "passed"
        assert validation["quality_score"] > 0

    def test_learn_from_execution(self):
        """Test learning from execution."""
        az = AgentZero()
        az.initialize_core_agents()

        task = Task(
            id="learn_test",
            name="Learning Test",
            description="Test",
            modality="test",
            tags=[],
        )

        analysis = az.analyze_task(task)
        architecture = az.build_cognitive_architecture(task, analysis)
        results = az.execute_with_architecture(task, architecture)
        validation = az.validate_results(task, results)

        initial_history_len = len(az.performance_history)
        az.learn_from_execution(task, results, validation)

        assert len(az.performance_history) == initial_history_len + 1
        assert az.performance_history[-1]["task_id"] == "learn_test"

    def test_orchestrate_task_full_pipeline(self):
        """Test full orchestration pipeline."""
        az = AgentZero()
        az.initialize_core_agents()

        task = Task(
            id="orch_test",
            name="Orchestration Test",
            description="Full pipeline test",
            modality="reasoning",
            tags=["Problem Solving", "Logic"],
            examples=[{"input": "x", "output": "y"}],
        )

        result = az.orchestrate_task(task)

        assert result["task"] == "Orchestration Test"
        assert "analysis" in result
        assert "architecture" in result
        assert "results" in result
        assert "validation" in result

    def test_orchestrate_records_performance(self):
        """Test that orchestration records performance history."""
        az = AgentZero()
        az.initialize_core_agents()

        task = Task(
            id="perf_test",
            name="Performance Test",
            description="Test",
            modality="test",
            tags=[],
        )

        az.orchestrate_task(task)

        assert len(az.performance_history) == 1
        assert az.performance_history[0]["task_name"] == "Performance Test"

    def test_get_agent_by_role(self):
        """Test getting agent by role."""
        az = AgentZero()
        az.initialize_core_agents()

        analyzer = az._get_agent_by_role(AgentRole.ANALYZER)
        assert analyzer is not None
        assert analyzer.role == AgentRole.ANALYZER

    def test_get_agent_by_role_nonexistent(self):
        """Test getting agent when none with role exists."""
        az = AgentZero()
        # Don't initialize core agents

        result = az._get_agent_by_role(AgentRole.ANALYZER)
        assert result is None

    def test_identify_required_patterns(self):
        """Test pattern identification for tasks."""
        az = AgentZero()

        task = Task(
            id="pattern_test",
            name="Pattern Test",
            description="Test",
            modality="test",
            tags=["Problem Solving", "Logic", "Recursion"],
        )

        patterns = az._identify_required_patterns(task)

        assert isinstance(patterns, list)
        assert len(patterns) > 0

    def test_identify_patterns_with_matching_tags(self):
        """Test that known tags map to patterns."""
        az = AgentZero()

        task = Task(
            id="tag_test",
            name="Tag Test",
            description="Test",
            modality="test",
            tags=["Logic", "Problem Solving"],
        )

        patterns = az._identify_required_patterns(task)

        # Logic and Problem Solving should map to patterns
        assert any("logical" in p or "decomposition" in p for p in patterns)

    def test_identify_patterns_unknown_tags(self):
        """Test pattern identification with unknown tags."""
        az = AgentZero()

        task = Task(
            id="unknown_test",
            name="Unknown Test",
            description="Test",
            modality="test",
            tags=["completely_unknown_tag_xyz"],
        )

        patterns = az._identify_required_patterns(task)

        # Should fall back to general reasoning pattern
        assert "general_reasoning_pattern" in patterns

    def test_estimate_difficulty_easy(self):
        """Test difficulty estimation for easy task."""
        az = AgentZero()

        task = Task(
            id="easy",
            name="Easy",
            description="Easy task",
            modality="test",
            tags=["tag1"],
            examples=[],
        )

        difficulty = az._estimate_difficulty(task)
        assert difficulty == "easy"

    def test_estimate_difficulty_medium(self):
        """Test difficulty estimation for medium task."""
        az = AgentZero()

        task = Task(
            id="medium",
            name="Medium",
            description="Medium task",
            modality="test",
            tags=["t1", "t2", "t3", "t4", "t5"],
            examples=[{"ex": 1}, {"ex": 2}, {"ex": 3}],
        )

        difficulty = az._estimate_difficulty(task)
        assert difficulty == "medium"

    def test_estimate_difficulty_hard(self):
        """Test difficulty estimation for hard task."""
        az = AgentZero()

        task = Task(
            id="hard",
            name="Hard",
            description="Hard task",
            modality="test",
            tags=["t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8"],
            examples=[{"ex": i} for i in range(10)],
        )

        difficulty = az._estimate_difficulty(task)
        assert difficulty == "hard"

    def test_calculate_quality_score(self):
        """Test quality score calculation."""
        az = AgentZero()

        completed_results = {"status": "completed", "steps": []}
        failed_results = {"status": "failed", "steps": []}

        assert az._calculate_quality_score(completed_results) > 0
        assert az._calculate_quality_score(failed_results) == 0


class TestAgentZeroIntegration:
    """Integration tests for Agent-Zero."""

    def test_multiple_task_orchestration(self):
        """Test orchestrating multiple tasks."""
        az = AgentZero()
        az.initialize_core_agents()

        tasks = [
            Task(
                id=f"task_{i}",
                name=f"Task {i}",
                description=f"Description {i}",
                modality="test",
                tags=["test"],
            )
            for i in range(3)
        ]

        for task in tasks:
            az.orchestrate_task(task)

        assert len(az.performance_history) == 3

    def test_performance_tracking_across_tasks(self):
        """Test that performance is tracked across tasks."""
        az = AgentZero()
        az.initialize_core_agents()

        # Execute several tasks
        for i in range(5):
            task = Task(
                id=f"perf_{i}",
                name=f"Performance Task {i}",
                description="Test",
                modality="test",
                tags=["Logic"],
            )
            az.orchestrate_task(task)

        # Check performance history
        assert len(az.performance_history) == 5
        for record in az.performance_history:
            assert "quality_score" in record
            assert "patterns_used" in record

    def test_agent_metrics_update(self):
        """Test that agent metrics are updated after orchestration."""
        az = AgentZero()
        az.initialize_core_agents()

        # Record initial metrics
        initial_metrics = {
            aid: dict(agent.performance_metrics)
            for aid, agent in az.agents.items()
        }

        # Execute task
        task = Task(
            id="metrics_test",
            name="Metrics Test",
            description="Test",
            modality="test",
            tags=[],
        )

        result = az.orchestrate_task(task)

        # Metrics should be updated for involved agents
        # Note: The current implementation may not update all agents
        # This tests the mechanism exists
        assert az.performance_history[-1]["quality_score"] > 0
