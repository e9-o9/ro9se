# OpenCog Multi-Agent Reasoning System

## Overview

The OpenCog framework in RosettaCog now includes a sophisticated multi-agent orchestration workbench for solving LLM reasoning tasks. This integration brings together the best insights from the [OpenCog-Reasoning-Tasks](https://github.com/cogpy/OpenCog-Reasoning-Tasks) repository with RosettaCog's language evaluation capabilities.

## Key Features

### 1. Agent-Zero Master Builder

Agent-Zero is the master orchestrator that coordinates cognitive architectures to solve reasoning challenges:

- **5 Specialized Agents** with distinct roles:
  - **Analyzer**: Task decomposition, requirement analysis, pattern matching
  - **Strategist**: Strategy design, cognitive pattern selection, optimization
  - **Executor**: Reasoning, problem solving, computation
  - **Validator**: Solution validation, correctness checking, quality assessment
  - **Learner**: Pattern learning, performance analysis, adaptation

- **Orchestration Pipeline**:
  1. Analyze task requirements and complexity
  2. Build cognitive architecture with appropriate patterns and agents
  3. Execute task with coordinated agent collaboration
  4. Validate solution quality
  5. Learn from outcomes to improve future performance

### 2. Pattern Language Library

Inspired by Christopher Alexander's "A Pattern Language," this living library contains 10 foundational cognitive patterns for AGI:

1. **Problem Decomposition** - Break complex problems into manageable parts
2. **Recursive Thinking** - Apply problem-solving recursively
3. **Pattern Recognition** - Identify and match patterns
4. **Logical Inference** - Apply logical rules systematically
5. **Abstraction** - Extract general principles from specifics
6. **Working Backward** - Start from goal and work backward
7. **Metacognitive Monitoring** - Monitor and adjust approach
8. **Analogical Transfer** - Apply solutions from similar problems
9. **Constraint Satisfaction** - Satisfy multiple constraints
10. **Causal Reasoning** - Understand cause-effect relationships

Each pattern includes:
- **Context**: When/where the pattern applies
- **Problem**: The recurring problem it solves
- **Forces**: Conflicting considerations to balance
- **Solution**: The proven solution approach
- **Examples**: Concrete usage examples
- **Related Patterns**: Connections to other patterns
- **Quality Level**: Experimental → Proven → Mature → Foundational

### 3. Multi-Agent Orchestration

Three orchestration modes for different needs:

- **Autonomous Mode**: Agent-Zero fully controls the process with minimal intervention
- **Collaborative Mode**: Multiple agents work together with coordination and communication
- **Guided Mode**: Provides analysis and recommendations for user guidance

Features:
- Message bus for inter-agent communication
- Task queue with pending/active/completed tracking
- Agent coordinator with capability matching
- Load balancing across agents

### 4. Strategy Repository

7 core reasoning strategies that can be composed and adapted:

1. **Decomposition Strategy** (Systematic) - Break down complex problems
2. **Pattern Matching Strategy** (Analogical) - Find and apply similar solutions
3. **Constraint Propagation Strategy** (Systematic) - Propagate constraints through problem space
4. **Working Backward Strategy** (Heuristic) - Start from desired end state
5. **Hypothesis Testing Strategy** (Abductive) - Generate and test hypotheses
6. **Recursive Refinement Strategy** (Systematic) - Iteratively refine solutions
7. **Creative Exploration Strategy** (Creative) - Explore unconventional approaches

Features:
- Hybrid strategy composition
- Performance tracking and adaptation
- Step-by-step execution
- Success rate monitoring

### 5. Reasoning Tasks

95 comprehensive LLM reasoning challenges covering:

- **Logic & Reasoning**: Deductive logic, syllogisms, truth tables
- **Analogical Reasoning**: Pattern transfer, metaphor interpretation
- **Causal Analysis**: Cause-effect relationships, counterfactuals
- **Problem Solving**: Lateral thinking, creative solutions
- **Theory of Mind**: False beliefs, perspective taking, emotional inference
- **Ethical Reasoning**: Moral dilemmas, ethical analysis
- **Spatial Reasoning**: Mental rotation, spatial relationships
- **Probabilistic Reasoning**: Risk assessment, uncertainty quantification
- **And many more...**

## Quick Start

### Using Python API

```python
from opencog import OpenCogWorkbench

# Initialize the workbench
workbench = OpenCogWorkbench(tasks_dir="opencog/reasoning-tasks")

# Get statistics
stats = workbench.get_statistics()
print(f"Loaded {stats['tasks']['total']} reasoning tasks")
print(f"Available patterns: {stats['patterns']['total']}")
print(f"Available strategies: {stats['strategies']['total']}")

# Get recommendations for a task
recommendations = workbench.get_task_recommendations("analogical-problem-solving")
print("\nRecommended patterns:")
for pattern in recommendations['recommended_patterns'][:3]:
    print(f"  - {pattern['name']} (Quality: {pattern['quality']})")

# Solve a task autonomously
result = workbench.solve_task(
    "analogical-problem-solving",
    orchestration_mode="autonomous"
)
print(f"\nSuccess: {result['success']}")
print(f"Patterns used: {result['patterns_used']}")
```

### Using Command-Line Tools

```bash
# Run the interactive demo
python3 opencog/opencog_reasoning_demo.py

# Analyze reasoning tasks and get statistics
opencog/bin/opencog-reasoning

# Get recommendations for a specific task
opencog/bin/opencog-reasoning analogical-problem-solving

# Solve a task with Agent-Zero
opencog/bin/opencog-agent-zero analogical-problem-solving autonomous

# Try different orchestration modes
opencog/bin/opencog-agent-zero deductive-logic-puzzles collaborative
opencog/bin/opencog-agent-zero ethical-dilemma-resolution guided
```

## Demo Script

The `opencog_reasoning_demo.py` script provides 6 interactive demonstrations:

1. **Workbench Initialization** - System overview and statistics
2. **Pattern Language Library** - Complete catalog with detailed examples
3. **Agent-Zero** - Agent capabilities and task analysis
4. **Multi-Agent Orchestration** - Collaborative task solving
5. **Pattern Evolution** - Learning from usage and experience
6. **Task Solving** - End-to-end autonomous reasoning

Run it with:
```bash
python3 opencog/opencog_reasoning_demo.py
```

## Architecture

```
opencog/
├── __init__.py                    # Main module exports
├── workbench.py                   # OpenCogWorkbench - main interface
├── opencog_reasoning_demo.py      # Interactive demo script
│
├── agents/
│   ├── __init__.py
│   └── agent_zero.py              # Agent-Zero master builder
│
├── patterns/
│   ├── __init__.py
│   └── pattern_library.py         # Pattern language library
│
├── orchestration/
│   ├── __init__.py
│   └── multi_agent.py             # Multi-agent coordination
│
├── strategies/
│   ├── __init__.py
│   └── strategy_system.py         # Strategy repository and executor
│
├── reasoning-tasks/               # 95 reasoning task definitions
│   ├── analogical-problem-solving.md
│   ├── deductive-logic-puzzles.md
│   ├── ethical-dilemma-resolution.md
│   └── ... (92 more tasks)
│
└── bin/                           # Command-line tools
    ├── opencog-reasoning          # Analyze reasoning tasks
    └── opencog-agent-zero         # Orchestrate tasks with Agent-Zero
```

## Integration with Language Evaluation

The multi-agent reasoning system complements RosettaCog's existing language evaluation capabilities:

1. **Language Capabilities** → Identifies which languages excel at different AI tasks
2. **Reasoning Tasks** → Provides concrete challenges to test reasoning capabilities
3. **Agent-Zero** → Orchestrates optimal approaches combining multiple paradigms
4. **Pattern Library** → Captures reusable cognitive strategies across languages
5. **FrankenCog Synthesis** → Creates optimal multi-language solutions for complex reasoning

Example workflow:
```python
from opencog import OpenCogWorkbench
from opencog.lib.opencog_analyzer import OpenCogAnalyzer

# Analyze which languages are best at specific AI categories
analyzer = OpenCogAnalyzer(".")
lang_capabilities = analyzer.get_language_tasks("Python")

# Use reasoning tasks to evaluate language-specific approaches
workbench = OpenCogWorkbench(tasks_dir="opencog/reasoning-tasks")
result = workbench.solve_task("deductive-logic-puzzles")

# Combine insights for FrankenCog integration manifest
```

## Advanced Usage

### Creating Custom Patterns

```python
from opencog.patterns.pattern_library import CognitivePattern, PatternCategory, PatternQuality

custom_pattern = CognitivePattern(
    id="my_pattern",
    name="My Custom Pattern",
    category=PatternCategory.PROBLEM_SOLVING,
    context="When facing a specific type of challenge...",
    problem="The recurring problem to solve...",
    forces=["Force 1", "Force 2", "Force 3"],
    solution="The solution approach...",
    examples=[{"task": "Example", "application": "How it applies"}],
    quality=PatternQuality.EXPERIMENTAL
)

workbench.pattern_library.add_pattern(custom_pattern)
```

### Creating Custom Strategies

```python
from opencog.strategies.strategy_system import Strategy, StrategyType

custom_strategy = Strategy(
    id="my_strategy",
    name="My Custom Strategy",
    strategy_type=StrategyType.HYBRID,
    description="Strategy description...",
    applicable_patterns=["pattern1", "pattern2"],
    steps=["Step 1", "Step 2", "Step 3"]
)

workbench.strategy_repository.add_strategy(custom_strategy)
```

### Multi-Agent Orchestration

```python
from opencog.agents.agent_zero import Task

# Create a custom task
task = Task(
    id="custom_task",
    name="Custom Reasoning Task",
    description="Description of the task...",
    modality="Text only",
    tags=["Reasoning", "Logic", "Problem Solving"]
)

# Create orchestration session
session_id = "session_001"
agent_ids = list(workbench.agent_zero.agents.keys())
workbench.orchestrator.create_session(session_id, agent_ids)

# Orchestrate with different strategies
result = workbench.orchestrator.orchestrate_multi_agent_task(
    session_id,
    {
        "id": task.id,
        "name": task.name,
        "description": task.description,
        "required_capabilities": ["reasoning", "problem_solving"]
    },
    strategy="collaborative"  # or "competitive" or "sequential"
)
```

## Pattern Evolution

Patterns evolve with usage and experience:

```python
# Record pattern usage
workbench.pattern_library.record_pattern_use("decomposition_pattern", success=True)

# Add new examples
new_example = {
    "task": "Complex system analysis",
    "application": "Decomposed system into components, analyzed each separately"
}
workbench.pattern_library.evolve_pattern("decomposition_pattern", new_example)

# Save pattern library for persistence
workbench.pattern_library.save_library("my_patterns.json")

# Load pattern library
workbench.pattern_library.load_library("my_patterns.json")
```

## Performance Tracking

The system tracks performance metrics automatically:

```python
# Get performance statistics
stats = workbench.get_statistics()

print(f"Total executions: {stats['performance']['executions']}")
print(f"Average success rate: {stats['performance']['avg_success_rate']}")

# Pattern-specific metrics
for pattern_id, pattern in workbench.pattern_library.patterns.items():
    print(f"{pattern.name}:")
    print(f"  Use count: {pattern.use_count}")
    print(f"  Success rate: {pattern.success_rate:.2f}")
    print(f"  Quality: {pattern.quality.value}")
```

## Future Enhancements

Potential areas for extension:

1. **LLM Integration** - Connect to actual LLM APIs for execution
2. **Distributed Systems** - Multi-node agent coordination
3. **Advanced Pattern Mining** - Automatically discover patterns from solutions
4. **Strategy Synthesis** - Generate new strategies from successful compositions
5. **Visual Pattern Browser** - Interactive exploration of pattern language
6. **Real-time Collaboration** - Human-AI co-working on reasoning tasks
7. **Benchmarking Suite** - Systematic evaluation across tasks
8. **Cross-Language Integration** - Combine with RosettaCog language analysis

## Contributing

To contribute new patterns, strategies, or tasks:

1. Follow the existing structure and format
2. Add appropriate metadata and examples
3. Test with the demo script
4. Document usage and relationships
5. Submit with clear description of benefits

## License

This integration maintains the Apache 2.0 License from both source projects.

## Citation

```bibtex
@software{rosettacog_opencog2024,
  title = {RosettaCog OpenCog Multi-Agent Reasoning Integration},
  author = {RosettaCog Contributors and OpenCog-Reasoning-Tasks Contributors},
  url = {https://github.com/cogpy/RosettaCog},
  year = {2024},
  note = {Integration of multi-agent orchestration and pattern language for AGI cognitive architectures}
}
```

## See Also

- [OpenCog Language Evaluation](opencog/README.md)
- [Original OpenCog-Reasoning-Tasks](https://github.com/cogpy/OpenCog-Reasoning-Tasks)
- [RosettaCode Data Project](https://github.com/acmeism/RosettaCodeData)
