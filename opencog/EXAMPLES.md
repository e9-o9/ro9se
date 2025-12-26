# OpenCog Usage Examples

This document provides examples of using the OpenCog AI evaluation framework.

## Quick Start

### Analyze All Languages

Get a comprehensive analysis of all programming languages and their AI capabilities:

```bash
./opencog/bin/opencog-analyze
```

This will:
- Analyze all 970+ languages in the repository
- Categorize implementations across 10 AI domains
- Identify top languages by AI capability
- Generate the FrankenCog Integration Manifest
- Save detailed reports to `opencog/output/`

### Evaluate a Specific Language

To evaluate Python's AI capabilities:

```bash
./opencog/bin/opencog-eval-lang Python
```

Example output:
```
Language: Python
Total Tasks Implemented: 1194
AI-Related Tasks: 133
Category Coverage: 100.0%
Categories Covered: 10 / 10
```

### Evaluate an AI Category

To see which languages excel at machine learning:

```bash
./opencog/bin/opencog-eval-category machine_learning
```

This shows:
- Languages implementing machine learning tasks
- Top 20 languages ranked by task count
- Task patterns in the category

### Generate the FrankenCog Manifest

To generate the optimal language selection for each AI domain:

```bash
./opencog/bin/opencog-manifest
```

This creates the patchwork showing which language is best for each AI function.

### Generate Comprehensive Report

To generate a detailed markdown report:

```bash
./opencog/bin/opencog-report
```

This creates `opencog/output/transcendent-expression-report.md` with:
- Executive summary
- Top languages by AI capability
- Optimal language per AI domain
- Detailed category analysis
- Language specializations

## AI Categories

The OpenCog framework evaluates languages across these categories:

1. **symbolic_reasoning** - Logic, theorem proving, constraint satisfaction
2. **pattern_recognition** - Search algorithms, pattern matching, classification
3. **knowledge_representation** - Data structures, graphs, semantic networks
4. **machine_learning** - Statistical methods, optimization, neural networks
5. **natural_language** - String manipulation, parsing, text analysis
6. **planning_problem_solving** - Heuristic search, game playing, puzzle solving
7. **uncertainty_reasoning** - Probabilistic methods, Bayesian reasoning
8. **cognitive_architecture** - Concurrent systems, distributed computing, agents
9. **perception_motor** - Image processing, signal processing, robotics
10. **meta_learning** - Self-improvement, reflection, introspection

## Example Workflow

```bash
# 1. Get overall analysis
./opencog/bin/opencog-analyze

# 2. Evaluate your favorite languages
./opencog/bin/opencog-eval-lang Python
./opencog/bin/opencog-eval-lang Julia
./opencog/bin/opencog-eval-lang Haskell

# 3. Explore specific AI domains
./opencog/bin/opencog-eval-category symbolic_reasoning
./opencog/bin/opencog-eval-category machine_learning

# 4. Generate the FrankenCog patchwork
./opencog/bin/opencog-manifest

# 5. Create comprehensive report
./opencog/bin/opencog-report

# 6. View reports
cat opencog/output/transcendent-expression-report.md
```

## Output Files

All analysis results are saved to `opencog/output/`:

- `language-analysis.json` - Detailed analysis of all languages
- `frankencog-manifest.json` - Optimal language per AI category
- `transcendent-expression-report.md` - Comprehensive markdown report

## Understanding the Results

### Category Coverage

Languages are rated on category coverage (0-100%):
- **100%** - Implements tasks in all 10 AI categories
- **90%** - Implements tasks in 9 AI categories
- **80%** - Implements tasks in 8 AI categories
- etc.

### AI Tasks

The "AI Tasks" metric counts how many AI-categorized tasks a language implements.
Higher numbers indicate broader AI/AGI capability.

### Best Language

For each category, the "Best Language" is determined by:
- Number of tasks implemented in that category
- Breadth of coverage within the category
- Overall quality of implementations

## The FrankenCog Philosophy

OpenCog doesn't claim any single language is "best for AI". Instead, it recognizes
that each language excels at specific cognitive functions. The FrankenCog patchwork
represents an optimal synthesis where:

- C excels at perception and performance-critical ML
- Ada dominates concurrent/distributed systems
- Haskell shines in symbolic reasoning
- Python provides breadth across all categories
- FreeBASIC leads in meta-learning and self-reflection

This post-polyglot approach leverages the unique strengths each language was
conceived to express.
