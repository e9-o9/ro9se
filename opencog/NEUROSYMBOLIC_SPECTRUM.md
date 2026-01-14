# Neuro-Symbolic Spectrum Analysis

## Overview

The neuro-symbolic spectrum analysis is a framework for understanding how programming languages balance **neural (continuous field)** and **symbolic (discrete collapse)** computation. This framework, based on the theoretical foundation in `neuro-symbo.md`, provides quantitative classification of languages along a spectrum from pure neural to pure symbolic computation.

## Theoretical Foundation

### The Neuro-Symbolic Spectrum

Programming languages differ primarily in **how long they preserve possibility before collapsing it into commitment**. This creates a spectrum:

```
Continuous field  ←────────────────────────→  Discrete collapse
(neural)                                           (symbolic)
```

**Key Question:** Does a language preserve *potential* during computation, or does it force early commitment?

### Core Insight

**Parentheses are topological markers** - languages that preserve parentheses (explicit nesting structure) tend to preserve the neural side of computation longer. This is not syntax sugar; it's fundamental to how the language handles the passage from field → collapse.

## Spectrum Positions

The framework defines 7 positions on the spectrum:

1. **Neural Extreme** (score ≥ 0.9)
   - Maximum field preservation
   - Bulk transformations, continuous structures
   - Examples: Tensor DSLs

2. **Neural Leaning** (score ≥ 0.7)
   - Strong field orientation
   - Array operations, dataflow
   - Examples: APL, J, Julia

3. **Balanced Neural** (score ≥ 0.55)
   - Slight neural bias
   - Constraint-based, search-oriented
   - Examples: Prolog, Datalog

4. **Equilibrium** (0.45 ≤ score < 0.55)
   - Perfect balance
   - Deferred evaluation, explicit topology
   - Examples: Lisp, Scheme, Racket

5. **Balanced Symbolic** (0.3 ≤ score < 0.45)
   - Slight symbolic bias
   - Expression-based but strict
   - Examples: Haskell, ML

6. **Symbolic Leaning** (0.1 ≤ score < 0.3)
   - Strong collapse orientation
   - State mutation, linear execution
   - Examples: C, Rust, Java, Python

7. **Symbolic Extreme** (score < 0.1)
   - Maximum collapse
   - No abstraction, immediate commitment
   - Examples: Assembly, machine code

## Classification Features

Languages are evaluated on 6 key features (each scored 0.0-1.0):

### 1. Structure Preservation
- **0.0**: Linear, flattened structure
- **1.0**: Nested, geometric structure
- Measures: Does the language maintain hierarchical relationships explicitly?

### 2. Meaning Distribution
- **0.0**: Localized meaning in single points
- **1.0**: Distributed meaning across structures
- Measures: Can meaning be spread across multiple elements?

### 3. Evaluation Deferral
- **0.0**: Immediate execution
- **1.0**: Deferred, lazy evaluation
- Measures: Can computation be postponed?

### 4. Control Model
- **0.0**: Command-based (do this, then that)
- **1.0**: Constraint-based (satisfy these conditions)
- Measures: How is flow of control expressed?

### 5. Syntactic Topology
- **0.0**: Statement-based, linear syntax
- **1.0**: Parenthesized, explicit nesting
- Measures: Are topological relationships visible?

### 6. Semantic Model
- **0.0**: State updating (mutation)
- **1.0**: Field shaping (transformation)
- Measures: How are changes expressed?

## Key Language Profiles

### Lisp/Scheme (Equilibrium → Neural Extreme)
```
Structure:    1.0  (explicit tree structure)
Meaning:      0.9  (code as data)
Evaluation:   1.0  (lazy evaluation possible)
Control:      0.8  (expression-based)
Topology:     1.0  (parentheses preserve structure)
Semantic:     0.9  (field reshaping via macros)
Neural Score: 0.93
```

**Why equilibrium?** Preserves possibility space through deferred evaluation, maintains explicit topology, allows field reshaping before collapse. Perfect for meta-programming and language generation.

### Prolog (Balanced Neural)
```
Structure:    0.7  (tree-based)
Meaning:      0.8  (unification)
Evaluation:   0.8  (backtracking preserves possibilities)
Control:      0.9  (constraint-based)
Topology:     0.6  (Horn clauses)
Semantic:     0.8  (search over possibilities)
Neural Score: 0.77
```

**Why balanced neural?** Symbolic syntax sitting on continuous possibility landscape. Backtracking = preserving multiple possibilities; unification = field consistency.

### C (Symbolic Leaning)
```
Structure:    0.2  (linear, sequential)
Meaning:      0.2  (localized state)
Evaluation:   0.1  (immediate execution)
Control:      0.1  (command-based)
Topology:     0.2  (statement-based)
Semantic:     0.1  (state mutation)
Neural Score: 0.15
```

**Why symbolic?** Collapses alternatives immediately through mutation, assumes one path/state/history, privileges linear time, forces early commitment.

## Paradigm-Spectrum Correlation

Analysis across 970 languages reveals strong paradigm correlations:

| Paradigm | Avg Neural Score | Characteristics |
|----------|-----------------|-----------------|
| Functional | 0.657 | Deferred evaluation, expression-based |
| Logic | 0.589 | Constraint satisfaction, backtracking |
| Scientific | 0.523 | Array operations, bulk transformations |
| Multi-paradigm | 0.500 | Balance of approaches |
| Scripting | 0.446 | Dynamic but imperative core |
| Object-oriented | 0.352 | Encapsulation with mutation |
| Imperative | 0.215 | State-based, command-driven |
| System | 0.188 | Close to hardware, immediate |

## Practical Applications

### 1. Language Selection

**Problem:** Need to implement a constraint satisfaction system

**Analysis:**
- Requires preserving multiple possibilities
- Needs backtracking capability
- Benefits from constraint-based control

**Recommendation:** Choose logic programming (Prolog) - balanced neural position ideal for maintaining search space.

### 2. Problem-Language Matching

Match problem characteristics to spectrum position:

- **Need flexibility/exploration?** → Neural-leaning (preserve possibilities)
- **Need deterministic execution?** → Symbolic-leaning (force commitment)
- **Need meta-programming?** → Equilibrium (structural manipulation)
- **Need performance?** → Symbolic-extreme (direct hardware)

### 3. Polyglot Architecture Design

Design systems that leverage spectrum diversity:

```
Neural Tier (exploration):     Lisp for strategy generation
Balanced Tier (reasoning):     Prolog for constraint solving
Symbolic Tier (execution):     C for performance-critical paths
```

## Integration with OpenCog

### Language Profiles

The `generate_language_profile()` method now includes spectrum analysis:

```python
from opencog.lib.opencog_analyzer import OpenCogAnalyzer

analyzer = OpenCogAnalyzer('.')
profile = analyzer.generate_language_profile('Lisp', include_spectrum=True)

print(profile['spectrum']['position'])        # 'neural_extreme'
print(profile['spectrum']['neural_score'])    # 0.933
print(profile['spectrum']['characteristics']) # homoiconic, etc.
```

### Hypergraph Analysis

The hypergraph now includes spectrum data:

```python
from opencog.lib.hypergraph_analyzer import HypergraphAnalyzer

analyzer = HypergraphAnalyzer('.')

# Generate full spectrum analysis
spectrum_data = analyzer.generate_spectrum_analysis()

# Get distribution
print(spectrum_data['distribution'])

# Get paradigm correlations
print(spectrum_data['paradigm_spectrum_correlation'])

# Print report
analyzer.print_spectrum_report()
```

### Correlations Revealed

Spectrum analysis integrated with hypergraph reveals:

1. **Paradigm-spectrum correlation**: Functional languages cluster at equilibrium/neural-leaning
2. **Subcategory-spectrum correlation**: Logic tasks correlate with balanced-neural languages
3. **Task complexity patterns**: More complex AI tasks benefit from neural-leaning features

## CLI Usage

### Basic Analysis

```bash
# Analyze single language
opencog/bin/opencog-neurosymbolic --language Lisp

# Show all languages with distribution
opencog/bin/opencog-neurosymbolic --all --distribution

# Find languages at specific position
opencog/bin/opencog-neurosymbolic --position equilibrium

# Verbose analysis
opencog/bin/opencog-neurosymbolic --language Haskell --verbose
```

### Export and Integration

```bash
# Export to JSON
opencog/bin/opencog-neurosymbolic --all --export spectrum-analysis.json

# Generate integrated hypergraph with spectrum
opencog/bin/opencog-hypergraph --all --export-graph --export-matrix
```

## Implementation Details

### Analyzer Module

Located at `opencog/lib/neurosymbolic_spectrum.py`:

- **SpectrumPosition**: Enum defining 7 positions
- **SpectrumFeatures**: 6-feature scoring system
- **LanguageSpectrumProfile**: Complete profile with rationale
- **NeuroSymbolicAnalyzer**: Main analysis engine

### Predefined Profiles

The analyzer includes carefully calibrated profiles for key languages:

- **Functional**: Lisp, Scheme, Racket, Haskell, ML
- **Logic**: Prolog
- **Array**: APL, J, Julia
- **Imperative**: C, Rust, Java, Python
- **Assembly**: Assembly language

Other languages are estimated based on paradigm classification.

### Extensibility

Add new language profiles:

```python
from opencog.lib.neurosymbolic_spectrum import NeuroSymbolicAnalyzer

analyzer = NeuroSymbolicAnalyzer('.')

# Profiles are cached, can be extended via:
analyzer.LANGUAGE_PROFILES['NewLang'] = SpectrumFeatures(
    structure_preservation=0.8,
    meaning_distribution=0.7,
    # ... etc
)
```

## Research Insights

### Finding 1: Parentheses Preserve Potential

Languages with parenthesized syntax (Lisp family) consistently score highest on neural features. Parentheses are not syntax - they're **topological markers** that preserve the possibility space.

### Finding 2: Paradigm Clustering

Paradigms form distinct clusters on the spectrum:
- Functional/Logic: Neural half
- Imperative/System: Symbolic half
- Multi-paradigm: Center (by design)

### Finding 3: Task-Spectrum Matching

AI reasoning tasks (symbolic reasoning, constraint satisfaction) correlate with balanced-neural languages. This suggests **optimal grip** on problem space requires preserving possibilities during search.

### Finding 4: Evolution Toward Balance

Modern languages trend toward center:
- Python (0.40) more balanced than C (0.15)
- Rust (0.23) more balanced than pure imperative
- Multi-paradigm languages increasingly common

## Future Directions

### 1. Automated Feature Extraction

Move from manual profiles to automated analysis of:
- AST structure complexity
- Evaluation model from language specs
- Control flow patterns from implementations

### 2. Dynamic Spectrum Analysis

Analyze how languages move on spectrum during execution:
- Compilation collapses possibilities
- Runtime reflection reopens possibilities
- JIT optimization shifts position

### 3. Spectrum-Aware Tooling

Build tools that leverage spectrum knowledge:
- **IDEs**: Suggest refactorings based on spectrum position
- **Compilers**: Optimize based on spectrum characteristics
- **Debuggers**: Present state differently for neural vs symbolic

### 4. Cross-Spectrum Translation

Develop semantic-preserving translation patterns:
- Neural → Symbolic: Resolve indeterminacy
- Symbolic → Neural: Introduce flexibility
- Preserve meaning across spectrum shifts

## References

- **Theoretical Foundation**: `neuro-symbo.md` - Complete spectrum analysis
- **Implementation**: `opencog/lib/neurosymbolic_spectrum.py`
- **Integration**: `opencog/lib/opencog_analyzer.py`, `opencog/lib/hypergraph_analyzer.py`
- **CLI Tool**: `opencog/bin/opencog-neurosymbolic`
- **OpenCog Framework**: `opencog/README.md`

## Key Takeaway

> **Programming languages differ mainly in how long they preserve possibility before collapsing it into commitment — and parentheses are the visible trace of that choice.**

This framework enables:
1. Evidence-based language selection
2. Understanding paradigm trade-offs
3. Designing polyglot architectures
4. Matching problems to computational styles
5. Revealing deep patterns in language design

The neuro-symbolic spectrum is not just classification - it's a lens for understanding the fundamental nature of computation itself.
