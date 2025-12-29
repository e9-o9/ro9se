# OpenCog Racket Implementation

Racket ports of the core OpenCog analysis components. These implementations provide functional programming alternatives to the Python implementations while maintaining full compatibility with the RosettaCog data structures.

## Components

### 1. OpenCog Analyzer (`opencog-analyzer.rkt`)

Core language capability evaluator that analyzes all 970+ programming languages in RosettaCog to evaluate their AI/AGI capabilities across different cognitive domains.

**Features:**
- Load and parse AI task categories from YAML
- Get all languages and tasks in the repository
- Categorize tasks by AI domain (10 cognitive domains)
- Analyze language capabilities and generate profiles
- Generate FrankenCog integration manifests

**Usage:**
```bash
opencog/bin/opencog-analyze-racket
# Or directly:
racket opencog/racket/opencog-analyzer.rkt /path/to/rosettacog
```

### 2. Hypergraph Analyzer (`hypergraph-analyzer.rkt`)

Multi-dimensional relationship analyzer that extends the base analyzer with subcategory analysis and hypergraph generation.

**Features:**
- 45 subcategory analysis (4-6 per cognitive domain)
- Language-to-subcategory relationship mapping
- Language-to-paradigm edge generation
- Paradigm performance matrix computation
- Hypergraph export to JSON

**Usage:**
```bash
opencog/bin/opencog-hypergraph-racket
# Or directly:
racket opencog/racket/hypergraph-analyzer.rkt /path/to/rosettacog
```

### 3. Atom Type System (`atom-types.rkt` + `atom-type-builder.rkt`)

Formal mathematical expression system for cognitive domains and language paradigms.

**Formalized Expressions:**

**Cognitive Domain Atom:**
```
CD(d) = ⟨Ω_d, Σ_d, Ψ_d, Φ_d⟩

Where:
- Ω_d (Omega): Task universe for domain d
- Σ_d (Sigma): Subcategory partitioning
- Ψ_d (Psi): Cognitive processes/operations
- Φ_d (Phi): Performance metrics
```

**Language Paradigm Atom:**
```
LP(p) = ⟨Λ_p, Π_p, Θ_p, Ξ_p⟩

Where:
- Λ_p (Lambda): Language set in paradigm p
- Π_p (Pi): Paradigmatic features
- Θ_p (Theta): Computational model
- Ξ_p (Xi): Cross-domain applicability
```

**Usage:**
```bash
opencog/bin/opencog-atom-types-racket
# Or directly:
racket opencog/racket/atom-type-builder.rkt /path/to/rosettacog
```

## Architecture

The Racket implementation follows functional programming principles:

1. **Immutable Data Structures**: Uses Racket's immutable hash tables and sets
2. **Pure Functions**: Analysis functions are side-effect free where possible
3. **Class-based API**: Uses Racket's class system for OOP compatibility
4. **Module System**: Clean separation of concerns with `provide`/`require`

## Dependencies

- **Racket 8.0+**: Core Racket with `racket/base`, `racket/class`, `json`
- No external packages required (uses stdlib only)

## Output Files

All components generate JSON output files in `opencog/output/`:

- `language-analysis-racket.json` - Full language capability analysis
- `frankencog-manifest-racket.json` - Optimal language per AI function
- `hypergraph-racket.json` - Hypergraph structure with nodes and edges
- `paradigm-matrix-racket.json` - Paradigm performance by subcategory
- `atom-type-expressions-racket.json` - Formal atom type expressions

## Comparison with Python Implementation

| Feature | Python | Racket |
|---------|--------|--------|
| YAML Parsing | PyYAML | Built-in parser |
| Data Structures | dict, set | hash, set |
| OOP Style | Classes | Classes (racket/class) |
| Functional Support | Limited | First-class |
| Type System | Dynamic | Dynamic + contracts |

## Why Racket?

Racket's strengths for this project:

1. **Lisp Heritage**: Natural fit for symbolic reasoning and meta-programming
2. **Pattern Matching**: Excellent for task categorization and analysis
3. **Contracts**: Optional static verification for formal systems
4. **Macros**: Potential for domain-specific language extensions
5. **Functional Paradigm**: Aligns with the mathematical formalism of atom types

## Future Enhancements

- Add Typed Racket annotations for compile-time type checking
- Implement graph visualization using `plot` library
- Create interactive REPL for exploratory analysis
- Add parallel processing for large-scale analysis
