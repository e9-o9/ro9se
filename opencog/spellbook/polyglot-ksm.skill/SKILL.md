---
name: polyglot-ksm
version: 1.0.0
type: hyper-skill-spellbook
status: active
scope: ro9se-opencog-polyglot-core
components:
  - cogutil
  - atomspace
  - cogserver
  - rosettacog
  - franken-cog
---

# Polyglot KSM Hyper-Skill Spellbook

## Purpose

The **Polyglot KSM Hyper-Skill** is the operational spellbook for evolving RO9SE from a RosettaCode-scale language corpus into an OpenCog-oriented polyglot cognitive substrate. It treats every language implementation as a candidate cognitive organ, every task as an observed behavior, and every OpenCog core binding as a stabilizing interface that lets the repository converge toward a coherent AGI-OS integration surface.

> **Definition.** A KSM spell is a repeatable Knowledge Sharing Mechanism cycle that senses repository structure, compares language-component coverage, selects the next center to strengthen, applies a concrete transformation, validates the result, and records the strengthened pattern for future iterations.

## Activation Triggers

Use this skill when the work item involves **polyglot OpenCog integration**, language-binding parity, FrankenCog synthesis, RosettaCog language capability analysis, Inferno/Plan 9 cognitive kernel alignment, or repository evolution across many programming languages. The skill is especially appropriate when adding or repairing implementations for `cogutil`, `atomspace`, and `cogserver`, because those components form the minimum executable OpenCog spine.

| Trigger | Interpretation | Primary Spell |
|---|---|---|
| Missing OpenCog binding | A component lacks one of the repository's implemented languages | `coverage-parity` |
| New language added to one component | The language must be evaluated for all OpenCog core components | `language-union-propagation` |
| Cross-language architecture drift | Interfaces differ across implementations | `interface-crystallization` |
| AGI-OS kernel integration | The repository must map language organs into b9/p9/j9 topology | `kernel-fiber-weave` |
| FrankenCog synthesis | A task/domain should be assigned to optimal languages | `organ-selection` |
| Failing validation | The repository has an executable defect or broken invariant | `repair-and-seal` |

## Core Invariants

The spellbook preserves five invariants. First, **language parity is computed from the union of all OpenCog component language directories**, never from a single reference component. Second, **concrete implementation wins over placeholders**: a binding must expose real predicates, functions, commands, or types that demonstrate the component interface. Third, **Scheme remains the metamodel seed** for formal symbolic structures, while Prolog remains the first-class logic and inference organ. Fourth, **validation must be machine-readable**, so spell output should include JSON, YAML, or test assertions where possible. Fifth, every evolution step must preserve existing RosettaCode data and repository history.

## Spell Registry

| Spell | Objective | Inputs | Outputs | Validation |
|---|---|---|---|---|
| `coverage-parity` | Ensure each OpenCog component has every language found in any component | `Repo/opencog/{cogutil,atomspace,cogserver}` | Missing-language report and generated/implemented bindings | `opencog/bin/opencog-bindgen --coverage-json` has empty `missing` arrays |
| `language-union-propagation` | Propagate a newly discovered language across `cogutil`, `atomspace`, and `cogserver` | Component language directories | New language directories or explicit unsupported-language skips | Unit test covers union-based parity |
| `interface-crystallization` | Align per-language implementations to common component contracts | Integration design and existing bindings | Interface notes and idiomatic code for each language | Component-specific smoke tests or syntax checks |
| `kernel-fiber-weave` | Map OpenCog bindings into b9/p9/j9 AGI-OS topology | OpenCog dependency graph and kernel target | b-files, m-files, or dis-files design notes | Architecture document updated |
| `organ-selection` | Choose best language organs for a cognitive task | Hypergraph scores and task corpus | FrankenCog implementation plan | `opencog-manifest` or hypergraph report regenerated |
| `repair-and-seal` | Fix a failing validation path and prevent recurrence | Failure logs and tests | Code fix plus regression test | `make validate` and targeted tests pass |

## Execution Protocol

### 1. Sense the Current Center

Begin by listing current OpenCog component coverage. The executable sensor is the binding generator's coverage report:

```bash
opencog/bin/opencog-bindgen --coverage-json
```

The JSON result is the authoritative state vector for component-language parity. A complete center has an empty `missing` array for every component.

### 2. Select the Weakest Boundary

Rank weaknesses by whether they break executable validation, language parity, interface consistency, or future AGI-OS composition. A missing language in `cogutil` is foundational; a missing language in `atomspace` affects knowledge representation; a missing language in `cogserver` affects orchestration and REPL/network control.

### 3. Apply the Transformation

Implement the smallest real binding that satisfies the component contract in the language's own idiom. For example, a Prolog `cogutil` binding should expose logging, configuration, exception, and queue predicates. A Prolog `cogserver` binding should expose command registration, session tracking, command execution, and a reasoning-oriented demo rule.

### 4. Seal with Tests

Every transformation must add or update a validation surface. The preferred minimum is `make validate`, backed by a regression test that checks the specific invariant the spell strengthens. For coverage parity, the required invariant is that the coverage JSON uses the language union and all missing arrays are empty.

### 5. Record the New Pattern

Document the evolved pattern in this spellbook or a related design file. Pattern records should identify the center strengthened, the invariant protected, and the next probable extension point.

## Composition Algebra

Polyglot KSM uses a small algebra for composing transformations:

```text
Repository State R = Languages ⊕ Tasks ⊕ Components ⊕ Agents
Component Spine C = cogutil ⊗ atomspace ⊗ cogserver
Spell Cycle K = sense → select → transform → validate → record
Hyper-Skill H = K(C) ⊗ K(RosettaCog) ⊗ K(FrankenCog)
```

The additive operator `⊕` means independent centers can be inspected and improved separately. The multiplicative operator `⊗` means the elements interact: a cogserver without atomspace has little cognitive substance, and an atomspace without cogutil lacks stable utility primitives.

## OpenCog Core Binding Contract

| Component | Minimal Interface | Language-Specific Expression |
|---|---|---|
| `cogutil` | logging, config, exceptions, platform/concurrency utilities | Predicates, modules, structs, classes, macros, or command helpers |
| `atomspace` | node/link creation, truth values, queries, pattern matching | Hypergraph terms, algebraic data types, objects, records, or S-expressions |
| `cogserver` | command registry, sessions, request dispatch, status, REPL/network shell | Servers, actors, goroutines, predicates, REPL forms, or command maps |

A binding is acceptable only when it demonstrates behavior in the target language. Empty scaffolds, TODO-only files, or comment-only placeholders do not satisfy this contract.

## AGI-OS Mapping

The spellbook maps RO9SE into the user's b9/p9/j9 topology as follows:

| Layer | RO9SE Meaning | OpenCog Binding Role |
|---|---|---|
| `b9` | Binary/base implementation fibers connected to localhost | `cogutil` and low-level language organs |
| `p9` | Membrane namespace and execution scopes connected to globalhost | `atomspace` as a queryable cognitive filesystem |
| `j9` | Distributed gradient and surface dynamics connected to orgalhost | `cogserver` as orchestration, REPL, and agent dispatch |

## Invocation Examples

```bash
# Verify the parity invariant.
make validate

# Produce machine-readable coverage for downstream agents.
opencog/bin/opencog-bindgen --coverage-json

# List human-readable missing component-language cells.
opencog/bin/opencog-bindgen --list-missing

# Run the regression tests for the coverage spell.
python3.11 -m pytest -q tests/unit/test_opencog_bindgen.py
```

## Next Evolution Hooks

The next strong extension is a `spell-runner` command that can read `spellbook.yaml`, execute selected spells, and emit a signed evolution report. After that, the repository can grow a Scheme metamodel for spell composition and a Prolog proof layer for verifying that every claimed invariant follows from executable observations.
