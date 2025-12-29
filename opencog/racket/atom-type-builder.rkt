#lang racket/base

;;; OpenCog Atom Type Builder
;;;
;;; Populates the atom type system from RosettaCog data.
;;; Creates concrete instances of cognitive domain and language paradigm atoms.
;;;
;;; Racket port of opencog/lib/atom_type_builder.py

(require racket/list
         racket/set
         racket/string
         racket/hash
         racket/path
         racket/file
         racket/format
         json
         "atom-types.rkt"
         "opencog-analyzer.rkt")

(provide atom-type-builder%
         make-atom-type-builder
         build-cognitive-domain-atoms
         build-language-paradigm-atoms
         build-atom-system)

;;; Atom Type Builder Class

(define atom-type-builder%
  (class object%
    (init-field root-dir)

    (super-new)

    (define analyzer (make-analyzer root-dir))
    (define atom-system (make-atom-type-system))

    ;; Load categories
    (define categories-data (send analyzer get-categories))

    ;; Build all 10 cognitive domain atoms
    (define/public (build-cognitive-domain-atoms)
      (define categories (hash-ref categories-data 'categories (hash)))

      (for ([(domain-name domain-data) (in-hash categories)])
        (define description (hash-ref domain-data 'description ""))
        (define tasks (list->set (hash-ref domain-data 'tasks '())))
        (define subcategories (hash-ref domain-data 'subcategories (hash)))

        ;; Get cognitive processes for this domain
        (define cognitive-processes (get-cognitive-processes domain-name))

        ;; Compute metrics
        (define metrics (compute-domain-metrics domain-name tasks))

        ;; Get relationships
        (define relationships (get-domain-relationships domain-name))

        ;; Create and register atom
        (define atom
          (make-cognitive-domain-atom
           domain-name
           description
           #:task-universe tasks
           #:subcategories subcategories
           #:cognitive-processes cognitive-processes
           #:metrics metrics
           #:relationships relationships))

        (register-cognitive-domain atom-system atom)))

    ;; Build all 9 language paradigm atoms
    (define/public (build-language-paradigm-atoms)
      (define paradigms (hash-ref categories-data 'paradigms (hash)))

      (for ([(paradigm-name paradigm-data) (in-hash paradigms)])
        (define description (hash-ref paradigm-data 'description ""))
        (define languages (list->set (hash-ref paradigm-data 'languages '())))

        ;; Get features for this paradigm
        (define features (get-paradigm-features paradigm-name))

        ;; Get computational model
        (define computational-model (get-computational-model paradigm-name))

        ;; Compute domain applicability
        (define domain-applicability
          (compute-domain-applicability paradigm-name languages))

        ;; Get relationships
        (define relationships (get-paradigm-relationships paradigm-name))

        ;; Create and register atom
        (define atom
          (make-language-paradigm-atom
           paradigm-name
           description
           #:language-set languages
           #:features features
           #:computational-model computational-model
           #:domain-applicability domain-applicability
           #:relationships relationships))

        (register-language-paradigm atom-system atom)))

    ;; Build complete system
    (define/public (build)
      (build-cognitive-domain-atoms)
      (build-language-paradigm-atoms)
      atom-system)

    ;; Get the atom system
    (define/public (get-atom-system) atom-system)))

;;; Helper Functions for Cognitive Domain Atoms

(define (get-cognitive-processes domain)
  "Get cognitive processes for each domain"
  (define processes
    (hasheq
     'symbolic_reasoning
     '("deduction" "inference" "constraint_propagation"
       "logical_evaluation" "proof_construction")
     'pattern_recognition
     '("matching" "classification" "similarity_computation"
       "search" "feature_extraction")
     'knowledge_representation
     '("encoding" "retrieval" "traversal"
       "association" "serialization")
     'machine_learning
     '("optimization" "regression" "classification"
       "clustering" "gradient_descent")
     'natural_language
     '("tokenization" "parsing" "generation"
       "analysis" "transformation")
     'planning_problem_solving
     '("search" "heuristic_evaluation" "goal_decomposition"
       "path_finding" "strategy_selection")
     'uncertainty_reasoning
     '("sampling" "estimation" "inference"
       "simulation" "distribution_fitting")
     'cognitive_architecture
     '("synchronization" "communication" "scheduling"
       "resource_allocation" "coordination")
     'perception_motor
     '("transformation" "filtering" "recognition"
       "rendering" "temporal_processing")
     'meta_learning
     '("reflection" "code_generation" "evaluation"
       "introspection" "self_modification")))
  (hash-ref processes (string->symbol domain) '()))

(define (compute-domain-metrics domain tasks)
  "Compute performance metrics for a domain"
  (hasheq 'task_count (exact->inexact (set-count tasks))
          'complexity (* (set-count tasks) 1.5)
          'coverage 1.0))

(define (get-domain-relationships domain)
  "Get relationships between cognitive domains"
  (define relationships
    (hasheq
     'symbolic_reasoning
     (hasheq 'complements '("knowledge_representation" "planning_problem_solving")
             'requires '("pattern_recognition")
             'enables '("uncertainty_reasoning"))
     'pattern_recognition
     (hasheq 'complements '("machine_learning" "perception_motor")
             'requires '()
             'enables '("symbolic_reasoning" "natural_language"))
     'knowledge_representation
     (hasheq 'complements '("symbolic_reasoning" "natural_language")
             'requires '()
             'enables '("planning_problem_solving"))
     'machine_learning
     (hasheq 'complements '("uncertainty_reasoning" "pattern_recognition")
             'requires '("knowledge_representation")
             'enables '("perception_motor"))
     'natural_language
     (hasheq 'complements '("knowledge_representation" "symbolic_reasoning")
             'requires '("pattern_recognition")
             'enables '("meta_learning"))
     'planning_problem_solving
     (hasheq 'complements '("symbolic_reasoning" "uncertainty_reasoning")
             'requires '("knowledge_representation")
             'enables '("cognitive_architecture"))
     'uncertainty_reasoning
     (hasheq 'complements '("machine_learning" "planning_problem_solving")
             'requires '("symbolic_reasoning")
             'enables '())
     'cognitive_architecture
     (hasheq 'complements '("planning_problem_solving")
             'requires '("knowledge_representation")
             'enables '("meta_learning"))
     'perception_motor
     (hasheq 'complements '("machine_learning" "pattern_recognition")
             'requires '()
             'enables '())
     'meta_learning
     (hasheq 'complements '("natural_language" "symbolic_reasoning")
             'requires '("cognitive_architecture")
             'enables '())))
  (hash-ref relationships (string->symbol domain)
            (hasheq 'complements '() 'requires '() 'enables '())))

;;; Helper Functions for Language Paradigm Atoms

(define (get-paradigm-features paradigm)
  "Get defining features for each paradigm"
  (define features
    (hasheq
     'imperative
     '("explicit_state" "sequential_execution" "mutation"
       "control_flow" "procedural_abstraction")
     'object_oriented
     '("encapsulation" "inheritance" "polymorphism"
       "message_passing" "dynamic_dispatch")
     'functional
     '("immutability" "higher_order_functions" "recursion"
       "lazy_evaluation" "referential_transparency")
     'logic
     '("declarative_rules" "unification" "backtracking"
       "pattern_matching" "non_determinism")
     'concurrent
     '("parallelism" "message_passing" "actor_model"
       "lightweight_processes" "fault_tolerance")
     'scripting
     '("dynamic_typing" "rapid_prototyping" "interpretive_execution"
       "flexible_syntax" "runtime_flexibility")
     'system
     '("low_level_control" "manual_memory_management" "performance"
       "hardware_access" "minimal_runtime")
     'scientific
     '("numerical_optimization" "array_operations" "vectorization"
       "mathematical_notation" "domain_libraries")
     'multi_paradigm
     '("paradigm_flexibility" "multiple_models" "expressive_power"
       "gradual_typing" "metaprogramming")))
  (hash-ref features (string->symbol paradigm) '()))

(define (get-computational-model paradigm)
  "Get computational model for each paradigm"
  (define models
    (hasheq
     'imperative "Von Neumann - sequential state transformation"
     'object_oriented "Message passing between objects with encapsulated state"
     'functional "Lambda calculus - function composition and evaluation"
     'logic "Resolution and unification in Horn clause logic"
     'concurrent "Actor model - asynchronous message passing"
     'scripting "Interpreted execution with dynamic evaluation"
     'system "Direct hardware manipulation with minimal abstraction"
     'scientific "Array-oriented computation with vectorized operations"
     'multi_paradigm "Hybrid model supporting multiple computational approaches"))
  (hash-ref models (string->symbol paradigm) "Unspecified"))

(define (compute-domain-applicability paradigm languages)
  "Compute how well a paradigm applies to each cognitive domain"
  (define affinities
    (hasheq
     'imperative
     (hasheq 'symbolic_reasoning 0.6 'pattern_recognition 0.7
             'knowledge_representation 0.7 'machine_learning 0.5
             'natural_language 0.6 'planning_problem_solving 0.7
             'uncertainty_reasoning 0.6 'cognitive_architecture 0.7
             'perception_motor 0.8 'meta_learning 0.5)
     'object_oriented
     (hasheq 'symbolic_reasoning 0.7 'pattern_recognition 0.8
             'knowledge_representation 0.9 'machine_learning 0.7
             'natural_language 0.8 'planning_problem_solving 0.8
             'uncertainty_reasoning 0.7 'cognitive_architecture 0.8
             'perception_motor 0.7 'meta_learning 0.6)
     'functional
     (hasheq 'symbolic_reasoning 0.9 'pattern_recognition 0.8
             'knowledge_representation 0.7 'machine_learning 0.8
             'natural_language 0.9 'planning_problem_solving 0.7
             'uncertainty_reasoning 0.8 'cognitive_architecture 0.6
             'perception_motor 0.6 'meta_learning 0.9)
     'logic
     (hasheq 'symbolic_reasoning 1.0 'pattern_recognition 0.7
             'knowledge_representation 0.8 'machine_learning 0.5
             'natural_language 0.7 'planning_problem_solving 0.8
             'uncertainty_reasoning 0.6 'cognitive_architecture 0.5
             'perception_motor 0.4 'meta_learning 0.7)
     'concurrent
     (hasheq 'symbolic_reasoning 0.5 'pattern_recognition 0.6
             'knowledge_representation 0.6 'machine_learning 0.7
             'natural_language 0.6 'planning_problem_solving 0.7
             'uncertainty_reasoning 0.8 'cognitive_architecture 1.0
             'perception_motor 0.7 'meta_learning 0.6)
     'scripting
     (hasheq 'symbolic_reasoning 0.6 'pattern_recognition 0.8
             'knowledge_representation 0.7 'machine_learning 0.7
             'natural_language 0.9 'planning_problem_solving 0.7
             'uncertainty_reasoning 0.7 'cognitive_architecture 0.6
             'perception_motor 0.7 'meta_learning 0.9)
     'system
     (hasheq 'symbolic_reasoning 0.6 'pattern_recognition 0.7
             'knowledge_representation 0.7 'machine_learning 0.8
             'natural_language 0.6 'planning_problem_solving 0.8
             'uncertainty_reasoning 0.8 'cognitive_architecture 0.9
             'perception_motor 0.9 'meta_learning 0.6)
     'scientific
     (hasheq 'symbolic_reasoning 0.7 'pattern_recognition 0.7
             'knowledge_representation 0.6 'machine_learning 0.9
             'natural_language 0.5 'planning_problem_solving 0.7
             'uncertainty_reasoning 0.9 'cognitive_architecture 0.6
             'perception_motor 0.8 'meta_learning 0.6)
     'multi_paradigm
     (hasheq 'symbolic_reasoning 0.8 'pattern_recognition 0.8
             'knowledge_representation 0.8 'machine_learning 0.8
             'natural_language 0.8 'planning_problem_solving 0.8
             'uncertainty_reasoning 0.8 'cognitive_architecture 0.8
             'perception_motor 0.8 'meta_learning 0.8)))
  (hash-ref affinities (string->symbol paradigm) (hasheq)))

(define (get-paradigm-relationships paradigm)
  "Get relationships between language paradigms"
  (define relationships
    (hasheq
     'imperative
     (hasheq 'contrasts '("functional" "logic")
             'extends_to '("object_oriented" "scripting")
             'composes_with '("concurrent"))
     'object_oriented
     (hasheq 'contrasts '("functional")
             'extends_to '("multi_paradigm")
             'composes_with '("imperative" "scripting"))
     'functional
     (hasheq 'contrasts '("imperative" "object_oriented")
             'extends_to '("multi_paradigm")
             'composes_with '("logic" "concurrent"))
     'logic
     (hasheq 'contrasts '("imperative")
             'extends_to '("multi_paradigm")
             'composes_with '("functional"))
     'concurrent
     (hasheq 'contrasts '()
             'extends_to '("multi_paradigm")
             'composes_with '("functional" "imperative"))
     'scripting
     (hasheq 'contrasts '("system")
             'extends_to '("multi_paradigm")
             'composes_with '("object_oriented" "functional"))
     'system
     (hasheq 'contrasts '("scripting")
             'extends_to '()
             'composes_with '("imperative"))
     'scientific
     (hasheq 'contrasts '()
             'extends_to '("multi_paradigm")
             'composes_with '("functional" "imperative"))
     'multi_paradigm
     (hasheq 'contrasts '()
             'extends_to '()
             'composes_with '("imperative" "functional" "object_oriented"))))
  (hash-ref relationships (string->symbol paradigm)
            (hasheq 'contrasts '() 'extends_to '() 'composes_with '())))

;;; Convenience Functions

(define (make-atom-type-builder root-dir)
  "Create a new Atom Type Builder"
  (new atom-type-builder% [root-dir root-dir]))

(define (build-cognitive-domain-atoms builder)
  (send builder build-cognitive-domain-atoms))

(define (build-language-paradigm-atoms builder)
  (send builder build-language-paradigm-atoms))

(define (build-atom-system builder)
  (send builder build))

;;; Main Entry Point

(module+ main
  (require racket/cmdline)

  (define root-dir
    (command-line
     #:args ([dir "."])
     dir))

  (define abs-root (path->complete-path (string->path root-dir)))
  (printf "Building atom type system from: ~a~n~n" abs-root)

  (define builder (make-atom-type-builder (path->string abs-root)))
  (define atom-system (build-atom-system builder))

  ;; Print summary
  (print-summary atom-system)

  ;; Export to JSON
  (define output-dir (build-path abs-root "opencog" "output"))
  (make-directory* output-dir)

  (export-to-json atom-system
                 (path->string (build-path output-dir "atom-type-expressions-racket.json")))

  (printf "~nAtom type expressions exported to:~n")
  (printf "  ~a~n" (build-path output-dir "atom-type-expressions-racket.json")))
