#lang racket/base

;;; OpenCog Atom Type System
;;;
;;; Defines formalized atom type expressions for cognitive-domain and language-paradigm atoms.
;;; Provides a type hierarchy and algebraic expressions for reasoning about language capabilities.
;;;
;;; Inspired by OpenCog's Atomese but tailored for RosettaCog's language evaluation framework.
;;;
;;; Racket port of opencog/lib/atom_types.py

(require racket/list
         racket/set
         racket/string
         racket/hash
         racket/format
         racket/match
         json)

(provide atom-type-category
         atom-type
         cognitive-domain-atom
         language-paradigm-atom
         atom-type-system%
         make-atom-type-system
         ;; Constructors
         make-cognitive-domain-atom
         make-language-paradigm-atom
         ;; Atom type methods
         atom-type-name
         atom-type-description
         atom-type-category-of
         atom-type-properties
         atom-type-relationships
         get-expression
         get-detailed-expression
         atom-type->hash
         ;; Cognitive domain specific
         cognitive-domain-task-universe
         cognitive-domain-subcategories
         cognitive-domain-processes
         cognitive-domain-metrics
         compute-complexity
         ;; Language paradigm specific
         language-paradigm-languages
         language-paradigm-features
         language-paradigm-computational-model
         language-paradigm-domain-applicability
         compute-versatility
         ;; System methods
         register-cognitive-domain
         register-language-paradigm
         get-generalized-expressions
         get-all-expressions
         compute-paradigm-domain-affinity
         generate-comparison-insights
         export-to-json
         print-summary)

;;; Atom Type Categories

(define atom-type-category
  (make-parameter 'unspecified))

(define (category-cognitive-domain) 'cognitive_domain)
(define (category-language-paradigm) 'language_paradigm)

;;; Base Atom Type Structure

(struct atom-type
  (name                ; string - atom name
   description         ; string - description
   category            ; symbol - category type
   properties          ; hash - additional properties
   relationships)      ; hash - relationships to other atoms
  #:transparent)

;;; Cognitive Domain Atom
;;; Formalized Expression: CD(d) = ⟨Ω_d, Σ_d, Ψ_d, Φ_d⟩
;;;
;;; Where:
;;; - Ω_d (Omega): Task universe for domain d
;;; - Σ_d (Sigma): Subcategory partitioning of Ω_d
;;; - Ψ_d (Psi): Cognitive processes/operations
;;; - Φ_d (Phi): Performance metrics and evaluation functions

(struct cognitive-domain-atom atom-type
  (task-universe       ; set - Ω_d: all tasks in domain
   subcategories       ; hash - Σ_d: subcategory partitioning
   cognitive-processes ; list - Ψ_d: cognitive operations
   metrics)            ; hash - Φ_d: performance metrics
  #:transparent)

(define (make-cognitive-domain-atom name description
                                    #:task-universe [task-universe (set)]
                                    #:subcategories [subcategories (hash)]
                                    #:cognitive-processes [cognitive-processes '()]
                                    #:metrics [metrics (hash)]
                                    #:properties [properties (hash)]
                                    #:relationships [relationships (hash)])
  (cognitive-domain-atom name description
                        (category-cognitive-domain)
                        properties relationships
                        task-universe subcategories
                        cognitive-processes metrics))

;;; Language Paradigm Atom
;;; Formalized Expression: LP(p) = ⟨Λ_p, Π_p, Θ_p, Ξ_p⟩
;;;
;;; Where:
;;; - Λ_p (Lambda): Language set belonging to paradigm p
;;; - Π_p (Pi): Paradigmatic features/characteristics
;;; - Θ_p (Theta): Computational model and execution semantics
;;; - Ξ_p (Xi): Cross-domain applicability matrix

(struct language-paradigm-atom atom-type
  (language-set             ; set - Λ_p: languages in paradigm
   features                 ; list - Π_p: defining features
   computational-model      ; string - Θ_p: execution semantics
   domain-applicability)    ; hash - Ξ_p: domain fitness scores
  #:transparent)

(define (make-language-paradigm-atom name description
                                     #:language-set [language-set (set)]
                                     #:features [features '()]
                                     #:computational-model [computational-model ""]
                                     #:domain-applicability [domain-applicability (hash)]
                                     #:properties [properties (hash)]
                                     #:relationships [relationships (hash)])
  (language-paradigm-atom name description
                         (category-language-paradigm)
                         properties relationships
                         language-set features
                         computational-model domain-applicability))

;;; Expression Generation

(define (get-expression atom)
  "Generate formal expression for this atom type"
  (match atom
    [(cognitive-domain-atom name _ _ _ _ _ _ _ _)
     (format "CD(~a) = ⟨Ω_~a, Σ_~a, Ψ_~a, Φ_~a⟩" name name name name name)]
    [(language-paradigm-atom name _ _ _ _ _ _ _ _)
     (format "LP(~a) = ⟨Λ_~a, Π_~a, Θ_~a, Ξ_~a⟩" name name name name name)]
    [_ "Unknown atom type"]))

(define (get-detailed-expression atom)
  "Get detailed breakdown of the expression"
  (match atom
    [(cognitive-domain-atom name _ _ _ _ task-universe subcategories processes metrics)
     (define task-list (set->list task-universe))
     (define task-sample (take task-list (min 5 (length task-list))))
     (define ellipsis (if (> (length task-list) 5) "..." ""))
     (hash 'canonical (get-expression atom)
           'omega (format "Ω_~a = { ~a~a } (n=~a)"
                         name (string-join (sort task-sample string<?) ", ") ellipsis
                         (set-count task-universe))
           'sigma (format "Σ_~a = { ~a } (k=~a)"
                         name (string-join (sort (hash-keys subcategories) string<?) ", ")
                         (hash-count subcategories))
           'psi (format "Ψ_~a = { ~a }" name (string-join processes ", "))
           'phi (format "Φ_~a = { ~a }"
                       name (string-join
                             (for/list ([(k v) (in-hash (take-hash metrics 3))])
                               (format "~a=~a" k (~r v #:precision 2)))
                             ", ")))]
    [(language-paradigm-atom name _ _ _ _ lang-set features comp-model domain-app)
     (define lang-list (set->list lang-set))
     (define lang-sample (take lang-list (min 5 (length lang-list))))
     (define ellipsis (if (> (length lang-list) 5) "..." ""))
     (hash 'canonical (get-expression atom)
           'lambda (format "Λ_~a = { ~a~a } (n=~a)"
                          name (string-join (sort lang-sample string<?) ", ") ellipsis
                          (set-count lang-set))
           'pi (format "Π_~a = { ~a }" name (string-join features ", "))
           'theta (format "Θ_~a = ~a" name comp-model)
           'xi (format "Ξ_~a = { ~a }"
                      name (string-join
                            (for/list ([(k v) (in-hash (take-hash domain-app 3))])
                              (format "~a→~a" k (~r v #:precision 2)))
                            ", ")))]
    [_ (hash 'canonical "Unknown" 'details "")]))

(define (take-hash h n)
  "Take first n entries from a hash"
  (for/hash ([(k v) (in-hash h)]
             [i (in-range n)])
    (values k v)))

;;; Complexity and Versatility Measures

(define (compute-complexity atom)
  "Compute domain complexity: C(d) = |Ω_d| × |Σ_d| × log(|Ψ_d|)"
  (match atom
    [(cognitive-domain-atom _ _ _ _ _ task-universe subcategories processes _)
     (define omega-size (set-count task-universe))
     (define sigma-size (hash-count subcategories))
     (define psi-size (max 1 (length processes)))
     (* omega-size sigma-size (log (+ psi-size 1)))]
    [_ 0.0]))

(define (compute-versatility atom)
  "Compute paradigm versatility: V(p) = |Λ_p| × Σ(Ξ_p) / |Features|"
  (match atom
    [(language-paradigm-atom _ _ _ _ _ lang-set features _ domain-app)
     (define lambda-size (set-count lang-set))
     (define xi-sum (for/sum ([v (in-hash-values domain-app)]) v))
     (define feature-count (max 1 (length features)))
     (/ (* lambda-size xi-sum) feature-count)]
    [_ 0.0]))

;;; Convert to hash for JSON export

(define (atom-type->hash atom)
  "Convert atom to hash representation"
  (match atom
    [(cognitive-domain-atom name desc cat props rels task-universe subcats procs metrics)
     (hasheq 'name name
             'category (symbol->string cat)
             'description desc
             'properties props
             'relationships rels
             'expression (get-expression atom)
             'task_universe (set->list task-universe)
             'subcategories subcats
             'cognitive_processes procs
             'metrics metrics)]
    [(language-paradigm-atom name desc cat props rels lang-set feats comp-model domain-app)
     (hasheq 'name name
             'category (symbol->string cat)
             'description desc
             'properties props
             'relationships rels
             'expression (get-expression atom)
             'language_set (set->list lang-set)
             'features feats
             'computational_model comp-model
             'domain_applicability domain-app)]
    [_ (hasheq 'error "Unknown atom type")]))

;;; Atom Type System

(define atom-type-system%
  (class object%
    (super-new)

    (define cognitive-domains (make-hash))
    (define language-paradigms (make-hash))

    ;; Register a cognitive domain atom
    (define/public (register-cognitive-domain atom)
      (hash-set! cognitive-domains (atom-type-name atom) atom))

    ;; Register a language paradigm atom
    (define/public (register-language-paradigm atom)
      (hash-set! language-paradigms (atom-type-name atom) atom))

    ;; Get all cognitive domains
    (define/public (get-cognitive-domains) cognitive-domains)

    ;; Get all language paradigms
    (define/public (get-language-paradigms) language-paradigms)

    ;; Get generalized expressions
    (define/public (get-generalized-expressions)
      (hasheq
       'cognitive_domain
       (hasheq 'name "Cognitive Domain Atom (CD)"
               'expression "CD(d) = ⟨Ω_d, Σ_d, Ψ_d, Φ_d⟩"
               'components (hasheq
                           'Ω_d "Task universe for domain d - set of all tasks in the domain"
                           'Σ_d "Subcategory partitioning - refined specialization of Ω_d"
                           'Ψ_d "Cognitive processes - mental operations required"
                           'Φ_d "Performance metrics - evaluation functions")
               'semantics "A cognitive domain atom represents a fundamental area of cognitive capability, characterized by its task universe, internal structure, required processes, and performance measures.")
       'language_paradigm
       (hasheq 'name "Language Paradigm Atom (LP)"
               'expression "LP(p) = ⟨Λ_p, Π_p, Θ_p, Ξ_p⟩"
               'components (hasheq
                           'Λ_p "Language set - programming languages in paradigm p"
                           'Π_p "Paradigmatic features - defining characteristics"
                           'Θ_p "Computational model - execution semantics"
                           'Ξ_p "Domain applicability - fitness for cognitive domains")
               'semantics "A language paradigm atom represents a fundamental approach to programming, characterized by its language membership, defining features, computational model, and effectiveness across cognitive domains.")))

    ;; Get all expressions
    (define/public (get-all-expressions)
      (hasheq 'generalized (get-generalized-expressions)
              'cognitive_domains (for/hasheq ([(name atom) (in-hash cognitive-domains)])
                                  (values name (atom-type->hash atom)))
              'language_paradigms (for/hasheq ([(name atom) (in-hash language-paradigms)])
                                   (values name (atom-type->hash atom)))))

    ;; Compute affinity between paradigm and domain
    (define/public (compute-paradigm-domain-affinity paradigm domain)
      (cond
        [(not (hash-has-key? language-paradigms paradigm)) 0.0]
        [(not (hash-has-key? cognitive-domains domain)) 0.0]
        [else
         (define paradigm-atom (hash-ref language-paradigms paradigm))
         (hash-ref (language-paradigm-atom-domain-applicability paradigm-atom)
                  domain 0.0)]))

    ;; Generate comparison insights
    (define/public (generate-comparison-insights)
      (hasheq 'structural_comparison (compare-structures)
              'complexity_analysis (analyze-complexity)
              'coverage_analysis (analyze-coverage)
              'affinity_patterns (analyze-affinity-patterns)))

    (define/private (compare-structures)
      (define cd-count (hash-count cognitive-domains))
      (define lp-count (hash-count language-paradigms))
      (define avg-subcats
        (if (> cd-count 0)
            (/ (for/sum ([atom (in-hash-values cognitive-domains)])
                 (hash-count (cognitive-domain-atom-subcategories atom)))
               cd-count)
            0))
      (define avg-langs
        (if (> lp-count 0)
            (/ (for/sum ([atom (in-hash-values language-paradigms)])
                 (set-count (language-paradigm-atom-language-set atom)))
               lp-count)
            0))
      (hasheq 'cognitive_domain_count cd-count
              'language_paradigm_count lp-count
              'ratio (if (> lp-count 0) (/ cd-count lp-count) 0)
              'avg_subcategories_per_domain avg-subcats
              'avg_languages_per_paradigm avg-langs
              'structural_similarity "Both use 4-tuple representation but with different semantics"))

    (define/private (analyze-complexity)
      (define domain-complexities
        (for/hasheq ([(name atom) (in-hash cognitive-domains)])
          (values name (compute-complexity atom))))
      (define paradigm-versatilities
        (for/hasheq ([(name atom) (in-hash language-paradigms)])
          (values name (compute-versatility atom))))
      (hasheq 'domain_complexities domain-complexities
              'paradigm_versatilities paradigm-versatilities
              'most_complex_domain
              (if (> (hash-count domain-complexities) 0)
                  (car (argmax cdr (hash->list domain-complexities)))
                  #f)
              'most_versatile_paradigm
              (if (> (hash-count paradigm-versatilities) 0)
                  (car (argmax cdr (hash->list paradigm-versatilities)))
                  #f)))

    (define/private (analyze-coverage)
      (define total-tasks
        (for/sum ([atom (in-hash-values cognitive-domains)])
          (set-count (cognitive-domain-atom-task-universe atom))))
      (define all-langs
        (for/fold ([acc (set)])
                  ([atom (in-hash-values language-paradigms)])
          (set-union acc (language-paradigm-atom-language-set atom))))
      (hasheq 'total_cognitive_tasks total-tasks
              'total_paradigm_languages (set-count all-langs)
              'tasks_per_domain
              (for/hasheq ([(name atom) (in-hash cognitive-domains)])
                (values name (set-count (cognitive-domain-atom-task-universe atom))))
              'languages_per_paradigm
              (for/hasheq ([(name atom) (in-hash language-paradigms)])
                (values name (set-count (language-paradigm-atom-language-set atom))))))

    (define/private (analyze-affinity-patterns)
      (for/hasheq ([(name atom) (in-hash language-paradigms)])
        (define domain-app (language-paradigm-atom-domain-applicability atom))
        (define sorted-domains (sort (hash->list domain-app) > #:key cdr))
        (values name
                (hasheq 'strongest_domains (take sorted-domains (min 3 (length sorted-domains)))
                        'weakest_domains (take (reverse sorted-domains)
                                              (min 3 (length sorted-domains)))))))

    ;; Export to JSON
    (define/public (export-to-json filepath)
      (call-with-output-file filepath
        #:exists 'replace
        (λ (out) (write-json (get-all-expressions) out))))

    ;; Print summary
    (define/public (print-summary)
      (printf "~a~n" (make-string 80 #\=))
      (printf "ROSETTACOG ATOM TYPE SYSTEM (Racket)~n")
      (printf "~a~n~n" (make-string 80 #\=))

      ;; Generalized expressions
      (define gen-expr (get-generalized-expressions))

      (printf "GENERALIZED EXPRESSIONS~n")
      (printf "~a~n~n" (make-string 80 #\-))

      (printf "1. COGNITIVE DOMAIN ATOMS~n")
      (define cd-expr (hash-ref gen-expr 'cognitive_domain))
      (printf "   Expression: ~a~n" (hash-ref cd-expr 'expression))
      (printf "   Semantics: ~a~n~n" (hash-ref cd-expr 'semantics))

      (printf "2. LANGUAGE PARADIGM ATOMS~n")
      (define lp-expr (hash-ref gen-expr 'language_paradigm))
      (printf "   Expression: ~a~n" (hash-ref lp-expr 'expression))
      (printf "   Semantics: ~a~n~n" (hash-ref lp-expr 'semantics))

      ;; Specific expressions
      (printf "~a~n" (make-string 80 #\=))
      (printf "COGNITIVE DOMAIN ATOM INSTANCES~n")
      (printf "~a~n" (make-string 80 #\=))

      (for ([(name atom) (in-hash cognitive-domains)])
        (printf "~n~a:~n" name)
        (printf "  ~a~n" (get-expression atom))
        (define details (get-detailed-expression atom))
        (for ([(key val) (in-hash details)]
              #:unless (eq? key 'canonical))
          (printf "    ~a~n" val)))

      (printf "~n~a~n" (make-string 80 #\=))
      (printf "LANGUAGE PARADIGM ATOM INSTANCES~n")
      (printf "~a~n" (make-string 80 #\=))

      (for ([(name atom) (in-hash language-paradigms)])
        (printf "~n~a:~n" name)
        (printf "  ~a~n" (get-expression atom))
        (define details (get-detailed-expression atom))
        (for ([(key val) (in-hash details)]
              #:unless (eq? key 'canonical))
          (printf "    ~a~n" val)))

      (printf "~n~a~n" (make-string 80 #\=))
      (printf "COMPARISON INSIGHTS~n")
      (printf "~a~n" (make-string 80 #\=))

      (define insights (generate-comparison-insights))

      (printf "~nStructural Comparison:~n")
      (for ([(key val) (in-hash (hash-ref insights 'structural_comparison))])
        (printf "  ~a: ~a~n" key val))

      (printf "~nComplexity Analysis:~n")
      (define complexity (hash-ref insights 'complexity_analysis))
      (printf "  Most complex domain: ~a~n" (hash-ref complexity 'most_complex_domain))
      (printf "  Most versatile paradigm: ~a~n" (hash-ref complexity 'most_versatile_paradigm))

      (printf "~nCoverage Analysis:~n")
      (define coverage (hash-ref insights 'coverage_analysis))
      (printf "  Total cognitive tasks: ~a~n" (hash-ref coverage 'total_cognitive_tasks))
      (printf "  Total paradigm languages: ~a~n" (hash-ref coverage 'total_paradigm_languages))

      (printf "~n"))))

;;; Convenience functions

(define (make-atom-type-system)
  (new atom-type-system%))

(define (register-cognitive-domain system atom)
  (send system register-cognitive-domain atom))

(define (register-language-paradigm system atom)
  (send system register-language-paradigm atom))

(define (get-generalized-expressions system)
  (send system get-generalized-expressions))

(define (get-all-expressions system)
  (send system get-all-expressions))

(define (compute-paradigm-domain-affinity system paradigm domain)
  (send system compute-paradigm-domain-affinity paradigm domain))

(define (generate-comparison-insights system)
  (send system generate-comparison-insights))

(define (export-to-json system filepath)
  (send system export-to-json filepath))

(define (print-summary system)
  (send system print-summary))
