#!/usr/bin/env racket
#lang racket
;; opencog-atomspace.rkt
;;
;; OpenCog AtomSpace - Hypergraph Knowledge Representation in Racket
;;
;; This single-file implementation demonstrates Racket's strengths for AI:
;; - Structs with contracts and transparent inspection
;; - Pattern matching for elegant queries
;; - Hash tables for efficient indexing
;; - Immutable updates with struct-copy
;; - Higher-order functions for graph operations

(require racket/contract
         racket/match
         racket/hash
         racket/set)

;; ===== Atom Types =====
;; Demonstrates: Symbols for type-safe enumerations

(define atom-types
  '(atom node link 
    concept-node predicate-node variable-node
    evaluation-link inheritance-link similarity-link
    list-link and-link or-link not-link))

(define/contract (valid-atom-type? type)
  (-> symbol? boolean?)
  (member type atom-types))

;; ===== Truth Value =====
;; Demonstrates: Structs with contracts, transparent inspection

(struct truth-value (strength confidence)
  #:transparent
  #:guard (struct-guard/c real? real?))

(define (make-tv [strength 1.0] [confidence 1.0])
  (truth-value strength confidence))

(define (tv->string tv)
  (format "tv=~a conf=~a" 
          (~r (truth-value-strength tv) #:precision 2)
          (~r (truth-value-confidence tv) #:precision 2)))

;; ===== Atom Base =====
;; Demonstrates: Generative constructors, immutability

(struct atom (type tv id)
  #:transparent
  #:guard (lambda (type tv id name)
            (unless (valid-atom-type? type)
              (error 'atom "Invalid atom type: ~a" type))
            (values type tv id)))

;; Atom ID counter
(define atom-id-counter (box 0))

(define (next-atom-id!)
  (begin0
    (unbox atom-id-counter)
    (set-box! atom-id-counter (add1 (unbox atom-id-counter)))))

;; ===== Node =====
;; Demonstrates: Struct inheritance, pattern matching

(struct node atom (name)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc nd port mode)
     (fprintf port "(~a \"~a\" ~a)" 
              (atom-type nd)
              (node-name nd)
              (tv->string (atom-tv nd))))])

(define/contract (make-node type name)
  (-> symbol? string? node?)
  (node type (make-tv) (next-atom-id!) name))

(define/contract (node-set-tv nd tv)
  (-> node? truth-value? node?)
  (struct-copy node nd [tv tv]))

;; ===== Link =====
;; Demonstrates: Recursive structures, list operations

(struct link atom (outgoing)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc lnk port mode)
     (fprintf port "(~a\n" (atom-type lnk))
     (for ([a (link-outgoing lnk)])
       (fprintf port "  ~a\n" a))
     (fprintf port ")"))])

(define/contract (make-link type outgoing)
  (-> symbol? (listof atom?) link?)
  (link type (make-tv) (next-atom-id!) outgoing))

(define/contract (link-set-tv lnk tv)
  (-> link? truth-value? link?)
  (struct-copy link lnk [tv tv]))

(define/contract (link-arity lnk)
  (-> link? exact-nonnegative-integer?)
  (length (link-outgoing lnk)))

;; ===== AtomSpace =====
;; Demonstrates: Multiple indexes, set operations, graph algorithms

(struct atomspace
  (atoms                ; List of all atoms
   node-index          ; Hash: (type . name) -> node
   type-index          ; Hash: type -> (setof atom)
   incoming-index)     ; Hash: atom-id -> (setof link)
  #:transparent
  #:mutable)

(define (make-atomspace)
  (atomspace '()
             (make-hash)
             (make-hash)
             (make-hash)))

;; Add node to atomspace
(define/contract (atomspace-add-node! as type name)
  (-> atomspace? symbol? string? node?)
  (define key (cons type name))
  (define existing (hash-ref (atomspace-node-index as) key #f))
  
  (if existing
      existing
      (let ([nd (make-node type name)])
        ;; Add to atoms list
        (set-atomspace-atoms! as (cons nd (atomspace-atoms as)))
        
        ;; Add to node index
        (hash-set! (atomspace-node-index as) key nd)
        
        ;; Add to type index
        (atomspace-add-to-type-index! as type nd)
        
        nd)))

;; Add link to atomspace
(define/contract (atomspace-add-link! as type outgoing)
  (-> atomspace? symbol? (listof atom?) link?)
  (define lnk (make-link type outgoing))
  
  ;; Add to atoms list
  (set-atomspace-atoms! as (cons lnk (atomspace-atoms as)))
  
  ;; Add to type index
  (atomspace-add-to-type-index! as type lnk)
  
  ;; Update incoming index for each outgoing atom
  (for ([a outgoing])
    (define aid (atom-id a))
    (define incoming-set (hash-ref (atomspace-incoming-index as) aid (set)))
    (hash-set! (atomspace-incoming-index as) aid (set-add incoming-set lnk)))
  
  lnk)

;; Helper: Add atom to type index
(define (atomspace-add-to-type-index! as type atm)
  (define type-set (hash-ref (atomspace-type-index as) type (set)))
  (hash-set! (atomspace-type-index as) type (set-add type-set atm)))

;; Get incoming links for an atom
(define/contract (atomspace-get-incoming as atm)
  (-> atomspace? atom? (setof link?))
  (hash-ref (atomspace-incoming-index as) (atom-id atm) (set)))

;; Get atoms by type
(define/contract (atomspace-get-by-type as type)
  (-> atomspace? symbol? (listof atom?))
  (set->list (hash-ref (atomspace-type-index as) type (set))))

;; Get specific node
(define/contract (atomspace-get-node as type name)
  (-> atomspace? symbol? string? (or/c node? #f))
  (hash-ref (atomspace-node-index as) (cons type name) #f))

;; Query operations
(define/contract (atomspace-size as)
  (-> atomspace? exact-nonnegative-integer?)
  (length (atomspace-atoms as)))

(define/contract (atomspace-contains? as atm)
  (-> atomspace? atom? boolean?)
  (member atm (atomspace-atoms as)))

(define/contract (atomspace-query as pattern)
  (-> atomspace? string? (listof atom?))
  (filter (lambda (a)
            (and (node? a)
                 (string-contains? (string-downcase (node-name a))
                                  (string-downcase pattern))))
          (atomspace-atoms as)))

;; Print all atoms
(define (atomspace-print-all as)
  (printf "AtomSpace contains ~a atoms:\n" (atomspace-size as))
  (for ([a (reverse (atomspace-atoms as))])
    (printf "  ~a\n" a)))

;; Statistics
(define (atomspace-stats as)
  (printf "AtomSpace Statistics:\n")
  (printf "  Total atoms: ~a\n" (atomspace-size as))
  (for ([type atom-types])
    (define count (length (atomspace-get-by-type as type)))
    (when (> count 0)
      (printf "  ~a: ~a\n" type count))))

;; ===== Pattern Matching Queries =====
;; Demonstrates: Racket's powerful pattern matching

(define/contract (find-inheritance-chains as concept)
  (-> atomspace? string? (listof (listof string?)))
  (define start-node (atomspace-get-node as 'concept-node concept))
  
  (if (not start-node)
      '()
      (let loop ([current start-node]
                 [chain (list concept)]
                 [visited (set)])
        (if (set-member? visited (atom-id current))
            (list chain)
            (let* ([new-visited (set-add visited (atom-id current))]
                   [incoming (atomspace-get-incoming as current)]
                   [inheritance-links 
                    (filter (lambda (lnk) 
                              (eq? (atom-type lnk) 'inheritance-link))
                            (set->list incoming))])
              (if (empty? inheritance-links)
                  (list chain)
                  (append-map
                   (lambda (lnk)
                     (define outgoing (link-outgoing lnk))
                     (if (and (= (length outgoing) 2)
                             (equal? (atom-id (first outgoing)) 
                                    (atom-id current)))
                         (let ([parent (second outgoing)])
                           (if (node? parent)
                               (loop parent 
                                    (append chain (list (node-name parent)))
                                    new-visited)
                               (list chain)))
                         (list chain)))
                   inheritance-links)))))))

;; ===== Demonstration =====

(define (demonstrate-atomspace)
  (displayln (make-string 70 #\=))
  (displayln "OpenCog AtomSpace - Hypergraph Knowledge Representation in Racket")
  (displayln "Showcasing: Structs, contracts, pattern matching, graph algorithms")
  (displayln (make-string 70 #\=))
  (newline)
  
  ;; Create AtomSpace
  (define as (make-atomspace))
  
  (displayln "1. Creating a Knowledge Base")
  (displayln (make-string 50 #\-))
  
  ;; Create concept nodes
  (define human (atomspace-add-node! as 'concept-node "human"))
  (define mortal (atomspace-add-node! as 'concept-node "mortal"))
  (define socrates (atomspace-add-node! as 'concept-node "Socrates"))
  (define animal (atomspace-add-node! as 'concept-node "animal"))
  
  (displayln "Created nodes:")
  (for ([nd (list human mortal socrates animal)])
    (printf "  ~a\n" nd))
  (newline)
  
  ;; Create inheritance relationships
  (displayln "2. Creating Relationships")
  (displayln (make-string 50 #\-))
  (define link1 (atomspace-add-link! as 'inheritance-link (list socrates human)))
  (define link2 (atomspace-add-link! as 'inheritance-link (list human mortal)))
  (define link3 (atomspace-add-link! as 'inheritance-link (list human animal)))
  
  ;; Set truth values (immutable updates)
  (set! link1 (link-set-tv link1 (make-tv 1.0 1.0)))
  (set! link2 (link-set-tv link2 (make-tv 1.0 1.0)))
  (set! link3 (link-set-tv link3 (make-tv 1.0 0.9)))
  
  (displayln "Created inheritance links:")
  (printf "  ~a\n" link1)
  (printf "  ~a\n" link2)
  (printf "  ~a\n" link3)
  (newline)
  
  ;; Create predicates and evaluations
  (displayln "3. Predicates and Evaluations")
  (displayln (make-string 50 #\-))
  (define breathes (atomspace-add-node! as 'predicate-node "breathes"))
  (define thinks (atomspace-add-node! as 'predicate-node "thinks"))
  
  (define eval1 (atomspace-add-link! as 'evaluation-link (list breathes socrates)))
  (define eval2 (atomspace-add-link! as 'evaluation-link (list thinks socrates)))
  
  (printf "  ~a\n" eval1)
  (printf "  ~a\n" eval2)
  (newline)
  
  ;; Query by type
  (displayln "4. Querying by Type")
  (displayln (make-string 50 #\-))
  (displayln "All concept nodes:")
  (for ([nd (atomspace-get-by-type as 'concept-node)])
    (printf "  ~a\n" nd))
  (newline)
  
  (displayln "All inheritance links:")
  (for ([lnk (atomspace-get-by-type as 'inheritance-link)])
    (printf "  ~a\n" lnk))
  (newline)
  
  ;; Query incoming links
  (displayln "5. Incoming Links (Graph Traversal)")
  (displayln (make-string 50 #\-))
  (printf "Links involving 'human':\n")
  (for ([lnk (atomspace-get-incoming as human)])
    (printf "  ~a\n" lnk))
  (newline)
  
  ;; Pattern matching
  (displayln "6. Pattern Matching Query")
  (displayln (make-string 50 #\-))
  (define results (atomspace-query as "mor"))
  (printf "Nodes matching 'mor': ~a\n" 
          (map node-name (filter node? results)))
  (newline)
  
  ;; Logical expressions
  (displayln "7. Logical Expressions")
  (displayln (make-string 50 #\-))
  (define prop1 (atomspace-add-node! as 'predicate-node "is_alive"))
  (define prop2 (atomspace-add-node! as 'predicate-node "is_wise"))
  
  (define and-link (atomspace-add-link! as 'and-link (list prop1 prop2)))
  (define or-link (atomspace-add-link! as 'or-link (list prop1 prop2)))
  
  (printf "  AND: ~a\n" and-link)
  (printf "  OR: ~a\n" or-link)
  (newline)
  
  ;; Inheritance chain finding (graph algorithm)
  (displayln "8. Finding Inheritance Chains (Graph Algorithm)")
  (displayln (make-string 50 #\-))
  (define chains (find-inheritance-chains as "Socrates"))
  (displayln "Inheritance chains from Socrates:")
  (for ([chain chains])
    (printf "  ~a\n" (string-join chain " → ")))
  (newline)
  
  ;; Complete dump
  (displayln "9. Complete AtomSpace")
  (displayln (make-string 50 #\-))
  (atomspace-print-all as)
  (newline)
  
  ;; Statistics
  (displayln "10. Statistics")
  (displayln (make-string 50 #\-))
  (atomspace-stats as)
  (newline)
  
  (displayln (make-string 70 #\=))
  (displayln "Racket AtomSpace strengths:")
  (displayln "  ✓ Contracts for type safety")
  (displayln "  ✓ Transparent structs for introspection")
  (displayln "  ✓ Immutable updates with struct-copy")
  (displayln "  ✓ Pattern matching for elegant queries")
  (displayln "  ✓ Hash tables for O(1) lookups")
  (displayln "  ✓ Sets for efficient graph operations")
  (displayln "  ✓ Higher-order functions for graph algorithms")
  (displayln (make-string 70 #\=)))

;; Run demonstration
(module+ main
  (demonstrate-atomspace))
