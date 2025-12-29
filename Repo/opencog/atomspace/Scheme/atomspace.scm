#!/usr/bin/env scheme-script
;; atomspace.scm
;;
;; OpenCog AtomSpace - Scheme Implementation
;; Hypergraph-based knowledge representation system
;;
;; This implementation demonstrates Scheme's strengths:
;; - S-expressions for code and data
;; - First-class functions and closures
;; - Homoiconicity (code is data)
;; - Functional programming patterns
;; - Tail recursion optimization

;; ===== Atom Types =====

(define-record-type atom-type
  (make-atom-type-impl name)
  atom-type?
  (name atom-type-name))

;; Node types
(define CONCEPT-NODE (make-atom-type-impl 'ConceptNode))
(define PREDICATE-NODE (make-atom-type-impl 'PredicateNode))
(define VARIABLE-NODE (make-atom-type-impl 'VariableNode))
(define NUMBER-NODE (make-atom-type-impl 'NumberNode))

;; Link types
(define INHERITANCE-LINK (make-atom-type-impl 'InheritanceLink))
(define SIMILARITY-LINK (make-atom-type-impl 'SimilarityLink))
(define EVALUATION-LINK (make-atom-type-impl 'EvaluationLink))
(define LIST-LINK (make-atom-type-impl 'ListLink))
(define AND-LINK (make-atom-type-impl 'AndLink))
(define OR-LINK (make-atom-type-impl 'OrLink))

;; ===== Truth Value =====

(define-record-type truth-value
  (make-truth-value-impl strength confidence)
  truth-value?
  (strength tv-strength)
  (confidence tv-confidence))

(define (make-truth-value strength confidence)
  (make-truth-value-impl
   (clamp strength 0.0 1.0)
   (clamp confidence 0.0 1.0)))

(define (default-truth-value)
  (make-truth-value 1.0 1.0))

(define (clamp value min-val max-val)
  (cond
    ((< value min-val) min-val)
    ((> value max-val) max-val)
    (else value)))

(define (tv->string tv)
  (format #f "<~a, ~a>" 
          (tv-strength tv) 
          (tv-confidence tv)))

;; ===== Node =====

(define-record-type node
  (make-node-impl handle atom-type name truth-value)
  node?
  (handle node-handle node-set-handle!)
  (atom-type node-atom-type)
  (name node-name)
  (truth-value node-truth-value node-set-truth-value!))

(define (make-node atom-type name)
  (make-node-impl 0 atom-type name (default-truth-value)))

(define (node->string n)
  (format #f "(~a \"~a\")" 
          (atom-type-name (node-atom-type n))
          (node-name n)))

;; ===== Link =====

(define-record-type link
  (make-link-impl handle atom-type outgoing truth-value)
  link?
  (handle link-handle link-set-handle!)
  (atom-type link-atom-type)
  (outgoing link-outgoing)
  (truth-value link-truth-value link-set-truth-value!))

(define (make-link atom-type outgoing)
  (make-link-impl 0 atom-type outgoing (default-truth-value)))

(define (link-arity l)
  (length (link-outgoing l)))

(define (link->string l)
  (let ((out-strs (map atom->string (link-outgoing l))))
    (format #f "(~a ~a)" 
            (atom-type-name (link-atom-type l))
            (string-join out-strs " "))))

;; ===== Atom Polymorphic Operations =====

(define (atom? x)
  (or (node? x) (link? x)))

(define (atom-handle a)
  (cond
    ((node? a) (node-handle a))
    ((link? a) (link-handle a))
    (else (error "Not an atom" a))))

(define (atom-set-handle! a handle)
  (cond
    ((node? a) (node-set-handle! a handle))
    ((link? a) (link-set-handle! a handle))
    (else (error "Not an atom" a))))

(define (atom-type a)
  (cond
    ((node? a) (node-atom-type a))
    ((link? a) (link-atom-type a))
    (else (error "Not an atom" a))))

(define (atom-truth-value a)
  (cond
    ((node? a) (node-truth-value a))
    ((link? a) (link-truth-value a))
    (else (error "Not an atom" a))))

(define (atom-set-truth-value! a tv)
  (cond
    ((node? a) (node-set-truth-value! a tv))
    ((link? a) (link-set-truth-value! a tv))
    (else (error "Not an atom" a))))

(define (atom->string a)
  (cond
    ((node? a) (node->string a))
    ((link? a) (link->string a))
    (else (error "Not an atom" a))))

;; ===== AtomSpace =====

(define-record-type atomspace
  (make-atomspace-impl atoms node-index next-handle)
  atomspace?
  (atoms atomspace-atoms atomspace-set-atoms!)
  (node-index atomspace-node-index atomspace-set-node-index!)
  (next-handle atomspace-next-handle atomspace-set-next-handle!))

(define (make-atomspace)
  (make-atomspace-impl
   (make-hash-table)    ; handle -> atom
   (make-hash-table)    ; (type . name) -> handle
   1))                  ; next handle

;; ===== AtomSpace Operations =====

(define (atomspace-add-node! as atom-type name)
  (let* ((key (cons (atom-type-name atom-type) name))
         (existing (hash-table-ref/default 
                     (atomspace-node-index as) 
                     key 
                     #f)))
    (if existing
        ;; Return existing node
        (hash-table-ref (atomspace-atoms as) existing)
        ;; Create new node
        (let ((node (make-node atom-type name))
              (handle (atomspace-next-handle as)))
          (node-set-handle! node handle)
          (hash-table-set! (atomspace-atoms as) handle node)
          (hash-table-set! (atomspace-node-index as) key handle)
          (atomspace-set-next-handle! as (+ handle 1))
          node))))

(define (atomspace-add-link! as atom-type outgoing)
  ;; TODO: Check for duplicate links
  (let ((link (make-link atom-type outgoing))
        (handle (atomspace-next-handle as)))
    (link-set-handle! link handle)
    (hash-table-set! (atomspace-atoms as) handle link)
    (atomspace-set-next-handle! as (+ handle 1))
    link))

(define (atomspace-get-atom as handle)
  (hash-table-ref/default (atomspace-atoms as) handle #f))

(define (atomspace-get-node as atom-type name)
  (let* ((key (cons (atom-type-name atom-type) name))
         (handle (hash-table-ref/default 
                   (atomspace-node-index as) 
                   key 
                   #f)))
    (if handle
        (hash-table-ref (atomspace-atoms as) handle)
        #f)))

(define (atomspace-remove-atom! as handle)
  (let ((atom (hash-table-ref/default (atomspace-atoms as) handle #f)))
    (when atom
      ;; Remove from node index if it's a node
      (when (node? atom)
        (let ((key (cons (atom-type-name (node-atom-type atom))
                        (node-name atom))))
          (hash-table-delete! (atomspace-node-index as) key)))
      ;; Remove from atoms map
      (hash-table-delete! (atomspace-atoms as) handle)
      #t)))

(define (atomspace-get-all-atoms as)
  (hash-table-values (atomspace-atoms as)))

(define (atomspace-get-atoms-by-type as atom-type)
  (let ((type-name (atom-type-name atom-type)))
    (filter (lambda (atom)
              (equal? (atom-type-name (atom-type atom)) type-name))
            (atomspace-get-all-atoms as))))

(define (atomspace-size as)
  (hash-table-size (atomspace-atoms as)))

(define (atomspace-empty? as)
  (= 0 (atomspace-size as)))

(define (atomspace-clear! as)
  (hash-table-clear! (atomspace-atoms as))
  (hash-table-clear! (atomspace-node-index as))
  (atomspace-set-next-handle! as 1))

;; ===== Convenience Functions =====

(define (concept-node as name)
  (atomspace-add-node! as CONCEPT-NODE name))

(define (inheritance-link as from to)
  (atomspace-add-link! as INHERITANCE-LINK (list from to)))

;; ===== Helper Functions =====

(define (string-join lst delimiter)
  (if (null? lst)
      ""
      (fold-left (lambda (acc s)
                   (if (string-null? acc)
                       s
                       (string-append acc delimiter s)))
                 ""
                 lst)))

(define (fold-left proc init lst)
  (if (null? lst)
      init
      (fold-left proc (proc init (car lst)) (cdr lst))))

;; Format function for implementations without it
(define (format dest fmt . args)
  (let ((s (apply sprintf fmt args)))
    (if (eq? dest #f)
        s
        (begin
          (display s dest)
          (void)))))

(define (sprintf fmt . args)
  ;; Simple sprintf implementation
  (define (replace-args str args)
    (if (null? args)
        str
        (let* ((arg (car args))
               (arg-str (cond
                          ((string? arg) arg)
                          ((symbol? arg) (symbol->string arg))
                          ((number? arg) (number->string arg))
                          (else (format #f "~a" arg))))
               (new-str (regexp-replace str "~a" arg-str)))
          (replace-args new-str (cdr args)))))
  (replace-args fmt args))

;; Simple hash table implementation if needed
(define (make-hash-table)
  (list 'hash-table '()))

(define (hash-table? ht)
  (and (pair? ht) (eq? (car ht) 'hash-table)))

(define (hash-table-ref ht key)
  (let ((pair (assoc key (cadr ht))))
    (if pair (cdr pair) (error "Key not found" key))))

(define (hash-table-ref/default ht key default)
  (let ((pair (assoc key (cadr ht))))
    (if pair (cdr pair) default)))

(define (hash-table-set! ht key value)
  (let ((alist (cadr ht)))
    (let ((pair (assoc key alist)))
      (if pair
          (set-cdr! pair value)
          (set-car! (cdr ht) (cons (cons key value) alist))))))

(define (hash-table-delete! ht key)
  (set-car! (cdr ht)
            (filter (lambda (p) (not (equal? (car p) key)))
                   (cadr ht))))

(define (hash-table-size ht)
  (length (cadr ht)))

(define (hash-table-clear! ht)
  (set-car! (cdr ht) '()))

(define (hash-table-keys ht)
  (map car (cadr ht)))

(define (hash-table-values ht)
  (map cdr (cadr ht)))

;; ===== Demo Function =====

(define (demo)
  (display "======================================================================\n")
  (display "OpenCog AtomSpace - Scheme Implementation Demo\n")
  (display "Showcasing S-expression based hypergraph knowledge representation\n")
  (display "======================================================================\n")
  (newline)

  ;; Create AtomSpace
  (display "1. Creating AtomSpace\n")
  (display "----------------------------------------------------------------------\n")
  (define as (make-atomspace))
  (display "  Created empty AtomSpace: size = ")
  (display (atomspace-size as))
  (newline)
  (newline)

  ;; Add nodes
  (display "2. Adding Nodes\n")
  (display "----------------------------------------------------------------------\n")
  (define human (atomspace-add-node! as CONCEPT-NODE "human"))
  (define mortal (atomspace-add-node! as CONCEPT-NODE "mortal"))
  (define socrates (atomspace-add-node! as CONCEPT-NODE "Socrates"))

  (display "  Added nodes:\n")
  (display "    ")
  (display (node->string human))
  (newline)
  (display "    ")
  (display (node->string mortal))
  (newline)
  (display "    ")
  (display (node->string socrates))
  (newline)
  (display "  AtomSpace size: ")
  (display (atomspace-size as))
  (newline)
  (newline)

  ;; Add links
  (display "3. Adding Links\n")
  (display "----------------------------------------------------------------------\n")
  (define link1 (atomspace-add-link! as INHERITANCE-LINK (list human mortal)))
  (define link2 (atomspace-add-link! as INHERITANCE-LINK (list socrates human)))

  (display "  Added links:\n")
  (display "    ")
  (display (link->string link1))
  (newline)
  (display "    ")
  (display (link->string link2))
  (newline)
  (display "  AtomSpace size: ")
  (display (atomspace-size as))
  (newline)
  (newline)

  ;; Truth values
  (display "4. Truth Values\n")
  (display "----------------------------------------------------------------------\n")
  (define tv-strong (make-truth-value 0.95 0.90))
  (define tv-weak (make-truth-value 0.60 0.50))

  (atom-set-truth-value! link1 tv-strong)
  (atom-set-truth-value! link2 tv-weak)

  (display "  Link 1 TV: ")
  (display (tv->string (atom-truth-value link1)))
  (newline)
  (display "  Link 2 TV: ")
  (display (tv->string (atom-truth-value link2)))
  (newline)
  (newline)

  ;; Query operations
  (display "5. Query Operations\n")
  (display "----------------------------------------------------------------------\n")
  (define retrieved (atomspace-get-node as CONCEPT-NODE "human"))
  (when retrieved
    (display "  Retrieved node: ")
    (display (node->string retrieved))
    (newline))

  (define concepts (atomspace-get-atoms-by-type as CONCEPT-NODE))
  (display "  Concept nodes (")
  (display (length concepts))
  (display "):\n")
  (for-each (lambda (c)
              (display "    ")
              (display (atom->string c))
              (newline))
            concepts)

  (define links (atomspace-get-atoms-by-type as INHERITANCE-LINK))
  (display "  Inheritance links (")
  (display (length links))
  (display "):\n")
  (for-each (lambda (l)
              (display "    ")
              (display (atom->string l))
              (newline))
            links)
  (newline)

  ;; Performance demonstration
  (display "6. Performance - Batch Operations\n")
  (display "----------------------------------------------------------------------\n")
  (display "  Creating 100 nodes...\n")
  (let ((start (current-time)))
    (let loop ((i 0))
      (when (< i 100)
        (atomspace-add-node! as CONCEPT-NODE 
                            (string-append "concept_" (number->string i)))
        (loop (+ i 1))))
    (let ((elapsed (time-difference (current-time) start)))
      (display "  Created 100 nodes\n")
      (display "  Final size: ")
      (display (atomspace-size as))
      (newline)))
  (newline)

  (display "======================================================================\n")
  (display "Scheme AtomSpace strengths demonstrated:\n")
  (display "  ✓ S-expressions for code and data\n")
  (display "  ✓ First-class functions and closures\n")
  (display "  ✓ Homoiconicity (code is data)\n")
  (display "  ✓ Functional programming patterns\n")
  (display "  ✓ Tail recursion optimization\n")
  (display "  ✓ Simple and elegant syntax\n")
  (display "======================================================================\n"))

;; Run demo if executed as script
(when (string=? (car (command-line)) "atomspace.scm")
  (demo))
