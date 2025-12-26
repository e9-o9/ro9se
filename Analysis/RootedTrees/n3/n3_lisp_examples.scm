;;; ============================================================================
;;; THE DUALITY AT N=3: CONCRETE LISP EXAMPLES
;;; ============================================================================
;;;
;;; This file demonstrates how the two 3-node tree structures correspond to
;;; fundamental Lisp operations and patterns.
;;;
;;; Tree 1: ((()))  - Linear/Nested structure
;;; Tree 2: (()())  - Branched/Adjacent structure
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; PART 1: DATA STRUCTURE INTERPRETATION
;;; ----------------------------------------------------------------------------

;;; Define nil for Scheme
(define nil '())

;;; Tree 1: ((()))  - Nested List
(define nested-structure '((())))
;;; Structure: A list containing a list containing nil
;;; Access pattern: (car (car nested-structure)) → nil
;;; Depth: 3 levels

;;; Tree 2: (()())  - Flat List with Two Elements
(define flat-structure '(() ()))
;;; Structure: A list containing two nil elements
;;; Access pattern: (car flat-structure) → nil, (cadr flat-structure) → nil
;;; Breadth: 2 elements

(display "Data Structure Examples:\n")
(display "Nested: ") (display nested-structure) (newline)
(display "Flat:   ") (display flat-structure) (newline)
(newline)

;;; ----------------------------------------------------------------------------
;;; PART 2: FUNCTION COMPOSITION VS. APPLICATION
;;; ----------------------------------------------------------------------------

;;; Tree 1: ((()))  → Function Composition
;;; Pattern: f(g(x)) - output of g becomes input of f

(define (add1 x) (+ x 1))
(define (double x) (* x 2))

;;; Nested application (composition)
(define composed-result 
  (add1 (double 5)))  ; Structure: ((double 5))
                       ; Then: (add1 10)
                       ; Result: 11

(display "Composition Example (Tree 1 pattern):\n")
(display "  (add1 (double 5)) = ") (display composed-result) (newline)
(display "  Structure: ((()))  - nested function calls\n")
(newline)

;;; Tree 2: (()())  → Function Application with Multiple Arguments
;;; Pattern: f(x, y) - independent arguments combined

(define (add x y) (+ x y))

(define applied-result
  (add 5 10))  ; Structure: (add 5 10) ~ (()())
               ; Two independent arguments

(display "Application Example (Tree 2 pattern):\n")
(display "  (add 5 10) = ") (display applied-result) (newline)
(display "  Structure: (()())  - multiple independent arguments\n")
(newline)

;;; ----------------------------------------------------------------------------
;;; PART 3: CAR/CDR NAVIGATION PATTERNS
;;; ----------------------------------------------------------------------------

;;; Tree 1: ((()))  - Deep Navigation (car of car)
(define deep-list '((42)))

(display "Deep Navigation (Tree 1 pattern):\n")
(display "  List: ") (display deep-list) (newline)
(display "  (car deep-list) = ") (display (car deep-list)) (newline)
(display "  (car (car deep-list)) = ") (display (car (car deep-list))) (newline)
(display "  Pattern: Nested car operations\n")
(newline)

;;; Tree 2: (()())  - Lateral Navigation (car and cadr)
(define wide-list '(42 99))

(display "Lateral Navigation (Tree 2 pattern):\n")
(display "  List: ") (display wide-list) (newline)
(display "  (car wide-list) = ") (display (car wide-list)) (newline)
(display "  (cadr wide-list) = ") (display (cadr wide-list)) (newline)
(display "  Pattern: Sequential car/cdr operations\n")
(newline)

;;; ----------------------------------------------------------------------------
;;; PART 4: CONS CONSTRUCTION PATTERNS
;;; ----------------------------------------------------------------------------

;;; Tree 1: ((()))  - Nested Cons
(define nested-cons
  (cons (cons '() '()) '()))  ; Creates ((nil) . nil) ≈ ((nil))

(display "Nested Cons (Tree 1 pattern):\n")
(display "  (cons (cons nil nil) nil) = ") (display nested-cons) (newline)
(display "  Structure: Cons inside cons\n")
(newline)

;;; Tree 2: (()())  - Adjacent Cons
(define adjacent-cons
  (cons '() (cons '() '())))  ; Creates (nil nil)

(display "Adjacent Cons (Tree 2 pattern):\n")
(display "  (cons nil (cons nil nil)) = ") (display adjacent-cons) (newline)
(display "  Structure: Cons beside cons\n")
(newline)

;;; ----------------------------------------------------------------------------
;;; PART 5: LAMBDA CALCULUS INTERPRETATION
;;; ----------------------------------------------------------------------------

;;; Tree 1: ((()))  → Nested Lambda (Currying)
;;; λx.(λy.y) - A function returning a function

(define curried-function
  (lambda (x)
    (lambda (y) y)))  ; Returns identity function

(display "Curried Function (Tree 1 pattern):\n")
(display "  (lambda (x) (lambda (y) y))\n")
(display "  Calling ((curried-function 5) 10) = ")
(display ((curried-function 5) 10)) (newline)
(display "  Structure: Lambda inside lambda\n")
(newline)

;;; Tree 2: (()())  → Lambda with Multiple Parameters
;;; λx.λy.(x, y) - A function taking two arguments

(define multi-param-function
  (lambda (x y) (cons x y)))  ; Takes two arguments at once

(display "Multi-Parameter Function (Tree 2 pattern):\n")
(display "  (lambda (x y) (cons x y))\n")
(display "  Calling (multi-param-function 5 10) = ")
(display (multi-param-function 5 10)) (newline)
(display "  Structure: Multiple parameters side-by-side\n")
(newline)

;;; ----------------------------------------------------------------------------
;;; PART 6: CONTROL FLOW PATTERNS
;;; ----------------------------------------------------------------------------

;;; Tree 1: ((()))  → Sequential Execution (begin)
(define sequential-result
  (begin
    (begin
      (display "    Step 1: Innermost\n")
      42)))

(display "Sequential Execution (Tree 1 pattern):\n")
(display "  (begin (begin ...))\n")
(display "  Result: ") (display sequential-result) (newline)
(display "  Structure: Nested begin blocks\n")
(newline)

;;; Tree 2: (()())  → Conditional Branching (if with two branches)
(define branch-result
  (if #t
      (display "    Branch 1: True\n")
      (display "    Branch 2: False\n")))

(display "Conditional Branching (Tree 2 pattern):\n")
(display "  (if condition branch1 branch2)\n")
(display "  Structure: Two alternative paths\n")
(newline)

;;; ----------------------------------------------------------------------------
;;; PART 7: RECURSION PATTERNS
;;; ----------------------------------------------------------------------------

;;; Tree 1: ((()))  → Linear Recursion (single recursive call)
(define (factorial-linear n)
  (if (<= n 1)
      1
      (* n (factorial-linear (- n 1)))))  ; Single nested recursive call

(display "Linear Recursion (Tree 1 pattern):\n")
(display "  (factorial-linear 5) = ") (display (factorial-linear 5)) (newline)
(display "  Structure: f(n, f(n-1)) - nested single call\n")
(newline)

;;; Tree 2: (()())  → Tree Recursion (multiple recursive calls)
(define (fib-tree n)
  (if (<= n 1)
      n
      (+ (fib-tree (- n 1))
         (fib-tree (- n 2)))))  ; Two parallel recursive calls

(display "Tree Recursion (Tree 2 pattern):\n")
(display "  (fib-tree 6) = ") (display (fib-tree 6)) (newline)
(display "  Structure: f(n-1, n-2) - two parallel calls\n")
(newline)

;;; ----------------------------------------------------------------------------
;;; PART 8: LIST PROCESSING PATTERNS
;;; ----------------------------------------------------------------------------

;;; Tree 1: ((()))  → Reduce/Fold (sequential accumulation)
(define (my-fold-right op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (my-fold-right op init (cdr lst)))))  ; Nested recursive structure

(display "Fold/Reduce (Tree 1 pattern):\n")
(display "  (fold-right + 0 '(1 2 3)) = ")
(display (my-fold-right + 0 '(1 2 3))) (newline)
(display "  Structure: op(x1, op(x2, op(x3, init))) - nested\n")
(newline)

;;; Tree 2: (()())  → Map (parallel transformation)
(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (my-map f (cdr lst)))))  ; Creates parallel structure

(display "Map (Tree 2 pattern):\n")
(display "  (map add1 '(1 2 3)) = ")
(display (my-map add1 '(1 2 3))) (newline)
(display "  Structure: (f(x1) f(x2) f(x3)) - parallel\n")
(newline)

;;; ----------------------------------------------------------------------------
;;; PART 9: COMBINATOR INTERPRETATION
;;; ----------------------------------------------------------------------------

;;; Tree 1: ((()))  → K Combinator (constant function)
;;; K = λx.λy.x - returns first argument, ignores second
;;; Structure is actually more like (()()) but conceptually nested

(define K
  (lambda (x)
    (lambda (y) x)))

(display "K Combinator (conceptually Tree 1):\n")
(display "  ((K 42) 99) = ") (display ((K 42) 99)) (newline)
(display "  Returns first, ignores second - nested evaluation\n")
(newline)

;;; Tree 2: (()())  → Pair/Cons (data structure)
;;; Creates a pair of two independent values

(define pair-example (cons 42 99))

(display "Pair/Cons (Tree 2 pattern):\n")
(display "  (cons 42 99) = ") (display pair-example) (newline)
(display "  Two independent values side-by-side\n")
(newline)

;;; ----------------------------------------------------------------------------
;;; PART 10: EVALUATION ORDER
;;; ----------------------------------------------------------------------------

;;; Tree 1: ((()))  → Applicative Order (inside-out)
(display "Applicative Order (Tree 1 pattern):\n")
(display "  (+ (* 2 3) 4)\n")
(display "  Evaluates (* 2 3) first (innermost)\n")
(display "  Then (+ 6 4)\n")
(display "  Result: ") (display (+ (* 2 3) 4)) (newline)
(newline)

;;; Tree 2: (()())  → Normal Order (left-to-right)
(display "Normal Order (Tree 2 pattern):\n")
(display "  In lazy evaluation: (or #t (error))\n")
(display "  Evaluates left argument first\n")
(display "  Short-circuits, never evaluates right\n")
(display "  Result: ") (display (or #t #f)) (newline)
(newline)

;;; ----------------------------------------------------------------------------
;;; SUMMARY
;;; ----------------------------------------------------------------------------

(display "============================================================================\n")
(display "SUMMARY: THE DUALITY AT N=3\n")
(display "============================================================================\n")
(display "\n")
(display "Tree 1: ((()))  - NESTING/COMPOSITION\n")
(display "  • Deep structure (vertical)\n")
(display "  • Sequential processing\n")
(display "  • Function composition\n")
(display "  • Nested contexts\n")
(display "  • Linear recursion\n")
(display "  • Reduce/fold operations\n")
(display "  • Inside-out evaluation\n")
(display "\n")
(display "Tree 2: (()())  - ADJACENCY/APPLICATION\n")
(display "  • Wide structure (horizontal)\n")
(display "  • Parallel processing\n")
(display "  • Function application\n")
(display "  • Independent contexts\n")
(display "  • Tree recursion\n")
(display "  • Map operations\n")
(display "  • Left-to-right evaluation\n")
(display "\n")
(display "These two patterns are the FUNDAMENTAL BUILDING BLOCKS of all computation.\n")
(display "Every complex program is built by combining and nesting these two structures.\n")
(display "============================================================================\n")
