;;; ============================================================================
;;; THE FOUR TREES AT N=4: CONCRETE LISP EXAMPLES
;;; ============================================================================
;;;
;;; This file demonstrates how the four 4-node tree structures correspond to
;;; fundamental computational patterns in Lisp.
;;;
;;; Tree 1: (((())))  - Maximum Chain (Pure Vertical)
;;; Tree 2: ((()()))  - Nested Fork (Vertical of Horizontal)
;;; Tree 3: ((())())  - Asymmetric Split (Horizontal with Vertical)
;;; Tree 4: (()()())  - Flat Forest (Pure Horizontal)
;;; ============================================================================

(define nil '())

;;; ----------------------------------------------------------------------------
;;; TREE 1: (((())))  - THE MAXIMUM CHAIN
;;; ----------------------------------------------------------------------------
;;; Pattern: Pure sequential composition, maximum depth
;;; Computational meaning: f(g(h(x))) - three-level pipeline

(display "============================================================================\n")
(display "TREE 1: (((())))  - THE MAXIMUM CHAIN (Pure Vertical)\n")
(display "============================================================================\n\n")

;;; Example 1: Three-Level Function Composition
(define (add1 x) (+ x 1))
(define (double x) (* x 2))
(define (square x) (* x x))

(define chain-result
  (add1 (double (square 3))))
;; Evaluation: (square 3) → 9
;;            (double 9) → 18
;;            (add1 18) → 19

(display "Function Composition (3 levels):\n")
(display "  (add1 (double (square 3)))\n")
(display "  Structure: ((((x))))\n")
(display "  Evaluation: 3 → 9 → 18 → 19\n")
(display "  Result: ") (display chain-result) (newline)
(newline)

;;; Example 2: Church Numeral 3
(display "Church Numeral Interpretation:\n")
(display "  (((())))  represents the number 3\n")
(display "  Meaning: Apply a function 3 times\n")
(display "  λf.λx.f(f(f(x)))\n")
(newline)

;;; Example 3: Deeply Nested List
(define deep-list '(((42))))

(display "Data Structure:\n")
(display "  List: ") (display deep-list) (newline)
(display "  Access: (car (car (car deep-list))) = ")
(display (car (car (car deep-list)))) (newline)
(display "  Pattern: Three levels of nesting\n")
(newline)

;;; Example 4: Linear Recursion with 3 Recursive Calls
(define (countdown n)
  (if (<= n 0)
      '()
      (cons n (countdown (- n 1)))))

(display "Linear Recursion:\n")
(display "  (countdown 3) = ") (display (countdown 3)) (newline)
(display "  Call stack: countdown(3) → countdown(2) → countdown(1) → countdown(0)\n")
(display "  Structure: ((((x)))) - linear chain of calls\n")
(newline)

;;; ----------------------------------------------------------------------------
;;; TREE 2: ((()()))  - THE NESTED FORK
;;; ----------------------------------------------------------------------------
;;; Pattern: Vertical composition of horizontal application
;;; Computational meaning: f(g(x, y)) - function applied to binary result

(display "============================================================================\n")
(display "TREE 2: ((()()))  - THE NESTED FORK (Vertical of Horizontal)\n")
(display "============================================================================\n\n")

;;; Example 1: Composition with Binary Function
(define (add x y) (+ x y))
(define (triple x) (* x 3))

(define nested-fork-result
  (triple (add 5 10)))
;; Evaluation: (add 5 10) → 15
;;            (triple 15) → 45

(display "Composition with Binary Function:\n")
(display "  (triple (add 5 10))\n")
(display "  Structure: ((()()))\n")
(display "  Evaluation: (5, 10) → 15 → 45\n")
(display "  Result: ") (display nested-fork-result) (newline)
(newline)

;;; Example 2: Map followed by Fold
(display "List Processing Pipeline:\n")
(display "  (fold + 0 (map add1 '(1 2)))\n")
(display "  Step 1: (map add1 '(1 2)) → (2 3)  [binary operation]\n")
(display "  Step 2: (fold + 0 '(2 3)) → 5     [unary operation on result]\n")
(display "  Structure: f(g(x, y)) pattern\n")
(newline)

;;; Example 3: Nested List with Two Elements
(define nested-fork-list '((5 10)))

(display "Data Structure:\n")
(display "  List: ") (display nested-fork-list) (newline)
(display "  Structure: A list containing a two-element list\n")
(display "  Access: (car nested-fork-list) = ") (display (car nested-fork-list)) (newline)
(newline)

;;; Example 4: Conditional with Binary Result
(display "Control Flow:\n")
(display "  (if condition (+ a b) default)\n")
(display "  The 'then' branch is a binary operation\n")
(display "  Structure: Nested decision with binary computation\n")
(newline)

;;; ----------------------------------------------------------------------------
;;; TREE 3: ((())())  - THE ASYMMETRIC SPLIT
;;; ----------------------------------------------------------------------------
;;; Pattern: Horizontal application with one nested argument
;;; Computational meaning: f(g(h(x)), y) - partial composition

(display "============================================================================\n")
(display "TREE 3: ((())())  - THE ASYMMETRIC SPLIT (Horizontal with Vertical)\n")
(display "============================================================================\n\n")

;;; Example 1: Binary Function with One Composed Argument
(define (multiply x y) (* x y))

(define asymmetric-result
  (multiply (double (square 2)) 5))
;; Evaluation: (square 2) → 4
;;            (double 4) → 8
;;            (multiply 8 5) → 40

(display "Binary Function with Composed Argument:\n")
(display "  (multiply (double (square 2)) 5)\n")
(display "  Structure: ((())())\n")
(display "  Evaluation: 2 → 4 → 8, then (8, 5) → 40\n")
(display "  Result: ") (display asymmetric-result) (newline)
(newline)

;;; Example 2: Cons with Nested Car
(define asymmetric-list '((42) 99))

(display "Data Structure:\n")
(display "  List: ") (display asymmetric-list) (newline)
(display "  Structure: Two elements, first is nested\n")
(display "  (car asymmetric-list) = ") (display (car asymmetric-list)) (newline)
(display "  (cadr asymmetric-list) = ") (display (cadr asymmetric-list)) (newline)
(newline)

;;; Example 3: Partial Application Pattern
(display "Partial Application:\n")
(display "  ((lambda (x) (lambda (y) (+ x y))) 10)\n")
(display "  Returns: (lambda (y) (+ 10 y))\n")
(display "  Then apply: ((result) 5) → 15\n")
(display "  Structure: Nested lambda applied to simple argument\n")
(newline)

;;; Example 4: Mixed Recursion
(define (mixed-recursion n acc)
  (if (<= n 0)
      acc
      (mixed-recursion (- n 1) (+ acc n))))

(display "Tail Recursion with Accumulator:\n")
(display "  (mixed-recursion 3 0)\n")
(display "  Pattern: Recursive call with composed first arg, simple second\n")
(display "  Result: ") (display (mixed-recursion 3 0)) (newline)
(newline)

;;; ----------------------------------------------------------------------------
;;; TREE 4: (()()())  - THE FLAT FOREST
;;; ----------------------------------------------------------------------------
;;; Pattern: Pure horizontal application, maximum breadth
;;; Computational meaning: f(x, y, z) - ternary function

(display "============================================================================\n")
(display "TREE 4: (()()())  - THE FLAT FOREST (Pure Horizontal)\n")
(display "============================================================================\n\n")

;;; Example 1: Ternary Function
(define (sum3 x y z) (+ x y z))

(define flat-result
  (sum3 10 20 30))

(display "Ternary Function:\n")
(display "  (sum3 10 20 30)\n")
(display "  Structure: (()()())\n")
(display "  All arguments independent\n")
(display "  Result: ") (display flat-result) (newline)
(newline)

;;; Example 2: Three-Element List
(define flat-list '(1 2 3))

(display "Data Structure:\n")
(display "  List: ") (display flat-list) (newline)
(display "  Structure: Three independent elements\n")
(display "  (car flat-list) = ") (display (car flat-list)) (newline)
(display "  (cadr flat-list) = ") (display (cadr flat-list)) (newline)
(display "  (caddr flat-list) = ") (display (caddr flat-list)) (newline)
(newline)

;;; Example 3: If Expression (condition, then, else)
(display "Control Flow:\n")
(display "  (if #t 'yes 'no)\n")
(display "  Three independent parts: condition, then-branch, else-branch\n")
(display "  Result: ") (display (if #t 'yes 'no)) (newline)
(newline)

;;; Example 4: Parallel Map Operations
(display "Parallel Operations:\n")
(display "  (list (add1 1) (add1 2) (add1 3))\n")
(display "  Each operation is independent\n")
(display "  Result: ") (display (list (add1 1) (add1 2) (add1 3))) (newline)
(newline)

;;; ----------------------------------------------------------------------------
;;; COMPARATIVE ANALYSIS
;;; ----------------------------------------------------------------------------

(display "============================================================================\n")
(display "COMPARATIVE ANALYSIS: THE FOUR PATTERNS AT N=4\n")
(display "============================================================================\n\n")

(display "Depth vs. Breadth Spectrum:\n\n")
(display "  (((())))    Depth: 4, Width: 1  - Maximum Sequential\n")
(display "  ((()()))    Depth: 3, Width: 2  - Nested Binary\n")
(display "  ((())())    Depth: 3, Width: 2  - Asymmetric Mix\n")
(display "  (()()())    Depth: 2, Width: 3  - Maximum Parallel\n")
(newline)

(display "Computational Patterns:\n\n")
(display "  1. (((())))  - Triple Composition:  f(g(h(x)))\n")
(display "  2. ((()()))  - Composed Binary:     f(g(x, y))\n")
(display "  3. ((())())  - Partial Composition: f(g(h(x)), y)\n")
(display "  4. (()()())  - Ternary Application: f(x, y, z)\n")
(newline)

(display "Recursion Patterns:\n\n")
(display "  1. (((())))  - Linear recursion (3 deep)\n")
(display "  2. ((()()))  - Binary tree with depth 2\n")
(display "  3. ((())())  - Asymmetric tree (left-heavy)\n")
(display "  4. (()()())  - Ternary tree (flat)\n")
(newline)

(display "Evaluation Strategies:\n\n")
(display "  1. (((())))  - Strictly sequential (inside-out)\n")
(display "  2. ((()()))  - Binary then unary\n")
(display "  3. ((())())  - Mixed (one composed, one direct)\n")
(display "  4. (()()())  - Fully parallel (all independent)\n")
(newline)

;;; ----------------------------------------------------------------------------
;;; COMPOSITION FROM N=3 PATTERNS
;;; ----------------------------------------------------------------------------

(display "============================================================================\n")
(display "HOW N=4 COMPOSES THE N=3 DUALITY\n")
(display "============================================================================\n\n")

(display "Recall the n=3 patterns:\n")
(display "  A = ((()))   - Vertical (nesting)\n")
(display "  B = (()())   - Horizontal (adjacency)\n\n")

(display "The n=4 patterns are compositions:\n\n")

(display "  1. (((())))  = A + ()    - Deepen A by adding () inside\n")
(display "                            - Vertical composed with vertical\n\n")

(display "  2. ((()()))  = (B)       - Nest B inside ()\n")
(display "                            - Vertical containing horizontal\n\n")

(display "  3. ((())())  = A + ()    - Juxtapose A with () horizontally\n")
(display "                            - Horizontal of vertical and atom\n\n")

(display "  4. (()()())  = B + ()    - Extend B by adding () beside\n")
(display "                            - Horizontal composed with horizontal\n\n")

(display "Key Insight:\n")
(display "  N=4 is where the fundamental duality COMPOSES WITH ITSELF.\n")
(display "  We get: Vertical+Vertical, Vertical(Horizontal),\n")
(display "          Horizontal(Vertical), Horizontal+Horizontal\n\n")

(display "This compositional growth drives the exponential explosion:\n")
(display "  n=1: 1 tree\n")
(display "  n=2: 1 tree\n")
(display "  n=3: 2 trees (the fundamental duality)\n")
(display "  n=4: 4 trees (duality composes with itself)\n")
(display "  n=5: 9 trees (compositions of compositions)\n")
(display "  ...\n\n")

(display "============================================================================\n")
(display "CONCLUSION\n")
(display "============================================================================\n\n")

(display "The four trees at n=4 represent the VOCABULARY OF BASIC COMPUTATION:\n\n")

(display "  1. Pure Sequential Processing    - (((())))   - Pipelines\n")
(display "  2. Divide-and-Conquer            - ((()()))   - Binary algorithms\n")
(display "  3. Partial Application           - ((())())   - Currying, closures\n")
(display "  4. Multi-Argument Functions      - (()()())   - Standard functions\n\n")

(display "Every complex program is built by recursively combining these patterns.\n")
(display "This is the essence of computational structure.\n")
(display "============================================================================\n")
