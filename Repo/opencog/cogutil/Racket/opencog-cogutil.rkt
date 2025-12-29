#!/usr/bin/env racket
#lang racket
;; opencog-cogutil.rkt
;;
;; OpenCog Cogutil - Racket Utility Library
;; A collection of utility functions for OpenCog framework
;;
;; This single-file implementation demonstrates Racket's strengths:
;; - Contracts for runtime type checking
;; - Structs with introspection
;; - Pattern matching
;; - Modules and namespaces
;; - Macros for DSL creation
;; - Hash tables for efficient data storage

(require racket/contract
         racket/match
         racket/hash
         racket/string)

;; ===== Logger System =====
;; Demonstrates: Structs with contracts, pattern matching

(struct logger (name level) #:transparent
  #:guard (struct-guard/c string? 
                         (or/c 'debug 'info 'warn 'error)))

(define (make-logger name [level 'info])
  (logger name level))

(define log-level-values
  (hash 'debug 0
        'info 1
        'warn 2
        'error 3))

(define/contract (log-level-value level)
  (-> (or/c 'debug 'info 'warn 'error) exact-nonnegative-integer?)
  (hash-ref log-level-values level))

(define (current-timestamp)
  (let* ([dt (current-date)]
         [h (date-hour dt)]
         [m (date-minute dt)]
         [s (date-second dt)])
    (format "~a:~a:~a" 
            (~r h #:min-width 2 #:pad-string "0")
            (~r m #:min-width 2 #:pad-string "0")
            (~r s #:min-width 2 #:pad-string "0"))))

(define/contract (log-message lgr level msg)
  (-> logger? (or/c 'debug 'info 'warn 'error) string? void?)
  (when (>= (log-level-value level) 
            (log-level-value (logger-level lgr)))
    (printf "[~a] ~a: ~a\n" 
            (current-timestamp)
            (string-upcase (symbol->string level))
            msg)))

;; Convenience functions using Racket's contract system
(define/contract (log-debug lgr msg)
  (-> logger? string? void?)
  (log-message lgr 'debug msg))

(define/contract (log-info lgr msg)
  (-> logger? string? void?)
  (log-message lgr 'info msg))

(define/contract (log-warn lgr msg)
  (-> logger? string? void?)
  (log-message lgr 'warn msg))

(define/contract (log-error lgr msg)
  (-> logger? string? void?)
  (log-message lgr 'error msg))

(define/contract (logger-set-level lgr level)
  (-> logger? (or/c 'debug 'info 'warn 'error) logger?)
  (struct-copy logger lgr [level level]))

;; ===== Configuration Manager =====
;; Demonstrates: Mutable hash tables, functional updates

(struct config (data) #:transparent
  #:guard (struct-guard/c hash?))

(define (make-config)
  (config (make-hash)))

(define/contract (config-set! cfg key value)
  (-> config? string? string? void?)
  (hash-set! (config-data cfg) key value))

(define/contract (config-get cfg key [default ""])
  (->* (config? string?) (string?) string?)
  (hash-ref (config-data cfg) key default))

(define/contract (config-has? cfg key)
  (-> config? string? boolean?)
  (hash-has-key? (config-data cfg) key))

(define/contract (config-load-from-file! cfg filename)
  (-> config? path-string? boolean?)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (call-with-input-file filename
      (lambda (in)
        (for ([line (in-lines in)])
          (let ([trimmed (string-trim line)])
            (unless (or (string=? trimmed "")
                       (string-prefix? trimmed "#"))
              (match (string-split trimmed "=")
                [(list key value)
                 (config-set! cfg 
                             (string-trim key) 
                             (string-trim value))]
                [_ (void)]))))
        #t))))

(define/contract (config-dump cfg)
  (-> config? void?)
  (for ([(k v) (in-hash (config-data cfg))])
    (printf "~a = ~a\n" k v)))

;; ===== Timer Utility =====
;; Demonstrates: Closures, mutable state in functional style

(struct timer (name start-time) #:transparent #:mutable)

(define (make-timer name)
  (timer name #f))

(define/contract (timer-start! tmr)
  (-> timer? void?)
  (set-timer-start-time! tmr (current-inexact-milliseconds)))

(define/contract (timer-stop! tmr)
  (-> timer? real?)
  (if (timer-start-time tmr)
      (let ([elapsed (- (current-inexact-milliseconds) 
                       (timer-start-time tmr))])
        (set-timer-start-time! tmr #f)
        (/ elapsed 1000.0))
      0.0))

;; Macro for timing expressions
(define-syntax-rule (time-it name expr)
  (let ([tmr (make-timer name)])
    (timer-start! tmr)
    (begin0
      expr
      (let ([elapsed (timer-stop! tmr)])
        (printf "[TIMER] ~a took ~a seconds\n" name elapsed)))))

;; ===== String Utilities =====
;; Demonstrates: Pattern matching, string operations, contracts

(module+ string-utils
  (provide (all-defined-out))
  
  (define/contract (str-split text [delimiter ","])
    (->* (string?) (string?) (listof string?))
    (filter (lambda (s) (not (string=? s "")))
            (map string-trim (string-split text delimiter))))
  
  (define/contract (str-join strings [delimiter ","])
    (->* ((listof string?)) (string?) string?)
    (string-join strings delimiter))
  
  (define/contract (str-to-lower text)
    (-> string? string?)
    (string-downcase text))
  
  (define/contract (str-to-upper text)
    (-> string? string?)
    (string-upcase text))
  
  (define/contract (str-trim text)
    (-> string? string?)
    (string-trim text))
  
  (define/contract (str-reverse text)
    (-> string? string?)
    (list->string (reverse (string->list text))))
  
  (define/contract (str-contains? text substring)
    (-> string? string? boolean?)
    (string-contains? text substring))
  
  (define/contract (camel->snake text)
    (-> string? string?)
    (string-downcase
     (regexp-replace* #rx"([A-Z])" text "_\\1"))))

;; ===== Performance Measurement =====
;; Demonstrates: Higher-order functions, functional composition

(define/contract (measure-performance operation thunk)
  (-> string? (-> any) void?)
  (printf "[PERF] Starting: ~a\n" operation)
  (let ([start (current-inexact-milliseconds)])
    (thunk)
    (let ([elapsed (/ (- (current-inexact-milliseconds) start) 1000.0)])
      (printf "[PERF] Completed: ~a in ~a seconds\n" operation elapsed))))

;; ===== Demonstration =====

(define (demonstrate-cogutil)
  (displayln (make-string 70 #\=))
  (displayln "OpenCog Cogutil - Racket Utility Library Demo")
  (displayln "Showcasing Racket's strengths: Contracts, macros, pattern matching")
  (displayln (make-string 70 #\=))
  (newline)
  
  ;; Logger demonstration
  (displayln "1. Logger Demonstration with Contracts")
  (displayln (make-string 50 #\-))
  (define lgr (make-logger "CogUtil" 'info))
  (log-info lgr "Cogutil library initialized")
  (log-debug lgr "This debug message won't show (level too low)")
  (log-warn lgr "This is a warning message")
  (log-error lgr "This is an error message")
  
  (set! lgr (logger-set-level lgr 'debug))
  (log-debug lgr "Now debug messages are visible")
  (newline)
  
  ;; Config demonstration
  (displayln "2. Configuration Manager with Hash Tables")
  (displayln (make-string 50 #\-))
  (define cfg (make-config))
  (config-set! cfg "opencog.version" "1.0.0")
  (config-set! cfg "atomspace.enabled" "true")
  (config-set! cfg "cogserver.port" "17001")
  
  (log-info lgr "Configuration loaded:")
  (config-dump cfg)
  (newline)
  
  (log-info lgr (format "Port setting: ~a" (config-get cfg "cogserver.port")))
  (newline)
  
  ;; Timer demonstration
  (displayln "3. Timer with Mutable State")
  (displayln (make-string 50 #\-))
  (define tmr (make-timer "Processing"))
  (timer-start! tmr)
  (log-info lgr "Simulating some work...")
  (define total (apply + (range 1000000)))
  (define elapsed (timer-stop! tmr))
  (printf "[TIMER] Processing took ~a seconds\n" elapsed)
  (newline)
  
  ;; Macro demonstration
  (displayln "4. Timing Macro (Racket Meta-programming)")
  (displayln (make-string 50 #\-))
  (time-it "computation" (apply + (range 1000000)))
  (newline)
  
  ;; String utilities demonstration
  (displayln "5. String Utilities with Pattern Matching")
  (displayln (make-string 50 #\-))
  (require (submod "." string-utils))
  (log-info lgr "String utilities demonstration:")
  (define text "OpenCog,AtomSpace,CogServer,Cogutil")
  (define parts (str-split text ","))
  
  (log-info lgr "Split result:")
  (for ([part parts])
    (printf "  - ~a\n" part))
  
  (define joined (str-join parts " + "))
  (log-info lgr (format "Joined: ~a" joined))
  
  (log-info lgr (format "Uppercase: ~a" (str-to-upper "opencog rocks")))
  (log-info lgr (format "Lowercase: ~a" (str-to-lower "OPENCOG ROCKS")))
  (log-info lgr (format "Trimmed: '~a'" (str-trim "  spaced out  ")))
  (log-info lgr (format "Reversed: ~a" (str-reverse "Racket")))
  (newline)
  
  ;; Performance measurement
  (displayln "6. Performance Measurement")
  (displayln (make-string 50 #\-))
  (measure-performance "Data processing"
                      (lambda () (sleep 0.01)))
  (newline)
  
  ;; Pattern matching demonstration
  (displayln "7. Pattern Matching (Racket-specific)")
  (displayln (make-string 50 #\-))
  (define (describe-atom atom)
    (match atom
      [(list 'node type name)
       (format "Node of type ~a named ~a" type name)]
      [(list 'link type children ...)
       (format "Link of type ~a with ~a children" type (length children))]
      [_ "Unknown atom type"]))
  
  (log-info lgr (describe-atom '(node concept "human")))
  (log-info lgr (describe-atom '(link inheritance (node concept "Socrates") 
                                                  (node concept "human"))))
  (newline)
  
  ;; Contract violation demonstration (commented to avoid error)
  ; (displayln "8. Contract Enforcement")
  ; (displayln (make-string 50 #\-))
  ; Try uncommenting this to see contract violation:
  ; (define bad-logger (make-logger "test" 'invalid-level))
  
  (log-info lgr "Cogutil demonstration complete!")
  (displayln (make-string 70 #\=))
  (displayln "Racket strengths demonstrated:")
  (displayln "  ✓ Contracts for runtime type checking")
  (displayln "  ✓ Structs with transparency and guards")
  (displayln "  ✓ Pattern matching for elegant code")
  (displayln "  ✓ Powerful macro system for DSLs")
  (displayln "  ✓ Hash tables for efficient storage")
  (displayln "  ✓ Module system for organization")
  (displayln "  ✓ Functional programming with immutability")
  (displayln (make-string 70 #\=)))

;; Run demonstration
(module+ main
  (demonstrate-cogutil))
