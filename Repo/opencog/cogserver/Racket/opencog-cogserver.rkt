#!/usr/bin/env racket
#lang racket
;; opencog-cogserver.rkt
;;
;; OpenCog CogServer - Network Server for AtomSpace Access in Racket
;;
;; This single-file implementation demonstrates Racket's strengths:
;; - First-class functions for command system
;; - Hash tables for efficient command registry
;; - Pattern matching for command parsing
;; - Structs with contracts
;; - Interactive REPL building

(require racket/contract
         racket/match
         racket/hash
         racket/date)

;; ===== Command Result =====
;; Demonstrates: Simple data structures with contracts

(struct command-result (success message)
  #:transparent
  #:guard (struct-guard/c boolean? string?))

(define (make-success msg)
  (command-result #t msg))

(define (make-failure msg)
  (command-result #f msg))

(define (result->string result)
  (command-result-message result))

;; ===== Command Registry =====
;; Demonstrates: Hash tables, first-class functions, contracts

(struct command-info (handler description)
  #:transparent
  #:guard (struct-guard/c procedure? string?))

(struct command-registry (commands)
  #:transparent
  #:mutable
  #:guard (struct-guard/c hash?))

(define (make-command-registry)
  (command-registry (make-hash)))

(define/contract (registry-register! reg name handler [description ""])
  (->* (command-registry? string? procedure?) (string?) void?)
  (hash-set! (command-registry-commands reg) 
             name 
             (command-info handler description)))

(define/contract (registry-execute reg name args)
  (-> command-registry? string? (listof string?) command-result?)
  (define cmd-info (hash-ref (command-registry-commands reg) name #f))
  (if cmd-info
      (with-handlers ([exn:fail? 
                       (lambda (e) 
                         (make-failure (format "Error: ~a" (exn-message e))))])
        ((command-info-handler cmd-info) args))
      (make-failure (format "Unknown command: ~a" name))))

(define/contract (registry-list-commands reg)
  (-> command-registry? (listof (list/c string? string?)))
  (sort (for/list ([(name info) (in-hash (command-registry-commands reg))])
          (list name (command-info-description info)))
        string<?
        #:key first))

(define/contract (registry-has-command? reg name)
  (-> command-registry? string? boolean?)
  (hash-has-key? (command-registry-commands reg) name))

;; ===== Session Management =====
;; Demonstrates: Mutable state, timestamps, property access

(struct session (id created-at data authenticated?)
  #:transparent
  #:mutable
  #:guard (lambda (id created-at data auth? name)
            (values id created-at data auth?)))

(define (make-session id)
  (session id (current-seconds) (make-hash) #f))

(define/contract (session-set! sess key value)
  (-> session? string? any/c void?)
  (hash-set! (session-data sess) key value))

(define/contract (session-get sess key [default #f])
  (->* (session? string?) (any/c) any/c)
  (hash-ref (session-data sess) key default))

(define/contract (session-age sess)
  (-> session? exact-nonnegative-integer?)
  (- (current-seconds) (session-created-at sess)))

(define (session->string sess)
  (format "Session(~a, age=~as, auth=~a)" 
          (session-id sess)
          (session-age sess)
          (session-authenticated? sess)))

;; ===== CogServer =====
;; Demonstrates: Composition, encapsulation, command pattern

(struct cogserver (name registry sessions)
  #:transparent
  #:mutable
  #:guard (lambda (name registry sessions sname)
            (values name registry sessions)))

(define (make-cogserver [name "OpenCog"])
  (cogserver name (make-command-registry) (make-hash)))

(define/contract (cogserver-register-command! server cmd-name handler [desc ""])
  (->* (cogserver? string? procedure?) (string?) void?)
  (registry-register! (cogserver-registry server) cmd-name handler desc))

(define/contract (cogserver-execute server cmd-name args)
  (-> cogserver? string? (listof string?) command-result?)
  (registry-execute (cogserver-registry server) cmd-name args))

(define/contract (cogserver-list-commands server)
  (-> cogserver? (listof (list/c string? string?)))
  (registry-list-commands (cogserver-registry server)))

;; Session operations
(define/contract (cogserver-create-session! server session-id)
  (-> cogserver? string? session?)
  (define sess (make-session session-id))
  (hash-set! (cogserver-sessions server) session-id sess)
  sess)

(define/contract (cogserver-get-session server session-id)
  (-> cogserver? string? (or/c session? #f))
  (hash-ref (cogserver-sessions server) session-id #f))

(define/contract (cogserver-remove-session! server session-id)
  (-> cogserver? string? void?)
  (hash-remove! (cogserver-sessions server) session-id))

(define/contract (cogserver-session-count server)
  (-> cogserver? exact-nonnegative-integer?)
  (hash-count (cogserver-sessions server)))

;; ===== Built-in Commands =====
;; Demonstrates: Closures capturing server state

(define (register-builtin-commands! server)
  ;; Help command
  (cogserver-register-command! 
   server "help"
   (lambda (args)
     (define cmds (cogserver-list-commands server))
     (define output 
       (string-append
        "Available commands:\n"
        (string-join
         (for/list ([cmd cmds])
           (match cmd
             [(list name desc)
              (format "  ~a - ~a" name desc)]))
         "\n")))
     (make-success output))
   "Display available commands")
  
  ;; Info command
  (cogserver-register-command! 
   server "info"
   (lambda (args)
     (define info-str
       (string-append
        (format "Server: ~a\n" (cogserver-name server))
        (format "Commands: ~a\n" (length (cogserver-list-commands server)))
        (format "Sessions: ~a\n" (cogserver-session-count server))))
     (make-success info-str))
   "Display server information")
  
  ;; Echo command
  (cogserver-register-command! 
   server "echo"
   (lambda (args)
     (make-success (string-join args " ")))
   "Echo back the arguments")
  
  ;; Version command
  (cogserver-register-command! 
   server "version"
   (lambda (args)
     (make-success "OpenCog Racket Implementation v1.0.0"))
   "Display version information")
  
  ;; Status command
  (cogserver-register-command! 
   server "status"
   (lambda (args)
     (define uptime (current-seconds))
     (make-success 
      (format "Status: Running\nUptime: ~a seconds\nSessions: ~a" 
              uptime
              (cogserver-session-count server))))
   "Display server status")
  
  ;; List command
  (cogserver-register-command! 
   server "list"
   (lambda (args)
     (define cmds (cogserver-list-commands server))
     (make-success 
      (string-join (map first cmds) ", ")))
   "List all commands"))

;; ===== Interactive Shell =====
;; Demonstrates: REPL implementation, I/O handling

(define/contract (parse-command-line line)
  (-> string? (values string? (listof string?)))
  (define parts (string-split (string-trim line)))
  (if (empty? parts)
      (values "" '())
      (values (first parts) (rest parts))))

(define (display-prompt)
  (display "opencog> ")
  (flush-output))

(define/contract (run-interactive-shell server)
  (-> cogserver? void?)
  (displayln "OpenCog CogServer - Interactive Shell")
  (displayln "Type 'help' for available commands, 'exit' to quit")
  (newline)
  
  (let loop ()
    (display-prompt)
    (define line (read-line))
    
    (cond
      [(eof-object? line)
       (displayln "\nGoodbye!")]
      
      [(string=? (string-trim line) "")
       (loop)]
      
      [(string=? (string-trim line) "exit")
       (displayln "Goodbye!")]
      
      [(string=? (string-trim line) "quit")
       (displayln "Goodbye!")]
      
      [else
       (define-values (cmd args) (parse-command-line line))
       (define result (cogserver-execute server cmd args))
       (displayln (result->string result))
       (unless (and (command-result-success result)
                   (or (string=? cmd "exit") (string=? cmd "quit")))
         (loop))])))

;; ===== Pattern Matching Command Parser =====
;; Demonstrates: Sophisticated pattern matching

(define/contract (parse-command-with-patterns line)
  (-> string? (or/c (list/c 'help) 
                    (list/c 'info)
                    (list/c 'echo (listof string?))
                    (list/c 'unknown string?)))
  (define trimmed (string-trim line))
  (match trimmed
    ["help" '(help)]
    ["info" '(info)]
    [(regexp #rx"^echo (.+)$" (list _ text))
     (list 'echo (string-split text))]
    [else (list 'unknown trimmed)]))

;; ===== Demonstration =====

(define (demonstrate-cogserver)
  (displayln (make-string 70 #\=))
  (displayln "OpenCog CogServer - Network Server in Racket")
  (displayln "Showcasing: Command pattern, REPL, pattern matching, contracts")
  (displayln (make-string 70 #\=))
  (newline)
  
  ;; Create server
  (displayln "1. Creating CogServer")
  (displayln (make-string 50 #\-))
  (define server (make-cogserver "TestServer"))
  (displayln "Server created")
  (newline)
  
  ;; Register built-in commands
  (displayln "2. Registering Built-in Commands")
  (displayln (make-string 50 #\-))
  (register-builtin-commands! server)
  (printf "Registered ~a commands\n" 
          (length (cogserver-list-commands server)))
  (newline)
  
  ;; List commands
  (displayln "3. Available Commands")
  (displayln (make-string 50 #\-))
  (for ([cmd (cogserver-list-commands server)])
    (match cmd
      [(list name desc)
       (printf "  ~a - ~a\n" name desc)]))
  (newline)
  
  ;; Execute commands
  (displayln "4. Executing Commands")
  (displayln (make-string 50 #\-))
  (define commands-to-test
    '(("version" ())
      ("echo" ("Hello" "World"))
      ("info" ())
      ("invalid" ())))
  
  (for ([cmd commands-to-test])
    (match cmd
      [(list name args)
       (printf "Executing: ~a ~a\n" name (string-join args " "))
       (define result (cogserver-execute server name args))
       (printf "  Result: ~a\n" 
               (if (command-result-success result) "SUCCESS" "FAILURE"))
       (printf "  Message: ~a\n" (result->string result))
       (newline)]))
  
  ;; Session management
  (displayln "5. Session Management")
  (displayln (make-string 50 #\-))
  (define sess1 (cogserver-create-session! server "session-001"))
  (define sess2 (cogserver-create-session! server "session-002"))
  
  (session-set! sess1 "username" "alice")
  (session-set! sess1 "language" "racket")
  (session-set! sess2 "username" "bob")
  
  (printf "Created ~a sessions\n" (cogserver-session-count server))
  (printf "Session 1: ~a\n" (session->string sess1))
  (printf "  username: ~a\n" (session-get sess1 "username"))
  (printf "  language: ~a\n" (session-get sess1 "language"))
  (printf "Session 2: ~a\n" (session->string sess2))
  (printf "  username: ~a\n" (session-get sess2 "username"))
  (newline)
  
  ;; Custom command registration
  (displayln "6. Registering Custom Command")
  (displayln (make-string 50 #\-))
  (cogserver-register-command!
   server "greet"
   (lambda (args)
     (define name (if (empty? args) "World" (first args)))
     (make-success (format "Hello, ~a!" name)))
   "Greet a user")
  
  (define greet-result (cogserver-execute server "greet" '("Racket")))
  (printf "Custom command result: ~a\n" (result->string greet-result))
  (newline)
  
  ;; Pattern matching demo
  (displayln "7. Pattern Matching Command Parser")
  (displayln (make-string 50 #\-))
  (define test-inputs '("help" "info" "echo testing 123" "unknown cmd"))
  (for ([input test-inputs])
    (printf "Input: '~a'\n" input)
    (printf "  Parsed: ~a\n" (parse-command-with-patterns input)))
  (newline)
  
  ;; Status command
  (displayln "8. Server Status")
  (displayln (make-string 50 #\-))
  (define status-result (cogserver-execute server "status" '()))
  (displayln (result->string status-result))
  (newline)
  
  (displayln (make-string 70 #\=))
  (displayln "Racket CogServer strengths:")
  (displayln "  ✓ First-class functions for flexible commands")
  (displayln "  ✓ Hash tables for O(1) command lookup")
  (displayln "  ✓ Pattern matching for elegant parsing")
  (displayln "  ✓ Contracts for runtime safety")
  (displayln "  ✓ Closures for stateful commands")
  (displayln "  ✓ Built-in REPL functionality")
  (displayln "  ✓ Structured data with transparent structs")
  (displayln (make-string 70 #\=))
  (newline)
  
  ;; Interactive mode
  (displayln "Enter interactive mode? (y/n)")
  (define response (read-line))
  (when (and (not (eof-object? response))
            (string-ci=? (string-trim response) "y"))
    (run-interactive-shell server)))

;; Run demonstration
(module+ main
  (demonstrate-cogserver))
