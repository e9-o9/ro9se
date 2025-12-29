# OpenCog CogServer - Scheme Implementation

## Overview

This is a single-file Scheme implementation of the OpenCog CogServer network server and REPL interface.

## Features

- **REPL Interface**: Read-Eval-Print Loop for interactive commands
- **Command Registry**: Extensible command system with first-class functions
- **Module System**: Load/unload modules dynamically
- **S-Expression Based**: Natural Scheme representation
- **Functional Design**: Closures for state encapsulation

## Scheme Strengths Demonstrated

- **S-Expressions**: Commands and responses as data structures
- **First-Class Functions**: Command handlers are functions
- **Closures**: Capture server state in handler functions
- **Homoiconicity**: Code is data, enabling metaprogramming
- **Functional Programming**: Immutable data where possible
- **Record Types**: Structured data with type checking

## Usage

### Running the Demo

```bash
# With Racket
racket cogserver.scm

# With Guile
guile cogserver.scm

# With Chez Scheme
scheme --script cogserver.scm
```

### As a Library

```scheme
(load "cogserver.scm")

;; Create server
(define server (make-cogserver "127.0.0.1" 17001))

;; Register custom command
(register-command! server
  (make-command "greet" "Greet the user"
    (lambda (args)
      (string-append "Hello, " (string-join args " ") "!"))))

;; Start server (blocks in REPL mode)
(cogserver-start! server)
```

## Built-in Commands

- `help` - Display available commands
- `list` - List loaded modules
- `info` - Display server information
- `shutdown` - Stop the server
- `echo <message>` - Echo back the message

## Core Types

### Command
Record type representing a registered command:
```scheme
(define-record-type command
  (make-command name description handler)
  command?
  (name command-name)
  (description command-description)
  (handler command-handler))
```

### CogModule
Record type for loadable modules:
```scheme
(define-record-type cog-module
  (make-module name init-proc get-commands-proc shutdown-proc)
  cog-module?
  ...)
```

### CogServer
Record type for the main server:
```scheme
(define-record-type cogserver
  (make-cogserver address port commands modules running?)
  cogserver?
  ...)
```

## API Reference

### Server Operations
- `(make-cogserver address port)` - Create server
- `(cogserver-start! server)` - Start server (blocking REPL)
- `(cogserver-stop! server)` - Stop server

### Command Management
- `(register-command! server cmd)` - Register command
- `(unregister-command! server name)` - Unregister command
- `(execute-command server line)` - Execute command string

### Module Management
- `(load-module! server name module)` - Load module, returns `(success . message)`
- `(unload-module! server name)` - Unload module, returns `(success . message)`

## Creating Custom Commands

```scheme
;; Simple command
(define greet-cmd
  (make-command
    "greet"                           ; Name
    "Greet the user"                  ; Description
    (lambda (args)                    ; Handler
      (if (null? args)
          "Hello!"
          (string-append "Hello, " (string-join args " ") "!")))))

;; Register
(register-command! server greet-cmd)

;; Command with server access (using closure)
(register-command! server
  (make-command "status" "Show status"
    (lambda (args)
      (format #f "Server running: ~a" 
              (cogserver-running? server)))))
```

## Creating Custom Modules

```scheme
;; Define module
(define (make-my-module)
  (let ((data '()))  ; Module state
    (make-module
      "mymodule"
      
      ;; Init procedure
      (lambda (server)
        (display "MyModule initialized\n")
        (set! data (list server)))
      
      ;; Get commands procedure
      (lambda ()
        (list
          (make-command
            "mycommand"
            "My custom command"
            (lambda (args)
              (string-append "MyModule response: " 
                           (string-join args " "))))))
      
      ;; Shutdown procedure
      (lambda ()
        (display "MyModule shutting down\n")
        (set! data '())))))

;; Load into server
(let ((result (load-module! server "mymodule" (make-my-module))))
  (if (car result)
      (display (cdr result))
      (error (cdr result))))
```

## Architecture

```
CogServer (record)
├── address: string
├── port: integer
├── commands: hash-table (name -> command)
├── modules: hash-table (name -> module)
└── running?: boolean

Command (record)
├── name: string
├── description: string
└── handler: function (args -> string)

CogModule (record)
├── name: string
├── init-proc: function (server -> void)
├── get-commands-proc: function (-> list of commands)
└── shutdown-proc: function (-> void)

REPL Flow:
1. Display prompt
2. Read command line
3. Parse into command name and args
4. Look up command in registry
5. Execute command handler
6. Display result
7. Repeat
```

## Example Session

```
$ racket cogserver.scm

cogserver> help
Available commands:
  echo - Echo back arguments
  help - Display available commands
  info - Display server information
  list - List loaded modules
  shutdown - Shutdown the server

cogserver> info
CogServer Information:
  Address: 127.0.0.1:17001
  Running: true
  Commands: 5
  Modules: 0

cogserver> echo Hello OpenCog!
Hello OpenCog!

cogserver> shutdown
Shutting down server...
```

## S-Expression Philosophy

### Code as Data
```scheme
;; Commands are data structures
(make-command "echo" "Echo" 
  (lambda (args) (string-join args " ")))

;; Can be manipulated programmatically
(map command-name (module-get-commands my-module))
```

### First-Class Functions
```scheme
;; Functions are values
(define my-handler 
  (lambda (args) "Result"))

;; Can be passed around
(make-command "test" "Test" my-handler)
```

### Closures Capture Environment
```scheme
;; Handler captures server
(let ((server-ref server))
  (lambda (args)
    (cogserver-stop! server-ref)
    "Stopped"))
```

## Functional Programming Patterns

### Pure Functions
```scheme
;; No side effects (where possible)
(define (execute-command server line)
  (let ((parts (string-split line #\space)))
    ...))
```

### Higher-Order Functions
```scheme
;; Functions that operate on functions
(define (register-command! server cmd)
  (hash-table-set! (cogserver-commands server)
                   (command-name cmd)
                   cmd))
```

### Immutability
```scheme
;; Prefer immutable updates
(define new-list (cons item old-list))
```

## Performance Characteristics

- **Tail Call Optimization**: Recursive REPL doesn't grow stack
- **Hash Tables**: Fast command lookup
- **Garbage Collection**: Automatic memory management
- **Functional Style**: Enables optimization opportunities

## Requirements

Compatible with most Scheme implementations:
- **Racket**: Full R6RS/R7RS support
- **Guile**: GNU's Scheme implementation
- **Chez Scheme**: Fast native code compiler
- **MIT Scheme**: Classic implementation

## Compatibility Notes

This implementation uses:
- `define-record-type` from SRFI-9
- Basic hash tables (custom implementation provided)
- Standard I/O operations

## REPL Implementation

### Read
```scheme
(read-line (current-input-port))
```

### Eval
```scheme
(execute-command server line)
```

### Print
```scheme
(display response)
(newline)
```

### Loop
```scheme
(when (cogserver-running? server)
  (repl-loop server))
```

## Extensibility

### Dynamic Command Registration
```scheme
;; Add commands at runtime
(register-command! server new-command)

;; Remove commands
(unregister-command! server "old-command")
```

### Module System
```scheme
;; Load functionality dynamically
(load-module! server "plugin" plugin-module)

;; Unload when done
(unload-module! server "plugin")
```

### Metaprogramming
```scheme
;; Generate commands programmatically
(define (make-echo-variants)
  (map (lambda (name)
         (make-command name 
                      (format #f "Echo ~a" name)
                      echo-handler))
       '("echo1" "echo2" "echo3")))
```

## Philosophy

This implementation embodies Scheme's philosophy:
- **Simplicity**: Minimal, elegant design
- **Composability**: Build complex from simple
- **Flexibility**: Easy to extend and modify
- **Power**: Homoiconicity enables metaprogramming

## License

Part of the RosettaCog project - see main repository for license details.
