# OpenCog Go Integration Example

This document demonstrates the complete integration of cogutil → atomspace → cogserver in Go, showing how the three core OpenCog components work together.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                      CogServer                          │
│  (Network Interface + Command Processing)               │
│                                                         │
│  ┌───────────────────────────────────────────────────┐ │
│  │              AtomSpace Module                     │ │
│  │  (Knowledge Representation + Hypergraph)          │ │
│  │                                                   │ │
│  │  ┌─────────────────────────────────────────────┐ │ │
│  │  │           CogUtil                           │ │ │
│  │  │  (Logging, Config, Utilities)               │ │ │
│  │  └─────────────────────────────────────────────┘ │ │
│  └───────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
```

## Complete Integration Example

### Directory Structure

```
examples/go-integration/
├── main.go
├── cogutil/
│   └── logger.go
├── atomspace/
│   └── atomspace.go (from Repo/opencog/atomspace/Go/)
└── cogserver/
    └── cogserver.go (from Repo/opencog/cogserver/Go/)
```

### Step 1: CogUtil - Logging and Configuration

```go
// cogutil/logger.go
package cogutil

import (
    "fmt"
    "log"
    "os"
    "sync"
    "time"
)

type LogLevel int

const (
    DEBUG LogLevel = iota
    INFO
    WARN
    ERROR
)

type Logger struct {
    level  LogLevel
    mu     sync.Mutex
    logger *log.Logger
}

func NewLogger(level LogLevel) *Logger {
    return &Logger{
        level:  level,
        logger: log.New(os.Stdout, "", 0),
    }
}

func (l *Logger) log(level LogLevel, format string, args ...interface{}) {
    if level < l.level {
        return
    }
    
    l.mu.Lock()
    defer l.mu.Unlock()
    
    levelStr := map[LogLevel]string{
        DEBUG: "DEBUG",
        INFO:  "INFO",
        WARN:  "WARN",
        ERROR: "ERROR",
    }[level]
    
    timestamp := time.Now().Format("2006-01-02 15:04:05")
    message := fmt.Sprintf(format, args...)
    l.logger.Printf("[%s] %s: %s", timestamp, levelStr, message)
}

func (l *Logger) Debug(format string, args ...interface{}) {
    l.log(DEBUG, format, args...)
}

func (l *Logger) Info(format string, args ...interface{}) {
    l.log(INFO, format, args...)
}

func (l *Logger) Warn(format string, args ...interface{}) {
    l.log(WARN, format, args...)
}

func (l *Logger) Error(format string, args ...interface{}) {
    l.log(ERROR, format, args...)
}
```

### Step 2: AtomSpace Module for CogServer

```go
// main.go - AtomSpace Module
package main

import (
    "fmt"
    "strings"
    
    "github.com/e9-o9/ro9se/atomspace"
    "github.com/e9-o9/ro9se/cogserver"
    "github.com/e9-o9/ro9se/cogutil"
)

type AtomSpaceModule struct {
    as     *atomspace.AtomSpace
    logger *cogutil.Logger
}

func NewAtomSpaceModule(logger *cogutil.Logger) *AtomSpaceModule {
    return &AtomSpaceModule{
        logger: logger,
    }
}

func (m *AtomSpaceModule) Init(server *cogserver.CogServer) error {
    m.as = atomspace.NewAtomSpace()
    m.logger.Info("AtomSpace module initialized")
    return nil
}

func (m *AtomSpaceModule) GetCommands() []cogserver.Command {
    return []cogserver.Command{
        {
            Name:        "addnode",
            Description: "Add a node: addnode <type> <name>",
            Handler:     m.handleAddNode,
        },
        {
            Name:        "addlink",
            Description: "Add a link: addlink <type> <node1> <node2>",
            Handler:     m.handleAddLink,
        },
        {
            Name:        "getnode",
            Description: "Get a node: getnode <type> <name>",
            Handler:     m.handleGetNode,
        },
        {
            Name:        "size",
            Description: "Get AtomSpace size",
            Handler:     m.handleSize,
        },
        {
            Name:        "list",
            Description: "List all atoms",
            Handler:     m.handleList,
        },
        {
            Name:        "settv",
            Description: "Set truth value: settv <type> <name> <strength> <confidence>",
            Handler:     m.handleSetTV,
        },
        {
            Name:        "gettv",
            Description: "Get truth value: gettv <type> <name>",
            Handler:     m.handleGetTV,
        },
    }
}

func (m *AtomSpaceModule) handleAddNode(args []string) (string, error) {
    if len(args) < 2 {
        return "", fmt.Errorf("usage: addnode <type> <name>")
    }
    
    atomType := atomspace.AtomType(args[0])
    name := strings.Join(args[1:], " ")
    
    node := m.as.AddNode(atomType, name)
    m.logger.Info("Added node: %s", node.String())
    
    return fmt.Sprintf("Added: %s [handle=%d]", node.String(), node.GetHandle()), nil
}

func (m *AtomSpaceModule) handleAddLink(args []string) (string, error) {
    if len(args) < 3 {
        return "", fmt.Errorf("usage: addlink <type> <node1> <node2>")
    }
    
    linkType := atomspace.AtomType(args[0])
    
    // Get or create nodes
    node1 := m.as.AddNode(atomspace.ConceptNode, args[1])
    node2 := m.as.AddNode(atomspace.ConceptNode, args[2])
    
    link := m.as.AddLink(linkType, []atomspace.Atom{node1, node2})
    m.logger.Info("Added link: %s", link.String())
    
    return fmt.Sprintf("Added: %s [handle=%d]", link.String(), link.GetHandle()), nil
}

func (m *AtomSpaceModule) handleGetNode(args []string) (string, error) {
    if len(args) < 2 {
        return "", fmt.Errorf("usage: getnode <type> <name>")
    }
    
    atomType := atomspace.AtomType(args[0])
    name := strings.Join(args[1:], " ")
    
    node := m.as.GetNode(atomType, name)
    if node == nil {
        return "Node not found", nil
    }
    
    tv := node.GetTruthValue()
    return fmt.Sprintf("%s [handle=%d, tv=(%.2f, %.2f)]", 
        node.String(), node.GetHandle(), tv.Strength, tv.Confidence), nil
}

func (m *AtomSpaceModule) handleSize(args []string) (string, error) {
    size := m.as.Size()
    m.logger.Debug("AtomSpace size queried: %d", size)
    return fmt.Sprintf("AtomSpace contains %d atoms", size), nil
}

func (m *AtomSpaceModule) handleList(args []string) (string, error) {
    atoms := m.as.GetAllAtoms()
    
    if len(atoms) == 0 {
        return "AtomSpace is empty", nil
    }
    
    var sb strings.Builder
    sb.WriteString(fmt.Sprintf("AtomSpace contains %d atoms:\n", len(atoms)))
    
    for _, atom := range atoms {
        tv := atom.GetTruthValue()
        sb.WriteString(fmt.Sprintf("  [%d] %s (tv=%.2f/%.2f)\n", 
            atom.GetHandle(), atom.String(), tv.Strength, tv.Confidence))
    }
    
    return sb.String(), nil
}

func (m *AtomSpaceModule) handleSetTV(args []string) (string, error) {
    if len(args) < 4 {
        return "", fmt.Errorf("usage: settv <type> <name> <strength> <confidence>")
    }
    
    atomType := atomspace.AtomType(args[0])
    name := args[1]
    
    var strength, confidence float64
    fmt.Sscanf(args[2], "%f", &strength)
    fmt.Sscanf(args[3], "%f", &confidence)
    
    node := m.as.GetNode(atomType, name)
    if node == nil {
        return "Node not found", nil
    }
    
    tv := atomspace.NewTruthValue(strength, confidence)
    node.SetTruthValue(tv)
    
    m.logger.Info("Set truth value for %s: (%.2f, %.2f)", node.String(), strength, confidence)
    return fmt.Sprintf("Truth value set: %s (%.2f, %.2f)", node.String(), strength, confidence), nil
}

func (m *AtomSpaceModule) handleGetTV(args []string) (string, error) {
    if len(args) < 2 {
        return "", fmt.Errorf("usage: gettv <type> <name>")
    }
    
    atomType := atomspace.AtomType(args[0])
    name := args[1]
    
    node := m.as.GetNode(atomType, name)
    if node == nil {
        return "Node not found", nil
    }
    
    tv := node.GetTruthValue()
    return fmt.Sprintf("%s: strength=%.2f, confidence=%.2f", 
        node.String(), tv.Strength, tv.Confidence), nil
}

func (m *AtomSpaceModule) Shutdown() error {
    m.logger.Info("AtomSpace module shutting down, clearing %d atoms", m.as.Size())
    m.as.Clear()
    return nil
}
```

### Step 3: Main Application

```go
// main.go - Main Application
package main

import (
    "fmt"
    "log"
    "os"
    "os/signal"
    "syscall"
    
    "github.com/e9-o9/ro9se/cogserver"
    "github.com/e9-o9/ro9se/cogutil"
)

func main() {
    // Step 1: Initialize CogUtil (logging)
    logger := cogutil.NewLogger(cogutil.INFO)
    logger.Info("Starting OpenCog Go Integration Example")
    
    // Step 2: Create CogServer
    server := cogserver.NewCogServer("localhost:17001")
    logger.Info("CogServer created on localhost:17001")
    
    // Step 3: Load AtomSpace module
    atomspaceModule := NewAtomSpaceModule(logger)
    if err := server.LoadModule("atomspace", atomspaceModule); err != nil {
        log.Fatal(err)
    }
    logger.Info("AtomSpace module loaded")
    
    // Step 4: Register additional custom commands
    server.RegisterCommand(cogserver.Command{
        Name:        "demo",
        Description: "Run a demo of AtomSpace operations",
        Handler:     createDemoHandler(atomspaceModule, logger),
    })
    
    // Step 5: Start server
    if err := server.Start(); err != nil {
        log.Fatal(err)
    }
    logger.Info("CogServer started successfully")
    
    fmt.Println("\n=== OpenCog Go Integration Demo ===")
    fmt.Println("Connect with: telnet localhost 17001")
    fmt.Println("Try commands: help, demo, addnode, addlink, size, list")
    fmt.Println("Press Ctrl+C to shutdown\n")
    
    // Wait for interrupt signal
    sigChan := make(chan os.Signal, 1)
    signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)
    <-sigChan
    
    logger.Info("Shutdown signal received")
    fmt.Println("\nShutting down...")
    server.Stop()
    logger.Info("Server stopped successfully")
}

func createDemoHandler(module *AtomSpaceModule, logger *cogutil.Logger) cogserver.CommandHandler {
    return func(args []string) (string, error) {
        logger.Info("Running demo...")
        
        // Create a simple knowledge base
        module.as.Clear()
        
        // Add concepts
        cat := module.as.AddNode(atomspace.ConceptNode, "cat")
        dog := module.as.AddNode(atomspace.ConceptNode, "dog")
        mammal := module.as.AddNode(atomspace.ConceptNode, "mammal")
        animal := module.as.AddNode(atomspace.ConceptNode, "animal")
        
        // Add inheritance links
        module.as.AddLink(atomspace.InheritanceLink, []atomspace.Atom{cat, mammal})
        module.as.AddLink(atomspace.InheritanceLink, []atomspace.Atom{dog, mammal})
        module.as.AddLink(atomspace.InheritanceLink, []atomspace.Atom{mammal, animal})
        
        // Set truth values
        cat.SetTruthValue(atomspace.NewTruthValue(1.0, 0.9))
        dog.SetTruthValue(atomspace.NewTruthValue(1.0, 0.9))
        
        logger.Info("Demo completed: created %d atoms", module.as.Size())
        
        return fmt.Sprintf("Demo complete! Created knowledge base with %d atoms.\n" +
            "Try: list, getnode ConceptNode cat, gettv ConceptNode cat", 
            module.as.Size()), nil
    }
}
```

## Running the Example

### Build and Run

```bash
# Build
go build -o opencog-demo main.go

# Run
./opencog-demo
```

### Example Session

```
$ telnet localhost 17001
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Welcome to CogServer
Type 'help' for available commands
cogserver> help
Available commands:
  addlink         Add a link: addlink <type> <node1> <node2>
  addnode         Add a node: addnode <type> <name>
  demo            Run a demo of AtomSpace operations
  getnode         Get a node: getnode <type> <name>
  gettv           Get truth value: gettv <type> <name>
  help            Display available commands
  list            List all atoms
  settv           Set truth value: settv <type> <name> <strength> <confidence>
  shutdown        Shutdown the server
  size            Get AtomSpace size
cogserver> demo
Demo complete! Created knowledge base with 7 atoms.
Try: list, getnode ConceptNode cat, gettv ConceptNode cat
cogserver> size
AtomSpace contains 7 atoms
cogserver> list
AtomSpace contains 7 atoms:
  [1] (ConceptNode "cat") (tv=1.00/0.90)
  [2] (ConceptNode "dog") (tv=1.00/0.90)
  [3] (ConceptNode "mammal") (tv=1.00/1.00)
  [4] (ConceptNode "animal") (tv=1.00/1.00)
  [5] (InheritanceLink (ConceptNode "cat") (ConceptNode "mammal")) (tv=1.00/1.00)
  [6] (InheritanceLink (ConceptNode "dog") (ConceptNode "mammal")) (tv=1.00/1.00)
  [7] (InheritanceLink (ConceptNode "mammal") (ConceptNode "animal")) (tv=1.00/1.00)
cogserver> getnode ConceptNode cat
(ConceptNode "cat") [handle=1, tv=(1.00, 0.90)]
cogserver> addnode PredicateNode likes
Added: (PredicateNode "likes") [handle=8]
cogserver> addlink EvaluationLink likes cat
Added: (EvaluationLink (ConceptNode "likes") (ConceptNode "cat")) [handle=9]
cogserver> size
AtomSpace contains 9 atoms
cogserver> shutdown
Shutting down server...
Connection closed by foreign host.
```

## Integration Points

### 1. CogUtil → AtomSpace
- Logger used throughout AtomSpace operations
- Configuration for AtomSpace parameters
- Utilities for concurrent access

### 2. AtomSpace → CogServer
- AtomSpace wrapped in a CogServer module
- Commands provide network interface to AtomSpace
- Module lifecycle managed by CogServer

### 3. Complete Pipeline
```
User → TCP Connection → CogServer → Command Handler → AtomSpace Module → AtomSpace → CogUtil
```

## Benefits of Go Implementation

1. **Concurrency**: Goroutines enable efficient multi-client handling
2. **Type Safety**: Strong typing catches errors at compile time
3. **Performance**: Near C++ performance with simpler code
4. **Deployment**: Single binary, no dependencies
5. **Cross-Platform**: Runs on Linux, macOS, Windows

## Next Steps

1. Add pattern matching to AtomSpace
2. Implement persistence (RocksDB backend)
3. Add gRPC interface for cross-language communication
4. Implement attention allocation
5. Add probabilistic reasoning (PLN)

## See Also

- [AtomSpace Go README](../atomspace/Go/README.md)
- [CogServer Go README](../cogserver/Go/README.md)
- [OpenCog Integration Design](OPENCOG_INTEGRATION_DESIGN.md)
