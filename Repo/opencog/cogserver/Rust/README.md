# OpenCog CogServer - Rust Implementation

## Overview

This is a single-file Rust implementation of the OpenCog CogServer network server and REPL interface.

## Features

- **TCP Network Server**: Accept multiple client connections
- **Command Registry**: Extensible command system with closures
- **Module System**: Load/unload modules dynamically
- **REPL Interface**: Interactive command-line interface
- **Thread-Safe**: Concurrent client handling with Arc and Mutex

## Rust Strengths Demonstrated

- **Safe Concurrency**: Threads with Arc/Mutex for shared state
- **Ownership System**: Automatic resource cleanup
- **Error Handling**: Result type for explicit error propagation
- **Pattern Matching**: Exhaustive command dispatch
- **Zero-Cost Abstractions**: Closures compile to direct calls
- **Type Safety**: Compile-time guarantees

## Usage

### Compiling and Running

```bash
# Compile
rustc cogserver.rs

# Run demo
./cogserver
```

Then connect from another terminal:
```bash
telnet 127.0.0.1 17001
# or
nc 127.0.0.1 17001
```

### As a Library

```rust
use cogserver::{CogServer, Command};

// Create server
let server = CogServer::new("127.0.0.1:17001");

// Register custom command
server.register_command(Command::new(
    "greet",
    "Greet the user",
    |args| Ok(format!("Hello, {}!", args.join(" ")))
));

// Start server (blocks)
server.start().expect("Failed to start server");
```

## Built-in Commands

- `help` - Display available commands
- `list` - List loaded modules
- `info` - Display server information
- `shutdown` - Stop the server
- `echo <message>` - Echo back the message

## Core Types

### Command Struct
Represents a registered command:
- `name: String` - Command name
- `description: String` - Help text
- `handler: CommandHandler` - Closure that handles execution

### CommandHandler Type
```rust
type CommandHandler = Arc<dyn Fn(&[String]) -> Result<String, String> + Send + Sync>;
```

### CogModule Trait
Interface for loadable modules:
- `init(&mut self, server)` - Initialize module
- `get_commands(&self)` - Return vector of commands
- `shutdown(&mut self)` - Clean up resources

### CogServer Struct
Main server structure:
- `address: String` - Bind address
- `commands: Arc<Mutex<HashMap<String, Command>>>` - Registered commands
- `modules: Arc<Mutex<HashMap<String, Box<dyn CogModule>>>>` - Loaded modules
- `running: Arc<Mutex<bool>>` - Server state

## API Reference

### Server Operations
- `new(address)` - Create server
- `start()` - Start server (blocking)
- `stop()` - Stop server

### Command Management
- `register_command(cmd)` - Register command
- `unregister_command(name)` - Unregister command

### Module Management
- `load_module(name, module)` - Load module
- `unload_module(name)` - Unload module

## Creating Custom Modules

```rust
struct MyModule {
    data: HashMap<String, String>,
}

impl MyModule {
    fn new() -> Self {
        MyModule {
            data: HashMap::new(),
        }
    }
}

impl CogModule for MyModule {
    fn init(&mut self, server: &CogServer) -> Result<(), String> {
        println!("MyModule initialized");
        Ok(())
    }

    fn get_commands(&self) -> Vec<Command> {
        vec![
            Command::new(
                "mycommand",
                "My custom command",
                |args| Ok(format!("MyModule response: {}", args.join(" ")))
            )
        ]
    }

    fn shutdown(&mut self) -> Result<(), String> {
        println!("MyModule shutting down");
        self.data.clear();
        Ok(())
    }
}

// Load into server
server.load_module("mymodule", Box::new(MyModule::new()))
    .expect("Failed to load module");
```

## Architecture

```
CogServer
├── Network Layer (TcpListener)
│   ├── Accept connections
│   ├── Spawn thread per client
│   └── REPL protocol
├── Command Registry (Arc<Mutex<HashMap>>)
│   ├── Built-in commands
│   └── Module commands
└── Module System
    ├── Load/unload modules
    └── Module lifecycle management

Concurrency Model:
- Main thread: Accept connections
- Client threads: Handle REPL per connection
- Arc<Mutex<T>>: Shared state across threads
```

## Example Session

```
$ ./cogserver
[CogServer] Listening on 127.0.0.1:17001

# In another terminal:
$ telnet 127.0.0.1 17001
Welcome to CogServer (Rust)
Type 'help' for available commands
cogserver> help
Available commands:
  echo            - Echo back arguments
  help            - Display available commands
  info            - Display server information
  list            - List loaded modules
  shutdown        - Shutdown the server

cogserver> info
CogServer Information:
Address: 127.0.0.1:17001
Commands: 5
Modules: 0

cogserver> echo Hello OpenCog!
Hello OpenCog!

cogserver> shutdown
Shutting down server...
Server shutting down...
Connection closed by foreign host.
```

## Thread Safety

### Shared State Protection
- `Arc<Mutex<HashMap>>` - Atomic reference counting with mutex
- Lock granularity: Per-operation (fine-grained)
- Deadlock prevention: Single lock per operation

### Ownership Model
```rust
// Clone Arc for thread
let commands_clone = Arc::clone(&self.commands);

thread::spawn(move || {
    // Thread owns commands_clone
    let cmds = commands_clone.lock().unwrap();
    // Use cmds...
}); // Lock released automatically (RAII)
```

## Error Handling

Uses `Result<T, E>` for explicit error handling:
```rust
pub fn start(&self) -> Result<(), String> {
    // Returns Ok(()) on success or Err(message) on failure
}
```

Advantages:
- Explicit error propagation
- No exceptions or panics
- Compiler ensures errors are handled
- `?` operator for clean error forwarding

## Performance Characteristics

- **Thread Pool**: One thread per client connection
- **Lock Contention**: Minimal (commands accessed briefly)
- **Memory Overhead**: Arc adds one pointer per reference
- **Network I/O**: Blocking I/O per thread (simple but effective)
- **Zero Runtime**: No GC pauses, predictable latency

## Requirements

- Rust 1.50 or later
- No external dependencies (uses only std library)

## Network Protocol

Simple text-based REPL protocol:
1. Server sends welcome message
2. Server sends prompt: `cogserver> `
3. Client sends command line
4. Server sends response
5. Server sends prompt
6. Repeat until disconnect or shutdown

## Safety Guarantees

- **No Data Races**: Mutex ensures exclusive access
- **No Use-After-Free**: Ownership prevents dangling references
- **No Memory Leaks**: Automatic cleanup with RAII
- **Thread Safety**: Arc + Mutex pattern is provably safe
- **No Undefined Behavior**: Rust's safety guarantees

## Concurrency Model

### Arc (Atomic Reference Counting)
- Thread-safe reference counting
- Cheap cloning (increments counter)
- Automatic cleanup when count reaches zero

### Mutex (Mutual Exclusion)
- Ensures only one thread accesses data
- Lock guard uses RAII (auto-release)
- Poisoning on panic (detects corruption)

### Thread Spawning
```rust
thread::spawn(move || {
    // Closure owns captured variables
    // Executes on separate thread
});
```

## Comparison with Other Implementations

| Feature | Rust | Go | Julia |
|---------|------|----|----|
| Concurrency | Threads + Arc/Mutex | Goroutines | Tasks |
| Memory Safety | Compile-time | Runtime checks | Runtime GC |
| Performance | Native code | Native code | JIT compiled |
| Error Handling | Result type | Multiple returns | Exceptions |
| Type Safety | Strong static | Strong static | Optional dynamic |

## Advanced Patterns

### Command Closures
```rust
// Capture server state in closure
let running_clone = Arc::clone(&self.running);
Command::new("shutdown", "Stop", move |_| {
    *running_clone.lock().unwrap() = false;
    Ok("Stopping...".to_string())
})
```

### Module Trait Objects
```rust
// Dynamic dispatch through trait objects
let module: Box<dyn CogModule> = Box::new(MyModule::new());
```

## License

Part of the RosettaCog project - see main repository for license details.
