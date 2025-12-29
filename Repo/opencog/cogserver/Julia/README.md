# OpenCog CogServer - Julia Implementation

## Overview

This is a single-file Julia implementation of the OpenCog CogServer network server and REPL interface.

## Features

- **TCP Network Server**: Accept multiple client connections
- **Command Registry**: Extensible command system
- **Module System**: Load/unload modules dynamically
- **REPL Interface**: Interactive command-line interface
- **Async I/O**: Concurrent client handling with tasks

## Julia Strengths Demonstrated

- **Asynchronous Programming**: Task-based concurrency with `@async`
- **Higher-Order Functions**: Commands as first-class functions
- **Networking**: TCP sockets with the Sockets module
- **Module System**: Dynamic module loading and initialization
- **Closures**: Command handlers capture server state

## Usage

### Running the Demo

```bash
julia cogserver.jl
```

Then connect from another terminal:
```bash
telnet 127.0.0.1 17001
# or
nc 127.0.0.1 17001
```

### As a Library

```julia
include("cogserver.jl")

# Create server
server = CogServer("127.0.0.1", 17001)

# Register custom command
register_command!(server, Command(
    "greet",
    "Greet the user",
    args -> ("Hello, $(join(args, " "))!", true)
))

# Start server (blocks until stopped)
start!(server)
```

## Built-in Commands

- `help` - Display available commands
- `list` - List loaded modules
- `info` - Display server information
- `shutdown` - Stop the server
- `echo <message>` - Echo back the message

## Core Types

### Command
Struct representing a registered command:
- `name::String` - Command name
- `description::String` - Help text
- `handler::Function` - Handler function `(args) -> (result, success)`

### CogModule
Abstract type for loadable modules. Implement:
- `module_init(m, server)` - Initialize module
- `module_commands(m)` - Return vector of commands
- `module_shutdown(m)` - Clean up resources

### CogServer
Main server structure:
- `address::String` - Bind address
- `port::Int` - Listen port
- `commands::Dict{String, Command}` - Registered commands
- `modules::Dict{String, CogModule}` - Loaded modules

## API Reference

### Server Operations
- `start!(server)` - Start server (blocking)
- `stop!(server)` - Stop server
- `cleanup!(server)` - Clean up resources

### Command Management
- `register_command!(server, cmd)` - Register command
- `unregister_command!(server, name)` - Unregister command
- `execute_command(server, line)` - Execute command string

### Module Management
- `load_module!(server, name, module)` - Load module
- `unload_module!(server, name)` - Unload module

## Creating Custom Modules

```julia
# Define module type
mutable struct MyModule <: CogModule
    data::Dict{String, Any}
    
    MyModule() = new(Dict{String, Any}())
end

# Implement interface
function module_init(m::MyModule, server)
    println("MyModule initialized")
    m.data["server"] = server
end

function module_commands(m::MyModule)
    return [
        Command(
            "mycommand",
            "My custom command",
            args -> ("MyModule response: $(join(args, " "))", true)
        )
    ]
end

function module_shutdown(m::MyModule)
    println("MyModule shutting down")
    empty!(m.data)
end

# Load into server
success, msg = load_module!(server, "mymodule", MyModule())
println(msg)
```

## Architecture

```
CogServer
├── Network Layer (TCP Sockets)
│   ├── Accept connections
│   ├── Handle clients (async tasks)
│   └── REPL protocol
├── Command Registry
│   ├── Built-in commands
│   └── Module commands
└── Module System
    ├── Load/unload modules
    └── Module lifecycle management

Client Connection Flow:
1. Accept TCP connection
2. Send welcome message
3. REPL loop:
   - Read command
   - Execute command
   - Print response
   - Repeat
4. Close on disconnect/shutdown
```

## Example Session

```
$ julia cogserver.jl
[CogServer] Listening on 127.0.0.1:17001

# In another terminal:
$ telnet 127.0.0.1 17001
Welcome to CogServer (Julia)
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
  Running: true
  Commands: 5
  Modules: 0
  Active Clients: 1

cogserver> echo Hello OpenCog!
Hello OpenCog!

cogserver> shutdown
Shutting down server...
Server shutting down...
Connection closed by foreign host.
```

## Performance Characteristics

- **Async I/O**: Non-blocking client handling
- **Task-Based**: Julia's lightweight tasks for concurrency
- **Lock Protection**: Thread-safe command and module access
- **Graceful Shutdown**: Clean resource cleanup

## Requirements

- Julia 1.6 or later
- Standard library only (Sockets, Dates)

## Network Protocol

Simple text-based REPL protocol:
1. Server sends welcome message
2. Server sends prompt: `cogserver> `
3. Client sends command line
4. Server sends response
5. Server sends prompt
6. Repeat until disconnect or shutdown

## Thread Safety

All shared state (commands, modules) is protected by locks:
- `server.lock` - ReentrantLock for all state access
- Safe for concurrent client connections
- Module operations are atomic

## License

Part of the RosettaCog project - see main repository for license details.
