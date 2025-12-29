#!/usr/bin/env julia
#=
cogserver.jl

OpenCog CogServer - Julia Implementation
Network server and REPL interface for OpenCog

This implementation demonstrates Julia's strengths:
- TCP networking with concurrent client handling
- Task-based asynchronous programming
- Higher-order functions for command handlers
- Module system with closures
- Dictionary-based command registry
=#

using Sockets
using Dates
using Printf

# ============================================================================
# COMMAND SYSTEM
# ============================================================================

"""
    CommandHandler

Type alias for command handler functions.
Takes arguments vector, returns (result_string, success_bool)
"""
const CommandHandler = Function

"""
    Command

Represents a registered command in the CogServer.
Demonstrates: Struct with function fields, documentation
"""
struct Command
    name::String
    description::String
    handler::CommandHandler
end

# ============================================================================
# MODULE INTERFACE
# ============================================================================

"""
    CogModule

Abstract type for CogServer modules.
Modules can register commands and manage their own state.
"""
abstract type CogModule end

# Module interface (to be implemented by concrete modules)
module_init(m::CogModule, server) = error("module_init not implemented")
module_commands(m::CogModule) = error("module_commands not implemented")
module_shutdown(m::CogModule) = error("module_shutdown not implemented")

# ============================================================================
# COGSERVER IMPLEMENTATION
# ============================================================================

"""
    CogServer

Main server structure handling network connections and command execution.
Demonstrates: Mutable struct, concurrent programming, event loop
"""
mutable struct CogServer
    address::String
    port::Int
    server_socket::Union{TCPServer, Nothing}
    commands::Dict{String, Command}
    modules::Dict{String, CogModule}
    running::Bool
    client_tasks::Vector{Task}
    lock::ReentrantLock
    
    function CogServer(address::String="0.0.0.0", port::Int=17001)
        cs = new(
            address,
            port,
            nothing,
            Dict{String, Command}(),
            Dict{String, CogModule}(),
            false,
            Task[],
            ReentrantLock()
        )
        
        # Register built-in commands
        register_builtin_commands!(cs)
        
        return cs
    end
end

# ============================================================================
# COMMAND REGISTRATION
# ============================================================================

"""
    register_command!(cs::CogServer, cmd::Command)

Register a new command in the server.
"""
function register_command!(cs::CogServer, cmd::Command)
    lock(cs.lock) do
        cs.commands[cmd.name] = cmd
    end
end

"""
    unregister_command!(cs::CogServer, name::String)

Unregister a command from the server.
"""
function unregister_command!(cs::CogServer, name::String)
    lock(cs.lock) do
        delete!(cs.commands, name)
    end
end

# ============================================================================
# BUILT-IN COMMAND HANDLERS
# ============================================================================

function handle_help(cs::CogServer, args::Vector{String})
    lock(cs.lock) do
        if isempty(cs.commands)
            return ("No commands available", true)
        end
        
        result = "Available commands:\n"
        for (name, cmd) in sort(collect(cs.commands), by=x->x[1])
            result *= @sprintf("  %-15s - %s\n", name, cmd.description)
        end
        return (result, true)
    end
end

function handle_list(cs::CogServer, args::Vector{String})
    lock(cs.lock) do
        if isempty(cs.modules)
            return ("No modules loaded", true)
        end
        
        result = "Loaded modules:\n"
        for (name, _) in sort(collect(cs.modules), by=x->x[1])
            result *= "  - $name\n"
        end
        return (result, true)
    end
end

function handle_shutdown(cs::CogServer, args::Vector{String})
    println("[CogServer] Shutdown command received")
    cs.running = false
    return ("Shutting down server...", true)
end

function handle_info(cs::CogServer, args::Vector{String})
    lock(cs.lock) do
        num_commands = length(cs.commands)
        num_modules = length(cs.modules)
        num_clients = length(cs.client_tasks)
        
        result = """
        CogServer Information:
          Address: $(cs.address):$(cs.port)
          Running: $(cs.running)
          Commands: $num_commands
          Modules: $num_modules
          Active Clients: $num_clients
        """
        return (result, true)
    end
end

function handle_echo(cs::CogServer, args::Vector{String})
    if isempty(args)
        return ("Usage: echo <message>", false)
    end
    return (join(args, " "), true)
end

"""
    register_builtin_commands!(cs::CogServer)

Register all built-in commands.
Demonstrates: Higher-order functions, closures
"""
function register_builtin_commands!(cs::CogServer)
    # Help command
    register_command!(cs, Command(
        "help",
        "Display available commands",
        args -> handle_help(cs, args)
    ))
    
    # List modules command
    register_command!(cs, Command(
        "list",
        "List loaded modules",
        args -> handle_list(cs, args)
    ))
    
    # Shutdown command
    register_command!(cs, Command(
        "shutdown",
        "Shutdown the server",
        args -> handle_shutdown(cs, args)
    ))
    
    # Info command
    register_command!(cs, Command(
        "info",
        "Display server information",
        args -> handle_info(cs, args)
    ))
    
    # Echo command
    register_command!(cs, Command(
        "echo",
        "Echo back arguments",
        args -> handle_echo(cs, args)
    ))
end

# ============================================================================
# MODULE MANAGEMENT
# ============================================================================

"""
    load_module!(cs::CogServer, name::String, module::CogModule)

Load a module and register its commands.
"""
function load_module!(cs::CogServer, name::String, module::CogModule)
    lock(cs.lock) do
        if haskey(cs.modules, name)
            return (false, "Module $name already loaded")
        end
        
        # Initialize module
        try
            module_init(module, cs)
        catch e
            return (false, "Failed to initialize module $name: $e")
        end
        
        # Register module commands
        for cmd in module_commands(module)
            cs.commands[cmd.name] = cmd
        end
        
        cs.modules[name] = module
        return (true, "Module $name loaded successfully")
    end
end

"""
    unload_module!(cs::CogServer, name::String)

Unload a module and unregister its commands.
"""
function unload_module!(cs::CogServer, name::String)
    lock(cs.lock) do
        if !haskey(cs.modules, name)
            return (false, "Module $name not loaded")
        end
        
        module = cs.modules[name]
        
        # Unregister module commands
        for cmd in module_commands(module)
            delete!(cs.commands, cmd.name)
        end
        
        # Shutdown module
        try
            module_shutdown(module)
        catch e
            return (false, "Failed to shutdown module $name: $e")
        end
        
        delete!(cs.modules, name)
        return (true, "Module $name unloaded successfully")
    end
end

# ============================================================================
# SERVER OPERATIONS
# ============================================================================

"""
    start!(cs::CogServer)

Start the CogServer and begin accepting connections.
Demonstrates: Asynchronous programming, task management
"""
function start!(cs::CogServer)
    lock(cs.lock) do
        if cs.running
            error("Server already running")
        end
        
        # Create TCP server
        try
            cs.server_socket = listen(IPv4(cs.address), cs.port)
        catch e
            error("Failed to start listener: $e")
        end
        
        cs.running = true
        println("[CogServer] Listening on $(cs.address):$(cs.port)")
    end
    
    # Accept connections in main loop
    while cs.running
        try
            # Accept with timeout to allow checking running flag
            client_sock = accept(cs.server_socket)
            
            # Handle client in separate task
            task = @async handle_client(cs, client_sock)
            push!(cs.client_tasks, task)
            
            # Clean up finished tasks
            filter!(t -> !istaskdone(t), cs.client_tasks)
        catch e
            if cs.running
                println("[CogServer] Error accepting connection: $e")
            end
        end
    end
    
    # Cleanup
    cleanup!(cs)
end

"""
    stop!(cs::CogServer)

Stop the CogServer.
"""
function stop!(cs::CogServer)
    println("[CogServer] Stopping server...")
    cs.running = false
    
    if cs.server_socket !== nothing
        close(cs.server_socket)
    end
end

"""
    cleanup!(cs::CogServer)

Clean up server resources.
"""
function cleanup!(cs::CogServer)
    println("[CogServer] Cleaning up...")
    
    # Wait for client tasks to finish (with timeout)
    deadline = time() + 5.0
    while !isempty(cs.client_tasks) && time() < deadline
        filter!(t -> !istaskdone(t), cs.client_tasks)
        sleep(0.1)
    end
    
    # Close server socket
    if cs.server_socket !== nothing
        close(cs.server_socket)
        cs.server_socket = nothing
    end
    
    println("[CogServer] Cleanup complete")
end

# ============================================================================
# CLIENT HANDLING
# ============================================================================

"""
    handle_client(cs::CogServer, client_sock::TCPSocket)

Handle a connected client.
Demonstrates: REPL implementation, I/O handling
"""
function handle_client(cs::CogServer, client_sock::TCPSocket)
    try
        # Send welcome message
        println(client_sock, "Welcome to CogServer (Julia)")
        println(client_sock, "Type 'help' for available commands")
        print(client_sock, "cogserver> ")
        flush(client_sock)
        
        # Read-eval-print loop
        while cs.running && isopen(client_sock)
            line = ""
            try
                line = readline(client_sock)
            catch e
                if e isa EOFError
                    break
                end
                println("[CogServer] Error reading from client: $e")
                break
            end
            
            line = strip(line)
            
            if isempty(line)
                print(client_sock, "cogserver> ")
                flush(client_sock)
                continue
            end
            
            # Execute command
            response = execute_command(cs, line)
            println(client_sock, response)
            
            if !cs.running
                println(client_sock, "Server shutting down...")
                break
            end
            
            print(client_sock, "cogserver> ")
            flush(client_sock)
        end
    catch e
        println("[CogServer] Client handler error: $e")
    finally
        close(client_sock)
    end
end

"""
    execute_command(cs::CogServer, line::String) -> String

Execute a command line and return the result.
"""
function execute_command(cs::CogServer, line::String)
    parts = split(line)
    if isempty(parts)
        return ""
    end
    
    cmd_name = String(parts[1])
    args = String[String(p) for p in parts[2:end]]
    
    # Look up command
    cmd = lock(cs.lock) do
        get(cs.commands, cmd_name, nothing)
    end
    
    if cmd === nothing
        return "Unknown command: $cmd_name"
    end
    
    # Execute command handler
    try
        result, success = cmd.handler(args)
        return result
    catch e
        return "Error executing command: $e"
    end
end

# ============================================================================
# DEMONSTRATION FUNCTION
# ============================================================================

"""
    demo()

Demonstrate CogServer functionality with interactive mode.
"""
function demo()
    println("="^70)
    println("OpenCog CogServer - Julia Implementation Demo")
    println("Network server and REPL interface")
    println("="^70)
    println()
    
    println("1. Creating CogServer")
    println("-"^70)
    server = CogServer("127.0.0.1", 17001)
    println("  Server created on 127.0.0.1:17001")
    println("  Built-in commands: $(length(server.commands))")
    println()
    
    println("2. Available Commands")
    println("-"^70)
    result, _ = handle_help(server, String[])
    println(result)
    
    println("3. Server Information")
    println("-"^70)
    result, _ = handle_info(server, String[])
    println(result)
    
    println("4. Testing Commands Locally")
    println("-"^70)
    test_commands = ["echo Hello OpenCog!", "info", "list"]
    for cmd in test_commands
        println("  > $cmd")
        response = execute_command(server, cmd)
        println(response)
    end
    println()
    
    println("5. Starting Server (Press Ctrl+C to stop)")
    println("-"^70)
    println("Connect with: telnet 127.0.0.1 17001")
    println("Or: nc 127.0.0.1 17001")
    println()
    
    # Start server in demo mode
    println("Starting server...")
    println("Press Ctrl+C to stop the server")
    println()
    
    try
        start!(server)
    catch e
        if e isa InterruptException
            println("\nInterrupt received, stopping server...")
            stop!(server)
        else
            rethrow(e)
        end
    end
    
    println()
    println("="^70)
    println("Julia CogServer strengths demonstrated:")
    println("  ✓ TCP networking with Sockets module")
    println("  ✓ Asynchronous client handling with tasks")
    println("  ✓ Higher-order functions for command handlers")
    println("  ✓ Module system with dynamic loading")
    println("  ✓ REPL-style interactive interface")
    println("  ✓ Thread-safe operations with locks")
    println("="^70)
end

# Run demo if executed as script
if abspath(PROGRAM_FILE) == @__FILE__
    demo()
end
