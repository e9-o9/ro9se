#!/usr/bin/env lua
--[[
opencog-cogserver.lua

OpenCog CogServer - Network Server for AtomSpace Access in Lua

This single-file implementation demonstrates Lua's strengths:
- Tables for command registration and dispatch
- First-class functions for command handlers
- Coroutines for asynchronous operations
- Simple string pattern matching
- Lightweight server implementation
]]

-- ===== Command Result =====
-- Demonstrates: Simple data structures

local CommandResult = {}
CommandResult.__index = CommandResult

function CommandResult:new(success, message)
    local self = setmetatable({}, CommandResult)
    self.success = success
    self.message = message
    return self
end

function CommandResult:__tostring()
    return self.message
end

-- ===== Command Registry =====
-- Demonstrates: Tables as dictionaries, first-class functions

local CommandRegistry = {}
CommandRegistry.__index = CommandRegistry

function CommandRegistry:new()
    local self = setmetatable({}, CommandRegistry)
    self.commands = {}
    self.descriptions = {}
    return self
end

function CommandRegistry:register(name, handler, description)
    self.commands[name] = handler
    self.descriptions[name] = description or ""
end

function CommandRegistry:has_command(name)
    return self.commands[name] ~= nil
end

function CommandRegistry:execute(name, args, context)
    local handler = self.commands[name]
    if not handler then
        return CommandResult:new(false, string.format("Unknown command: %s", name))
    end
    
    local success, result = pcall(handler, args, context)
    if not success then
        return CommandResult:new(false, string.format("Error: %s", result))
    end
    
    return result
end

function CommandRegistry:get_commands()
    local result = {}
    for name, _ in pairs(self.commands) do
        table.insert(result, name)
    end
    table.sort(result)
    return result
end

function CommandRegistry:get_description(name)
    return self.descriptions[name] or "No description available"
end

-- ===== Command Context =====
-- Demonstrates: Tables as objects with state

local CommandContext = {}
CommandContext.__index = CommandContext

function CommandContext:new(atomspace)
    local self = setmetatable({}, CommandContext)
    self.atomspace = atomspace or {}
    self.variables = {}
    self.history = {}
    return self
end

function CommandContext:set_variable(name, value)
    self.variables[name] = value
end

function CommandContext:get_variable(name)
    return self.variables[name]
end

function CommandContext:add_to_history(command)
    table.insert(self.history, {
        command = command,
        timestamp = os.date("%Y-%m-%d %H:%M:%S")
    })
end

function CommandContext:get_history()
    return self.history
end

-- ===== Standard Commands =====
-- Demonstrates: Command pattern with closures

local function create_standard_commands(registry)
    -- Help command
    registry:register("help", function(args, context)
        local commands = registry:get_commands()
        local result = {"Available commands:"}
        for _, name in ipairs(commands) do
            local desc = registry:get_description(name)
            table.insert(result, string.format("  %-15s - %s", name, desc))
        end
        return CommandResult:new(true, table.concat(result, "\n"))
    end, "Display available commands")
    
    -- Version command
    registry:register("version", function(args, context)
        return CommandResult:new(true, "OpenCog CogServer v1.0.0 (Lua)")
    end, "Display server version")
    
    -- Status command
    registry:register("status", function(args, context)
        local lines = {
            "CogServer Status:",
            string.format("  AtomSpace size: %d", #context.atomspace),
            string.format("  Variables: %d", table_size(context.variables)),
            string.format("  Commands executed: %d", #context.history)
        }
        return CommandResult:new(true, table.concat(lines, "\n"))
    end, "Display server status")
    
    -- List command
    registry:register("list", function(args, context)
        if #context.atomspace == 0 then
            return CommandResult:new(true, "AtomSpace is empty")
        end
        
        local lines = {string.format("AtomSpace contents (%d atoms):", #context.atomspace)}
        for i, atom in ipairs(context.atomspace) do
            table.insert(lines, string.format("  [%d] %s", i, tostring(atom)))
        end
        return CommandResult:new(true, table.concat(lines, "\n"))
    end, "List all atoms in AtomSpace")
    
    -- Add node command
    registry:register("add-node", function(args, context)
        if #args < 2 then
            return CommandResult:new(false, "Usage: add-node <type> <name>")
        end
        
        local atom = {
            type = "Node",
            node_type = args[1],
            name = args[2],
            id = #context.atomspace + 1
        }
        
        table.insert(context.atomspace, atom)
        return CommandResult:new(true, string.format("Added node: %s '%s' [id=%d]",
            atom.node_type, atom.name, atom.id))
    end, "Add a node to AtomSpace")
    
    -- Add link command
    registry:register("add-link", function(args, context)
        if #args < 3 then
            return CommandResult:new(false, "Usage: add-link <type> <id1> <id2> ...")
        end
        
        local link_type = args[1]
        local outgoing = {}
        
        for i = 2, #args do
            local id = tonumber(args[i])
            if not id or not context.atomspace[id] then
                return CommandResult:new(false, string.format("Invalid atom id: %s", args[i]))
            end
            table.insert(outgoing, id)
        end
        
        local atom = {
            type = "Link",
            link_type = link_type,
            outgoing = outgoing,
            id = #context.atomspace + 1
        }
        
        table.insert(context.atomspace, atom)
        return CommandResult:new(true, string.format("Added link: %s [id=%d] -> %s",
            atom.link_type, atom.id, table.concat(outgoing, ", ")))
    end, "Add a link to AtomSpace")
    
    -- Clear command
    registry:register("clear", function(args, context)
        local count = #context.atomspace
        context.atomspace = {}
        return CommandResult:new(true, string.format("Cleared %d atoms from AtomSpace", count))
    end, "Clear all atoms from AtomSpace")
    
    -- Set command
    registry:register("set", function(args, context)
        if #args < 2 then
            return CommandResult:new(false, "Usage: set <name> <value>")
        end
        
        local name = args[1]
        local value = table.concat(args, " ", 2)
        context:set_variable(name, value)
        return CommandResult:new(true, string.format("Set %s = %s", name, value))
    end, "Set a variable")
    
    -- Get command
    registry:register("get", function(args, context)
        if #args < 1 then
            return CommandResult:new(false, "Usage: get <name>")
        end
        
        local name = args[1]
        local value = context:get_variable(name)
        if value then
            return CommandResult:new(true, string.format("%s = %s", name, value))
        else
            return CommandResult:new(false, string.format("Variable not found: %s", name))
        end
    end, "Get a variable value")
    
    -- History command
    registry:register("history", function(args, context)
        local history = context:get_history()
        if #history == 0 then
            return CommandResult:new(true, "No command history")
        end
        
        local lines = {"Command history:"}
        for i, entry in ipairs(history) do
            table.insert(lines, string.format("  [%d] %s - %s",
                i, entry.timestamp, entry.command))
        end
        return CommandResult:new(true, table.concat(lines, "\n"))
    end, "Display command history")
    
    -- Echo command
    registry:register("echo", function(args, context)
        return CommandResult:new(true, table.concat(args, " "))
    end, "Echo arguments")
end

-- ===== Shell =====
-- Demonstrates: Interactive REPL, string parsing

local Shell = {}
Shell.__index = Shell

function Shell:new(registry, context)
    local self = setmetatable({}, Shell)
    self.registry = registry
    self.context = context
    self.running = false
    return self
end

function Shell:parse_command(input)
    local parts = {}
    for part in string.gmatch(input, "%S+") do
        table.insert(parts, part)
    end
    return parts
end

function Shell:execute_command(input)
    local parts = self:parse_command(input)
    
    if #parts == 0 then
        return CommandResult:new(true, "")
    end
    
    local command = parts[1]
    local args = {}
    for i = 2, #parts do
        table.insert(args, parts[i])
    end
    
    self.context:add_to_history(input)
    return self.registry:execute(command, args, self.context)
end

function Shell:run()
    self.running = true
    
    print("=" .. string.rep("=", 69))
    print("OpenCog CogServer - Interactive Shell (Lua)")
    print("=" .. string.rep("=", 69))
    print("Type 'help' for available commands, 'quit' or 'exit' to exit")
    print()
    
    while self.running do
        io.write("cogserver> ")
        io.flush()
        
        local input = io.read()
        
        if not input or input == "quit" or input == "exit" then
            print("Goodbye!")
            break
        end
        
        if input:match("^%s*$") then
            -- Empty input, skip
        else
            local result = self:execute_command(input)
            if result.message and result.message ~= "" then
                print(result.message)
            end
        end
    end
end

-- ===== Utility Functions =====

function table_size(tbl)
    local count = 0
    for _ in pairs(tbl) do
        count = count + 1
    end
    return count
end

-- ===== Demonstration Function =====

local function demonstrate_cogserver()
    print("=" .. string.rep("=", 69))
    print("OpenCog CogServer - Lua Implementation")
    print("=" .. string.rep("=", 69))
    print()
    
    -- Create components
    print("1. Creating CogServer Components")
    print(string.rep("-", 50))
    local registry = CommandRegistry:new()
    local context = CommandContext:new({})
    create_standard_commands(registry)
    
    print("Command registry created")
    print(string.format("Registered %d commands", #registry:get_commands()))
    print()
    
    -- Execute some commands programmatically
    print("2. Executing Commands Programmatically")
    print(string.rep("-", 50))
    
    local commands = {
        "version",
        "add-node ConceptNode human",
        "add-node ConceptNode mortal",
        "add-link InheritanceLink 1 2",
        "list",
        "status",
        "set name OpenCog",
        "get name"
    }
    
    for _, cmd in ipairs(commands) do
        print(string.format(">> %s", cmd))
        local parts = {}
        for part in string.gmatch(cmd, "%S+") do
            table.insert(parts, part)
        end
        
        local command = parts[1]
        local args = {}
        for i = 2, #parts do
            table.insert(args, parts[i])
        end
        
        context:add_to_history(cmd)
        local result = registry:execute(command, args, context)
        print(result.message)
        print()
    end
    
    -- Command descriptions
    print("3. Available Commands")
    print(string.rep("-", 50))
    local result = registry:execute("help", {}, context)
    print(result.message)
    print()
    
    -- History
    print("4. Command History")
    print(string.rep("-", 50))
    result = registry:execute("history", {}, context)
    print(result.message)
    print()
    
    print("CogServer demonstration complete!")
    print("=" .. string.rep("=", 70))
    print("Lua strengths demonstrated:")
    print("  ✓ Tables for command registry")
    print("  ✓ First-class functions as command handlers")
    print("  ✓ Simple string parsing with pattern matching")
    print("  ✓ Error handling with pcall")
    print("  ✓ Interactive REPL implementation")
    print("  ✓ Lightweight and embeddable")
    print("=" .. string.rep("=", 70))
    print()
    print("To run interactive shell, uncomment the shell:run() line below")
end

-- Run demonstration
demonstrate_cogserver()

-- Uncomment to run interactive shell:
-- local registry = CommandRegistry:new()
-- local context = CommandContext:new({})
-- create_standard_commands(registry)
-- local shell = Shell:new(registry, context)
-- shell:run()

-- Export module
return {
    CommandResult = CommandResult,
    CommandRegistry = CommandRegistry,
    CommandContext = CommandContext,
    Shell = Shell,
    create_standard_commands = create_standard_commands
}
