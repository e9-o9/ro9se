#!/usr/bin/env rdmd
// opencog-cogserver.d - Network Server in D
import std.stdio, std.string, std.algorithm, std.datetime;

struct CommandResult {
    bool success;
    string message;
}

alias CommandHandler = CommandResult delegate(string[] args);

class CommandRegistry {
    private CommandHandler[string] commands;
    private string[string] descriptions;
    
    void register(string name, CommandHandler handler, string desc = "") {
        commands[name] = handler;
        descriptions[name] = desc;
    }
    
    CommandResult execute(string name, string[] args) {
        if (auto cmd = name in commands) {
            try {
                return (*cmd)(args);
            } catch (Exception e) {
                return CommandResult(false, "Error: " ~ e.msg);
            }
        }
        return CommandResult(false, "Unknown command: " ~ name);
    }
    
    string[] listCommands() {
        return commands.keys.sort.array;
    }
}

class Session {
    string id;
    SysTime createdAt;
    string[string] data;
    bool authenticated;
    
    this(string id) {
        this.id = id;
        this.createdAt = Clock.currTime;
    }
    
    void set(string key, string value) { data[key] = value; }
    string get(string key, string defaultVal = "") { return data.get(key, defaultVal); }
}

class CogServer {
    string name;
    CommandRegistry registry;
    Session[string] sessions;
    
    this(string name) {
        this.name = name;
        this.registry = new CommandRegistry();
    }
    
    void registerCommand(string name, CommandHandler handler, string desc = "") {
        registry.register(name, handler, desc);
    }
    
    CommandResult execute(string name, string[] args) {
        return registry.execute(name, args);
    }
}

void registerBuiltinCommands(CogServer server) {
    server.registerCommand("help", (args) {
        return CommandResult(true, "Commands: " ~ server.registry.listCommands().join(", "));
    }, "Show commands");
    
    server.registerCommand("version", (args) {
        return CommandResult(true, "OpenCog D v1.0.0");
    }, "Show version");
    
    server.registerCommand("echo", (args) {
        return CommandResult(true, args.join(" "));
    }, "Echo args");
}

void demonstrateCogserver() {
    writeln("==================================================================");
    writeln("OpenCog CogServer - D Implementation");
    writeln("==================================================================\n");
    
    auto server = new CogServer("TestServer");
    registerBuiltinCommands(server);
    
    writeln("Commands: ", server.registry.listCommands().join(", "));
    
    writeln("\nExecuting version:");
    auto result = server.execute("version", []);
    writeln(result.message);
    
    writeln("\nD CogServer: Templates, delegates, ranges, fast compilation");
}

void main() { demonstrateCogserver(); }
