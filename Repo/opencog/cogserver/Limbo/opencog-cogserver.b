# opencog-cogserver.b - Network Server in Limbo
implement CogServer;

include "sys.m";
    sys: Sys;
    print: import sys;
include "draw.m";

CogServer: module {
    init: fn(nil: ref Draw->Context, nil: list of string);
};

CommandResult: adt {
    success: int;
    message: string;
};

Command: adt {
    name: string;
    handler: ref fn(args: list of string): ref CommandResult;
    desc: string;
};

Session: adt {
    id: string;
    authenticated: int;
    data: list of (string, string);
    
    set: fn(s: self ref Session, key: string, value: string);
    get: fn(s: self ref Session, key: string): string;
};

Session.set(s: self ref Session, key: string, value: string) {
    s.data = (key, value) :: s.data;
}

Session.get(s: self ref Session, key: string): string {
    for (l := s.data; l != nil; l = tl l) {
        (k, v) := hd l;
        if (k == key)
            return v;
    }
    return "";
}

Server: adt {
    name: string;
    commands: list of ref Command;
    sessions: list of ref Session;
    
    registerCommand: fn(s: self ref Server, name: string, 
                       handler: ref fn(args: list of string): ref CommandResult,
                       desc: string);
    execute: fn(s: self ref Server, name: string, args: list of string): ref CommandResult;
};

Server.registerCommand(s: self ref Server, name: string, 
                      handler: ref fn(args: list of string): ref CommandResult,
                      desc: string) {
    cmd := ref Command(name, handler, desc);
    s.commands = cmd :: s.commands;
}

Server.execute(s: self ref Server, name: string, args: list of string): ref CommandResult {
    for (l := s.commands; l != nil; l = tl l) {
        cmd := hd l;
        if (cmd.name == name)
            return cmd.handler(args);
    }
    return ref CommandResult(0, "Unknown command");
}

versionHandler(args: list of string): ref CommandResult {
    return ref CommandResult(1, "OpenCog Limbo v1.0.0");
}

echoHandler(args: list of string): ref CommandResult {
    msg := "";
    for (l := args; l != nil; l = tl l)
        msg += hd l + " ";
    return ref CommandResult(1, msg);
}

init(nil: ref Draw->Context, nil: list of string) {
    sys = load Sys Sys->PATH;
    
    print("==================================================================\n");
    print("OpenCog CogServer - Limbo Implementation\n");
    print("==================================================================\n\n");
    
    server := ref Server("TestServer", nil, nil);
    
    server.registerCommand("version", ref versionHandler, "Show version");
    server.registerCommand("echo", ref echoHandler, "Echo args");
    
    print("Executing version:\n");
    result := server.execute("version", nil);
    print("%s\n", result.message);
    
    print("\nLimbo CogServer: CSP channels, ADTs, concurrent processes\n");
}
