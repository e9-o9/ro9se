# opencog-cogutil.b
# OpenCog Cogutil - Limbo Utility Library (Inferno OS)
# Demonstrates: Channels, CSP concurrency, modules, ADTs

implement CogUtil;

include "sys.m";
    sys: Sys;
    print: import sys;
include "draw.m";

CogUtil: module {
    init: fn(nil: ref Draw->Context, nil: list of string);
};

# Logger levels
LogLevel: adt {
    level: int;
    name: string;
    
    DEBUG: fn(): ref LogLevel;
    INFO: fn(): ref LogLevel;
    WARN: fn(): ref LogLevel;
    ERROR: fn(): ref LogLevel;
};

LogLevel.DEBUG(): ref LogLevel {
    return ref LogLevel(0, "DEBUG");
}

LogLevel.INFO(): ref LogLevel {
    return ref LogLevel(1, "INFO");
}

LogLevel.WARN(): ref LogLevel {
    return ref LogLevel(2, "WARN");
}

LogLevel.ERROR(): ref LogLevel {
    return ref LogLevel(3, "ERROR");
}

# Logger implementation
Logger: adt {
    name: string;
    minLevel: ref LogLevel;
    
    log: fn(l: self ref Logger, level: ref LogLevel, msg: string);
    info: fn(l: self ref Logger, msg: string);
    warn: fn(l: self ref Logger, msg: string);
    error: fn(l: self ref Logger, msg: string);
};

Logger.log(l: self ref Logger, level: ref LogLevel, msg: string) {
    if (level.level >= l.minLevel.level) {
        print("[%s] %s: %s\n", level.name, level.name, msg);
    }
}

Logger.info(l: self ref Logger, msg: string) {
    l.log(LogLevel.INFO(), msg);
}

Logger.warn(l: self ref Logger, msg: string) {
    l.log(LogLevel.WARN(), msg);
}

Logger.error(l: self ref Logger, msg: string) {
    l.log(LogLevel.ERROR(), msg);
}

# Configuration using string tables
Config: adt {
    data: list of (string, string);
    
    set: fn(c: self ref Config, key: string, value: string);
    get: fn(c: self ref Config, key: string): string;
    has: fn(c: self ref Config, key: string): int;
};

Config.set(c: self ref Config, key: string, value: string) {
    c.data = (key, value) :: c.data;
}

Config.get(c: self ref Config, key: string): string {
    for (l := c.data; l != nil; l = tl l) {
        (k, v) := hd l;
        if (k == key)
            return v;
    }
    return "";
}

Config.has(c: self ref Config, key: string): int {
    for (l := c.data; l != nil; l = tl l) {
        (k, v) := hd l;
        if (k == key)
            return 1;
    }
    return 0;
}

# String utilities
tolower(s: string): string {
    # Simplified - Limbo has built-in string ops
    return s;
}

toupper(s: string): string {
    return s;
}

# Main demonstration
init(nil: ref Draw->Context, nil: list of string) {
    sys = load Sys Sys->PATH;
    
    print("======================================================================\n");
    print("OpenCog Cogutil - Limbo Utility Library (Inferno OS)\n");
    print("Showcasing: ADTs, channels, CSP concurrency, modules\n");
    print("======================================================================\n\n");
    
    # Logger demonstration
    print("1. Logger with ADTs\n");
    print("--------------------------------------------------\n");
    logger := ref Logger("CogUtil", LogLevel.INFO());
    logger.info("Cogutil library initialized");
    logger.warn("This is a warning message");
    logger.error("This is an error message");
    print("\n");
    
    # Config demonstration
    print("2. Configuration Manager\n");
    print("--------------------------------------------------\n");
    config := ref Config(nil);
    config.set("opencog.version", "1.0.0");
    config.set("atomspace.enabled", "true");
    config.set("cogserver.port", "17001");
    
    print("Port: %s\n", config.get("cogserver.port"));
    print("\n");
    
    # Channel demonstration (CSP concurrency)
    print("3. CSP Concurrency with Channels\n");
    print("--------------------------------------------------\n");
    ch := chan of int;
    
    spawn worker(ch);
    
    # Send work
    ch <-= 42;
    ch <-= 100;
    
    print("Sent values through channel\n");
    print("\n");
    
    print("======================================================================\n");
    print("Limbo strengths demonstrated:\n");
    print("  ✓ Abstract Data Types (ADTs)\n");
    print("  ✓ CSP-style concurrency with channels\n");
    print("  ✓ Module system\n");
    print("  ✓ Pattern matching\n");
    print("  ✓ Type safety\n");
    print("  ✓ Inferno OS integration\n");
    print("======================================================================\n");
}

# Worker process demonstrating concurrency
worker(ch: chan of int) {
    for (;;) {
        val := <-ch;
        sys->print("Worker received: %d\n", val);
    }
}
