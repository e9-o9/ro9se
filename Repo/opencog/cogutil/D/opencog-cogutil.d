#!/usr/bin/env rdmd
// opencog-cogutil.d
// OpenCog Cogutil - D Utility Library
// Demonstrates: Templates, CTFE, mixins, ranges, UFCS

import std.stdio;
import std.string;
import std.algorithm;
import std.range;
import std.datetime;
import std.conv;
import std.traits;

// ===== Logger System =====
// Demonstrates: Enums, classes, templates

enum LogLevel {
    DEBUG, INFO, WARN, ERROR
}

class Logger {
    private string name;
    private LogLevel minLevel;
    
    this(string name, LogLevel level = LogLevel.INFO) {
        this.name = name;
        this.minLevel = level;
    }
    
    private string timestamp() {
        auto now = Clock.currTime();
        return format("%02d:%02d:%02d", now.hour, now.minute, now.second);
    }
    
    private void log(LogLevel level, string message) {
        if (level >= minLevel) {
            writefln("[%s] %s: %s", timestamp(), level, message);
        }
    }
    
    void debug_(string msg) { log(LogLevel.DEBUG, msg); }
    void info(string msg) { log(LogLevel.INFO, msg); }
    void warn(string msg) { log(LogLevel.WARN, msg); }
    void error(string msg) { log(LogLevel.ERROR, msg); }
    
    void setLevel(LogLevel level) { minLevel = level; }
}

// ===== Configuration Manager =====
// Demonstrates: Associative arrays, properties

class Config {
    private string[string] data;
    
    void set(string key, string value) {
        data[key] = value;
    }
    
    string get(string key, string defaultValue = "") {
        return key in data ? data[key] : defaultValue;
    }
    
    bool has(string key) {
        return (key in data) != null;
    }
    
    void dump() {
        import std.algorithm.sorting : sort;
        foreach (key; data.keys.sort) {
            writefln("%s = %s", key, data[key]);
        }
    }
}

// ===== Timer Utility =====
// Demonstrates: Structs, MonoTime

struct Timer {
    string name;
    Logger logger;
    MonoTime startTime;
    
    void start() {
        startTime = MonoTime.currTime;
    }
    
    double stop() {
        auto elapsed = (MonoTime.currTime - startTime).total!"usecs" / 1_000_000.0;
        if (logger !is null) {
            logger.info(format("%s completed in %.6f seconds", name, elapsed));
        }
        return elapsed;
    }
}

// ===== String Utilities with Templates =====
// Demonstrates: Template functions, UFCS (Uniform Function Call Syntax)

auto strSplit(string text, string delimiter = ",") {
    return text.split(delimiter).map!(strip).filter!(s => s.length > 0).array;
}

string strJoin(Range)(Range strings, string delimiter = ",") if (isInputRange!Range) {
    return strings.joiner(delimiter).to!string;
}

string toLower(string text) { return text.toLower(); }
string toUpper(string text) { return text.toUpper(); }
string trim(string text) { return text.strip(); }

// ===== Meta-programming with Mixins =====
// Demonstrates: String mixins, compile-time code generation

mixin template PropertyMixin(T, string name) {
    mixin("private " ~ T.stringof ~ " _" ~ name ~ ";");
    mixin("@property " ~ T.stringof ~ " " ~ name ~ "() { return _" ~ name ~ "; }");
    mixin("@property void " ~ name ~ "(" ~ T.stringof ~ " val) { _" ~ name ~ " = val; }");
}

class Example {
    mixin PropertyMixin!(int, "value");
    mixin PropertyMixin!(string, "text");
}

// ===== Demonstration =====

void demonstrateCogutil() {
    writeln("=".repeat(70));
    writeln("OpenCog Cogutil - D Utility Library Demo");
    writeln("Showcasing: Templates, CTFE, mixins, ranges, metaprogramming");
    writeln("=".repeat(70));
    writeln();
    
    // Logger
    writeln("1. Logger with Enums");
    writeln("-".repeat(50));
    auto logger = new Logger("CogUtil");
    logger.info("Cogutil library initialized");
    logger.debug_("Won't show");
    logger.warn("Warning message");
    logger.setLevel(LogLevel.DEBUG);
    logger.debug_("Now visible");
    writeln();
    
    // Config
    writeln("2. Configuration Manager");
    writeln("-".repeat(50));
    auto config = new Config();
    config.set("opencog.version", "1.0.0");
    config.set("atomspace.enabled", "true");
    logger.info("Configuration:");
    config.dump();
    writeln();
    
    // Timer
    writeln("3. Timer");
    writeln("-".repeat(50));
    Timer timer = Timer("Processing", logger);
    timer.start();
    // Simulate work
    long sum = 0;
    foreach (i; 0 .. 1_000_000) sum += i;
    timer.stop();
    writeln();
    
    // Ranges and UFCS
    writeln("4. Ranges and UFCS");
    writeln("-".repeat(50));
    auto numbers = iota(1, 11);  // 1..10
    auto squares = numbers.map!(x => x * x);
    auto evens = numbers.filter!(x => x % 2 == 0);
    logger.info(format("Squares: %s", squares.array));
    logger.info(format("Evens: %s", evens.array));
    writeln();
    
    // Template functions
    writeln("5. Template Functions");
    writeln("-".repeat(50));
    auto parts = strSplit("OpenCog,AtomSpace,CogServer", ",");
    logger.info(format("Split: %s", parts));
    writeln();
    
    writeln("=".repeat(70));
    writeln("D strengths demonstrated:");
    writeln("  ✓ Templates for generic programming");
    writeln("  ✓ CTFE (Compile-Time Function Execution)");
    writeln("  ✓ Mixins for code generation");
    writeln("  ✓ Ranges for functional operations");
    writeln("  ✓ UFCS for method chaining");
    writeln("  ✓ Fast compilation and execution");
    writeln("=".repeat(70));
}

void main() {
    demonstrateCogutil();
}
