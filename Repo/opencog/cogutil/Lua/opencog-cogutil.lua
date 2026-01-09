#!/usr/bin/env lua
--[[
opencog-cogutil.lua

OpenCog Cogutil - Lua Utility Library
A collection of utility functions and classes for OpenCog framework

This single-file implementation demonstrates Lua's strengths:
- Tables as universal data structure (arrays, dicts, objects)
- First-class functions and closures
- Metatables for object-oriented programming
- Lightweight and embeddable
- Simple and elegant syntax
]]

-- ===== Logger System =====
-- Demonstrates: Tables as objects, metatables for OOP

local LogLevel = {
    DEBUG = 1,
    INFO = 2,
    WARN = 3,
    ERROR = 4
}

local LogLevelNames = {
    [LogLevel.DEBUG] = "DEBUG",
    [LogLevel.INFO] = "INFO",
    [LogLevel.WARN] = "WARN",
    [LogLevel.ERROR] = "ERROR"
}

local Logger = {}
Logger.__index = Logger

function Logger:new(name, min_level)
    local self = setmetatable({}, Logger)
    self.name = name or "OpenCog"
    self.min_level = min_level or LogLevel.INFO
    return self
end

function Logger:log(level, message)
    if level >= self.min_level then
        local timestamp = os.date("%H:%M:%S")
        local level_name = LogLevelNames[level] or "UNKNOWN"
        print(string.format("[%s] %s: %s", timestamp, level_name, message))
    end
end

function Logger:debug(message)
    self:log(LogLevel.DEBUG, message)
end

function Logger:info(message)
    self:log(LogLevel.INFO, message)
end

function Logger:warn(message)
    self:log(LogLevel.WARN, message)
end

function Logger:error(message)
    self:log(LogLevel.ERROR, message)
end

function Logger:set_level(level)
    self.min_level = level
end

-- ===== Configuration Manager =====
-- Demonstrates: Tables as dictionaries, dynamic key access

local Config = {}
Config.__index = Config

function Config:new()
    local self = setmetatable({}, Config)
    self.settings = {}
    return self
end

function Config:set(key, value)
    self.settings[key] = value
end

function Config:get(key, default)
    return self.settings[key] or default
end

function Config:has(key)
    return self.settings[key] ~= nil
end

function Config:remove(key)
    self.settings[key] = nil
end

function Config:clear()
    self.settings = {}
end

function Config:dump()
    print("Configuration settings:")
    for key, value in pairs(self.settings) do
        print(string.format("  %s = %s", key, tostring(value)))
    end
end

function Config:size()
    local count = 0
    for _ in pairs(self.settings) do
        count = count + 1
    end
    return count
end

-- ===== Timer =====
-- Demonstrates: Closures, first-class functions

local Timer = {}
Timer.__index = Timer

function Timer:new(label, logger)
    local self = setmetatable({}, Timer)
    self.label = label or "Timer"
    self.logger = logger
    self.start_time = nil
    return self
end

function Timer:start()
    self.start_time = os.clock()
    if self.logger then
        self.logger:info(string.format("Timer '%s' started", self.label))
    end
end

function Timer:stop()
    if not self.start_time then
        error("Timer not started")
    end
    local elapsed = os.clock() - self.start_time
    if self.logger then
        self.logger:info(string.format("Timer '%s' stopped: %.4f seconds", self.label, elapsed))
    end
    return elapsed
end

function Timer:elapsed()
    if not self.start_time then
        return 0
    end
    return os.clock() - self.start_time
end

-- Higher-order function for timing
function measure_time(label, func)
    local start_time = os.clock()
    local result = func()
    local elapsed = os.clock() - start_time
    print(string.format("'%s' took %.4f seconds", label, elapsed))
    return result, elapsed
end

-- ===== String Utilities =====
-- Demonstrates: String manipulation, pattern matching, functional style

local StringUtils = {}

function StringUtils.split(str, delimiter)
    local result = {}
    local pattern = string.format("([^%s]+)", delimiter)
    for match in string.gmatch(str, pattern) do
        table.insert(result, match)
    end
    return result
end

function StringUtils.join(table, delimiter)
    return table.concat(table, delimiter)
end

function StringUtils.trim(str)
    return str:match("^%s*(.-)%s*$")
end

function StringUtils.to_upper(str)
    return string.upper(str)
end

function StringUtils.to_lower(str)
    return string.lower(str)
end

function StringUtils.starts_with(str, prefix)
    return string.sub(str, 1, #prefix) == prefix
end

function StringUtils.ends_with(str, suffix)
    return string.sub(str, -#suffix) == suffix
end

function StringUtils.contains(str, substring)
    return string.find(str, substring, 1, true) ~= nil
end

function StringUtils.reverse(str)
    return string.reverse(str)
end

function StringUtils.capitalize(str)
    return str:gsub("^%l", string.upper)
end

-- ===== Collection Utilities =====
-- Demonstrates: Functional programming with tables

local CollectionUtils = {}

function CollectionUtils.map(table, func)
    local result = {}
    for i, v in ipairs(table) do
        result[i] = func(v)
    end
    return result
end

function CollectionUtils.filter(table, predicate)
    local result = {}
    for _, v in ipairs(table) do
        if predicate(v) then
            table.insert(result, v)
        end
    end
    return result
end

function CollectionUtils.reduce(table, func, initial)
    local acc = initial
    for _, v in ipairs(table) do
        acc = func(acc, v)
    end
    return acc
end

function CollectionUtils.foreach(table, func)
    for i, v in ipairs(table) do
        func(v, i)
    end
end

function CollectionUtils.contains(table, value)
    for _, v in ipairs(table) do
        if v == value then
            return true
        end
    end
    return false
end

function CollectionUtils.size(table)
    local count = 0
    for _ in pairs(table) do
        count = count + 1
    end
    return count
end

-- ===== Memoization =====
-- Demonstrates: Closures, higher-order functions

function memoize(func)
    local cache = {}
    return function(...)
        local key = table.concat({...}, ",")
        if cache[key] == nil then
            cache[key] = func(...)
        end
        return cache[key]
    end
end

-- ===== Demonstration Function =====

local function demonstrate_cogutil()
    print("=" .. string.rep("=", 69))
    print("OpenCog Cogutil - Lua Implementation")
    print("=" .. string.rep("=", 69))
    print()
    
    -- Logger demonstration
    print("1. Logger System")
    print(string.rep("-", 50))
    local logger = Logger:new("OpenCog", LogLevel.INFO)
    
    logger:debug("This debug message won't show")
    logger:info("This is an info message")
    logger:warn("This is a warning message")
    logger:error("This is an error message")
    
    logger:set_level(LogLevel.DEBUG)
    logger:debug("Now debug messages are visible")
    print()
    
    -- Config demonstration
    print("2. Configuration Manager")
    print(string.rep("-", 50))
    local config = Config:new()
    config:set("opencog.version", "1.0.0")
    config:set("atomspace.enabled", "true")
    config:set("cogserver.port", "17001")
    
    logger:info("Configuration loaded:")
    config:dump()
    print()
    
    logger:info(string.format("Port setting: %s", config:get("cogserver.port")))
    logger:info(string.format("Config size: %d entries", config:size()))
    print()
    
    -- Timer demonstration
    print("3. Timer System")
    print(string.rep("-", 50))
    local timer = Timer:new("Processing", logger)
    timer:start()
    
    -- Simulate work
    local sum = 0
    for i = 1, 1000000 do
        sum = sum + i
    end
    
    timer:stop()
    print()
    
    -- Higher-order timing function
    print("4. Higher-Order Timing")
    print(string.rep("-", 50))
    local result, elapsed = measure_time("Computation", function()
        local product = 1
        for i = 1, 100 do
            product = product * 1.001
        end
        return product
    end)
    print()
    
    -- String utilities demonstration
    print("5. String Utilities")
    print(string.rep("-", 50))
    logger:info("String utilities demonstration:")
    local text = "OpenCog,AtomSpace,CogServer,Cogutil"
    local parts = StringUtils.split(text, ",")
    
    logger:info("Split result:")
    for _, part in ipairs(parts) do
        print(string.format("  - %s", part))
    end
    
    local joined = StringUtils.join(parts, " + ")
    logger:info(string.format("Joined: %s", joined))
    
    logger:info(string.format("Uppercase: %s", StringUtils.to_upper("opencog rocks")))
    logger:info(string.format("Lowercase: %s", StringUtils.to_lower("OPENCOG ROCKS")))
    logger:info(string.format("Trimmed: '%s'", StringUtils.trim("  spaced out  ")))
    logger:info(string.format("Capitalized: %s", StringUtils.capitalize("opencog")))
    logger:info(string.format("Reversed: %s", StringUtils.reverse("OpenCog")))
    print()
    
    -- Collection utilities demonstration
    print("6. Collection Utilities (Functional Style)")
    print(string.rep("-", 50))
    local numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    
    local squares = CollectionUtils.map(numbers, function(x) return x * x end)
    logger:info(string.format("Squares: %s", table.concat(squares, ", ")))
    
    local evens = CollectionUtils.filter(numbers, function(x) return x % 2 == 0 end)
    logger:info(string.format("Even numbers: %s", table.concat(evens, ", ")))
    
    local sum_result = CollectionUtils.reduce(numbers, function(acc, x) return acc + x end, 0)
    logger:info(string.format("Sum: %d", sum_result))
    print()
    
    -- Memoization demonstration
    print("7. Memoization")
    print(string.rep("-", 50))
    
    local function fibonacci(n)
        if n <= 1 then return n end
        return fibonacci(n-1) + fibonacci(n-2)
    end
    
    local fib_memo = memoize(fibonacci)
    
    local start = os.clock()
    local fib_result = fib_memo(30)
    local elapsed1 = os.clock() - start
    logger:info(string.format("Fibonacci(30) = %d (first call: %.4fs)", fib_result, elapsed1))
    
    start = os.clock()
    fib_result = fib_memo(30)
    local elapsed2 = os.clock() - start
    logger:info(string.format("Fibonacci(30) = %d (cached call: %.4fs)", fib_result, elapsed2))
    print()
    
    logger:info("Cogutil demonstration complete!")
    print("=" .. string.rep("=", 70))
    print("Lua strengths demonstrated:")
    print("  ✓ Tables as universal data structure")
    print("  ✓ Metatables for object-oriented programming")
    print("  ✓ First-class functions and closures")
    print("  ✓ Simple and elegant syntax")
    print("  ✓ Lightweight and fast")
    print("  ✓ Pattern matching for string manipulation")
    print("=" .. string.rep("=", 70))
end

-- Run demonstration if executed directly
if arg and arg[0]:match("opencog%-cogutil%.lua$") then
    demonstrate_cogutil()
end

-- Export module
return {
    Logger = Logger,
    LogLevel = LogLevel,
    Config = Config,
    Timer = Timer,
    StringUtils = StringUtils,
    CollectionUtils = CollectionUtils,
    measure_time = measure_time,
    memoize = memoize
}
