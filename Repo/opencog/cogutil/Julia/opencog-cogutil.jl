#!/usr/bin/env julia
#=
opencog-cogutil.jl

OpenCog Cogutil - Julia Utility Library
A collection of utility functions showcasing Julia's strengths

This single-file implementation demonstrates Julia's strengths:
- Multiple dispatch for polymorphic functions
- High performance (JIT compilation)
- Mathematical notation and Unicode support
- Metaprogramming with macros
- Optional typing for clarity and performance
=#

using Dates
using Printf

# ============================================================================
# LOGGER WITH MULTIPLE DISPATCH
# ============================================================================

# Log levels as enum-like constants
@enum LogLevel DEBUG=1 INFO=2 WARN=3 ERROR=4

# Logger struct with parametric type
struct Logger
    name::String
    level::LogLevel
end

# Multiple dispatch for logging at different levels
# Demonstrates: Multiple dispatch, method specialization
function log_message(logger::Logger, level::LogLevel, msg::String)
    if level >= logger.level
        timestamp = Dates.format(now(), "yyyy-mm-dd HH:MM:SS")
        println("[$timestamp] $level: $msg")
    end
end

# Convenience methods using multiple dispatch
log_debug(logger::Logger, msg::String) = log_message(logger, DEBUG, msg)
log_info(logger::Logger, msg::String) = log_message(logger, INFO, msg)
log_warn(logger::Logger, msg::String) = log_message(logger, WARN, msg)
log_error(logger::Logger, msg::String) = log_message(logger, ERROR, msg)

# ============================================================================
# CONFIGURATION WITH DICTIONARY
# ============================================================================

# Configuration as mutable struct
mutable struct Config
    data::Dict{String, String}
end

Config() = Config(Dict{String, String}())

# Multiple dispatch for setting/getting
function set!(config::Config, key::String, value::String)
    config.data[key] = value
    config
end

# Multiple dispatch with default parameter
function get(config::Config, key::String, default::String="")
    get(config.data, key, default)
end

has(config::Config, key::String) = haskey(config.data, key)

function dump(config::Config)
    for (k, v) in sort(collect(config.data))
        println("  $k = $v")
    end
end

# ============================================================================
# TIMER WITH METAPROGRAMMING
# ============================================================================

# Macro for timing code blocks
# Demonstrates: Metaprogramming, code generation
macro timed(name, ex)
    quote
        local start_time = time()
        local result = $(esc(ex))
        local elapsed = time() - start_time
        println("[TIMER] ", $(esc(name)), " took ", @sprintf("%.6f", elapsed), " seconds")
        result
    end
end

# Function-based timer
function time_function(f::Function, name::String)
    start = time()
    result = f()
    elapsed = time() - start
    println("[TIMER] $name took $(@sprintf("%.6f", elapsed)) seconds")
    (result, elapsed)
end

# ============================================================================
# STRING UTILITIES WITH UNICODE SUPPORT
# ============================================================================

module StringUtils
    # Julia supports Unicode naturally
    split_string(s::String, delim::Char=',') = filter(!isempty, split(s, delim))
    
    join_strings(strings::Vector, delim::String=",") = join(strings, delim)
    
    # Multiple dispatch for different types
    to_lower(s::String) = lowercase(s)
    to_upper(s::String) = uppercase(s)
    trim_string(s::String) = strip(s)
    
    # Unicode example - Julia handles Unicode seamlessly
    function show_unicode_support()
        greek = "α β γ δ"
        math = "∑ ∫ ∂ ∇"
        emoji = "🔬 💻 🧬"
        println("  Greek: $greek")
        println("  Math: $math")
        println("  Emoji: $emoji")
    end
end

# ============================================================================
# MATHEMATICAL OPERATIONS (Julia's strength)
# ============================================================================

# Matrix operations - Julia excels at numerical computing
function demo_matrix_ops()
    # Create matrices with mathematical notation
    A = [1 2; 3 4]
    B = [5 6; 7 8]
    
    # Mathematical operations are clean and efficient
    C = A * B  # Matrix multiplication
    D = A .+ B # Element-wise addition
    
    println("  Matrix A:\n$A")
    println("  Matrix B:\n$B")
    println("  A * B:\n$C")
    println("  A .+ B:\n$D")
end

# Vector operations with broadcasting
function demo_broadcasting()
    x = [1, 2, 3, 4, 5]
    
    # Broadcasting with . operator
    y = x .^ 2  # Square each element
    z = 2 .* x .+ 1  # Vectorized operation
    
    println("  x = $x")
    println("  x² = $y")
    println("  2x + 1 = $z")
end

# ============================================================================
# TYPE SYSTEM AND PERFORMANCE
# ============================================================================

# Abstract type hierarchy
abstract type Shape end

struct Circle <: Shape
    radius::Float64
end

struct Rectangle <: Shape
    width::Float64
    height::Float64
end

# Multiple dispatch based on type
area(c::Circle) = π * c.radius^2
area(r::Rectangle) = r.width * r.height

# Generic function with type constraints
function describe(shape::T) where T <: Shape
    a = area(shape)
    println("  $(typeof(shape)): area = $(@sprintf("%.2f", a))")
end

# ============================================================================
# PARALLEL COMPUTING (Julia's strength)
# ============================================================================

# Parallel map (demonstrative - would need Distributed package in practice)
function demo_parallel_concept()
    println("  Julia supports easy parallelization:")
    println("  - @threads for parallel loops")
    println("  - @distributed for distributed computing")
    println("  - pmap for parallel mapping")
    println("  - Shared arrays for parallel data")
end

# ============================================================================
# MAIN DEMONSTRATION
# ============================================================================

function main()
    println("="^70)
    println("OpenCog Cogutil - Julia Utility Library Demo")
    println("Showcasing Julia's strengths: Performance, multiple dispatch, math")
    println("="^70)
    println()
    
    # Logger demonstration
    println("1. Logger with Multiple Dispatch")
    println("-"^70)
    logger = Logger("CogUtil", INFO)
    log_info(logger, "Cogutil library initialized")
    log_debug(logger, "This debug message won't show (level too low)")
    log_warn(logger, "This is a warning message")
    log_error(logger, "This is an error message")
    println()
    
    # Config demonstration
    println("2. Configuration Management")
    println("-"^70)
    config = Config()
    set!(config, "opencog.version", "1.0.0")
    set!(config, "atomspace.enabled", "true")
    set!(config, "cogserver.port", "17001")
    
    log_info(logger, "Configuration loaded:")
    dump(config)
    println()
    
    # Macro timing
    println("3. Timing with Macros")
    println("-"^70)
    log_info(logger, "Computing sum with macro...")
    result = @timed "Sum computation" sum(1:1_000_000)
    log_info(logger, "Result: $result")
    println()
    
    # String utilities
    println("4. String Utilities")
    println("-"^70)
    log_info(logger, "String utilities demonstration:")
    text = "OpenCog,AtomSpace,CogServer,Cogutil"
    parts = StringUtils.split_string(text, ',')
    
    println("  Split result:")
    for part in parts
        println("    - $part")
    end
    
    joined = StringUtils.join_strings(parts, " + ")
    log_info(logger, "Joined: $joined")
    
    log_info(logger, "Uppercase: $(StringUtils.to_upper("opencog rocks"))")
    log_info(logger, "Lowercase: $(StringUtils.to_lower("OPENCOG ROCKS"))")
    println()
    
    # Unicode support
    println("5. Unicode Support (Natural in Julia)")
    println("-"^70)
    log_info(logger, "Julia handles Unicode seamlessly:")
    StringUtils.show_unicode_support()
    println()
    
    # Mathematical operations
    println("6. Matrix Operations (Julia's Strength)")
    println("-"^70)
    log_info(logger, "Demonstrating matrix math:")
    demo_matrix_ops()
    println()
    
    # Broadcasting
    println("7. Broadcasting")
    println("-"^70)
    log_info(logger, "Vectorized operations:")
    demo_broadcasting()
    println()
    
    # Type system
    println("8. Multiple Dispatch with Types")
    println("-"^70)
    log_info(logger, "Type-based polymorphism:")
    circle = Circle(5.0)
    rectangle = Rectangle(4.0, 6.0)
    describe(circle)
    describe(rectangle)
    println()
    
    # Parallel computing
    println("9. Parallel Computing Capabilities")
    println("-"^70)
    log_info(logger, "Julia's parallelism features:")
    demo_parallel_concept()
    println()
    
    # Performance note
    println("10. Performance Characteristics")
    println("-"^70)
    log_info(logger, "Julia performance features:")
    println("  - JIT compilation to native code")
    println("  - Type inference for optimization")
    println("  - LLVM-based code generation")
    println("  - Performance approaching C/Fortran")
    println()
    
    log_info(logger, "Cogutil demonstration complete!")
    println()
    println("="^70)
    println("Julia strengths demonstrated:")
    println("  ✓ Multiple dispatch for polymorphism")
    println("  ✓ High performance (JIT to native code)")
    println("  ✓ Mathematical notation and operations")
    println("  ✓ Unicode support throughout")
    println("  ✓ Metaprogramming with macros")
    println("  ✓ Optional typing for clarity")
    println("  ✓ Easy parallelization")
    println("  ✓ Excellent for scientific computing")
    println("="^70)
end

# Run if executed as script
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
