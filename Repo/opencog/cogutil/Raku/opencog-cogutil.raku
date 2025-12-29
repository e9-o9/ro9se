#!/usr/bin/env raku
# opencog-cogutil.raku
#
# OpenCog Cogutil - Raku (Perl 6) Utility Library
# A collection of utility functions for OpenCog framework
#
# This single-file implementation demonstrates Raku's strengths:
# - Gradual typing (optional static types)
# - Grammars for parsing
# - Junctions for logic operations
# - Unicode operators and identifiers
# - Powerful regex and rule system
# - Multiple dispatch

use v6;

# ===== Logger System =====
# Demonstrates: Enumerations, classes with typed attributes

enum LogLevel <DEBUG INFO WARN ERROR>;

class Logger {
    has Str $.name;
    has LogLevel $.level is rw = INFO;
    
    method !timestamp() returns Str {
        my $dt = DateTime.now;
        sprintf "%02d:%02d:%02d", $dt.hour, $dt.minute, $dt.second;
    }
    
    method !log(LogLevel $level, Str $message) {
        if $level >= $!level {
            say "[{self!timestamp}] {$level.Str}: $message";
        }
    }
    
    method debug(Str $message) { self!log(DEBUG, $message) }
    method info(Str $message)  { self!log(INFO, $message) }
    method warn(Str $message)  { self!log(WARN, $message) }
    method error(Str $message) { self!log(ERROR, $message) }
    
    method set-level(LogLevel $level) {
        $!level = $level;
    }
}

# ===== Configuration Manager =====
# Demonstrates: Hash attributes, gradual typing

class Config {
    has %.data;
    
    method set(Str $key, Str $value) {
        %!data{$key} = $value;
    }
    
    method get(Str $key, Str $default = '') returns Str {
        %!data{$key} // $default;
    }
    
    method has(Str $key) returns Bool {
        %!data{$key}:exists;
    }
    
    method load-from-file(Str $filename) returns Bool {
        return False unless $filename.IO.e;
        
        for $filename.IO.lines -> $line {
            my $trimmed = $line.trim;
            next if $trimmed eq '' || $trimmed.starts-with('#');
            
            if $trimmed ~~ / ^ (<-[=]>+) '=' (.+) $ / {
                self.set(~$0.trim, ~$1.trim);
            }
        }
        
        return True;
    }
    
    method dump() {
        for %!data.keys.sort -> $key {
            say "$key = %!data{$key}";
        }
    }
}

# ===== Timer Utility =====
# Demonstrates: Instant type, duration calculations

class Timer {
    has Str $.name;
    has Logger $.logger;
    has Instant $!start-time;
    
    method start() {
        $!start-time = now;
    }
    
    method stop() returns Num {
        return 0 without $!start-time;
        
        my $elapsed = now - $!start-time;
        if $!logger {
            $!logger.info("$.name completed in {$elapsed.fmt('%.6f')} seconds");
        }
        return $elapsed;
    }
}

# ===== String Utilities =====
# Demonstrates: Unicode support, multiple dispatch, cool operators

class StringUtils {
    # Multiple dispatch based on parameter types
    multi method split(Str $text, Str $delimiter = ',') returns Array[Str] {
        $text.split($delimiter).map(*.trim).grep(* ne '').Array;
    }
    
    multi method join(Array[Str] $strings, Str $delimiter = ',') returns Str {
        $strings.join($delimiter);
    }
    
    method to-lower(Str $text) returns Str {
        $text.lc;
    }
    
    method to-upper(Str $text) returns Str {
        $text.uc;
    }
    
    method trim(Str $text) returns Str {
        $text.trim;
    }
    
    method reverse-str(Str $text) returns Str {
        $text.flip;  # Raku's reverse is called flip
    }
    
    method contains(Str $text, Str $substring) returns Bool {
        $text.contains($substring);
    }
    
    # Camel case conversions
    method camel-to-snake(Str $text) returns Str {
        $text.subst(/ <[A..Z]> /, { "_$_".lc }, :g).subst(/^ '_' /, '');
    }
    
    method snake-to-camel(Str $text) returns Str {
        $text.subst(/ '_' (<[a..z]>) /, { $0.uc }, :g);
    }
}

# ===== Grammar for Command Parsing =====
# Demonstrates: Raku's powerful grammar system

grammar CommandGrammar {
    token TOP { <command> [ \s+ <args> ]? }
    token command { \w+ }
    token args { <word>+ % \s+ }
    token word { \S+ }
}

# ===== Junction Demonstration =====
# Demonstrates: Junctions for logical operations (Raku unique feature)

sub any-of(@values, $test) {
    return so any(@values) ~~ $test;
}

sub all-of(@values, $test) {
    return so all(@values) ~~ $test;
}

# ===== Main Demonstration =====

sub demonstrate-cogutil() {
    say '=' x 70;
    say "OpenCog Cogutil - Raku (Perl 6) Utility Library Demo";
    say "Showcasing Raku's strengths: Gradual typing, grammars, junctions, Unicode";
    say '=' x 70;
    say '';
    
    # Logger demonstration
    say "1. Logger with Enums and Gradual Typing";
    say '-' x 50;
    my $logger = Logger.new(name => "CogUtil", level => INFO);
    $logger.info("Cogutil library initialized");
    $logger.debug("This debug message won't show (level too low)");
    $logger.warn("This is a warning message");
    $logger.error("This is an error message");
    
    $logger.set-level(DEBUG);
    $logger.debug("Now debug messages are visible");
    say '';
    
    # Config demonstration
    say "2. Configuration Manager with Typed Hashes";
    say '-' x 50;
    my $config = Config.new;
    $config.set("opencog.version", "1.0.0");
    $config.set("atomspace.enabled", "true");
    $config.set("cogserver.port", "17001");
    
    $logger.info("Configuration loaded:");
    $config.dump;
    say '';
    
    $logger.info("Port setting: {$config.get('cogserver.port')}");
    say '';
    
    # Timer demonstration
    say "3. Timer with Instant Type";
    say '-' x 50;
    my $timer = Timer.new(name => "Processing", logger => $logger);
    $timer.start;
    $logger.info("Simulating some work...");
    
    # Raku's range and reduction operators
    my $total = [+] 1..1_000_000;  # Reduction with + operator
    
    $timer.stop;
    say '';
    
    # String utilities demonstration
    say "4. String Utilities with Multiple Dispatch";
    say '-' x 50;
    my $utils = StringUtils.new;
    $logger.info("String utilities demonstration:");
    my $text = "OpenCog,AtomSpace,CogServer,Cogutil";
    my @parts = $utils.split($text, ',');
    
    $logger.info("Split result:");
    say "  - $_" for @parts;
    
    my $joined = $utils.join(@parts, " + ");
    $logger.info("Joined: $joined");
    
    $logger.info("Uppercase: {$utils.to-upper('opencog rocks')}");
    $logger.info("Lowercase: {$utils.to-lower('OPENCOG ROCKS')}");
    $logger.info("Trimmed: '{$utils.trim('  spaced out  ')}'");
    $logger.info("Reversed: {$utils.reverse-str('Raku')}");
    
    # Raku-specific: Case conversions
    $logger.info("camelCase → snake_case: {$utils.camel-to-snake('myVariableName')}");
    $logger.info("snake_case → camelCase: {$utils.snake-to-camel('my_variable_name')}");
    say '';
    
    # Grammar demonstration
    say "5. Grammar-Based Parsing (Raku Unique)";
    say '-' x 50;
    my @commands = ('help', 'echo hello world', 'status');
    
    for @commands -> $cmd {
        if CommandGrammar.parse($cmd) -> $match {
            $logger.info("Parsed '$cmd':");
            say "  Command: {$match<command>}";
            say "  Args: {$match<args> // 'none'}";
        }
    }
    say '';
    
    # Junctions demonstration
    say "6. Junctions for Logic Operations (Raku Unique)";
    say '-' x 50;
    my @numbers = (2, 4, 6, 8);
    
    # Junction 'any' - true if any value matches
    if any(@numbers) == 4 {
        $logger.info("Found 4 in array using 'any' junction");
    }
    
    # Junction 'all' - true if all values match
    if all(@numbers) %% 2 {  # %% is divisibility operator
        $logger.info("All numbers are even using 'all' junction");
    }
    
    # Junction 'none'
    if none(@numbers) < 0 {
        $logger.info("No negative numbers using 'none' junction");
    }
    say '';
    
    # Unicode operators and identifiers
    say "7. Unicode Support (Operators and Identifiers)";
    say '-' x 50;
    my $α = 1.5;  # Greek letters as identifiers
    my $β = 2.0;
    my $Σ = $α + $β;  # Sigma for sum
    
    $logger.info("α = $α, β = $β, Σ = $Σ");
    
    # Set operators with Unicode
    my $set1 = set(1, 2, 3, 4);
    my $set2 = set(3, 4, 5, 6);
    my $union = $set1 ∪ $set2;  # Unicode union operator
    my $intersect = $set1 ∩ $set2;  # Unicode intersection
    
    $logger.info("Set union (∪): {$union.keys.sort}");
    $logger.info("Set intersection (∩): {$intersect.keys.sort}");
    say '';
    
    # Functional operations
    say "8. Functional Programming with Lambdas";
    say '-' x 50;
    my @values = 1..10;
    
    # Map (using block)
    my @squares = @values.map: * ** 2;
    $logger.info("Squares: {@squares}");
    
    # Grep (filter)
    my @evens = @values.grep: * %% 2;
    $logger.info("Even numbers: {@evens}");
    
    # Reduce (using reduction metaoperator)
    my $sum = [+] @values;  # [+] is reduction metaoperator
    $logger.info("Sum: $sum");
    say '';
    
    # Whatever star (Raku idiom)
    say "9. Whatever Star (*) - Raku Idiom";
    say '-' x 50;
    my @data = <alice bob carol dave>;
    
    # * is a placeholder for the topic variable
    my @caps = @data.map: *.uc;
    $logger.info("Uppercase names: {@caps}");
    
    my @starts-with-a = @data.grep: *.starts-with('a');
    $logger.info("Names starting with 'a': {@starts-with-a}");
    say '';
    
    # Hyperoperators (vectorized operations)
    say "10. Hyperoperators (Vectorized Operations)";
    say '-' x 50;
    my @nums1 = (1, 2, 3, 4);
    my @nums2 = (10, 20, 30, 40);
    
    # >>+<< is a hyperoperator for element-wise addition
    my @sums = @nums1 >>+<< @nums2;
    $logger.info("Element-wise addition: {@sums}");
    
    # >>*<< for multiplication
    my @products = @nums1 >>*<< @nums2;
    $logger.info("Element-wise multiplication: {@products}");
    say '';
    
    # Pattern matching with smart matching
    say "11. Smart Matching (~~) for Pattern Matching";
    say '-' x 50;
    given "OpenCog AtomSpace" {
        when /Atom/ { $logger.info("Contains 'Atom'") }
        when Int    { $logger.info("Is an integer") }
        default     { $logger.info("Something else") }
    }
    say '';
    
    $logger.info("Cogutil demonstration complete!");
    say '=' x 70;
    say "Raku strengths demonstrated:";
    say "  ✓ Gradual typing (optional static types)";
    say "  ✓ Powerful grammar system for parsing";
    say "  ✓ Junctions for logical operations";
    say "  ✓ Unicode operators and identifiers";
    say "  ✓ Multiple dispatch";
    say "  ✓ Hyperoperators for vectorized ops";
    say "  ✓ Whatever star (*) idiom";
    say "  ✓ Smart matching (~~)";
    say '=' x 70;
}

# Run demonstration
demonstrate-cogutil();
