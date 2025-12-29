#!/usr/bin/env perl
# opencog-cogutil.pl
#
# OpenCog Cogutil - Perl Utility Library
# A collection of utility functions for OpenCog framework
#
# This single-file implementation demonstrates Perl's strengths:
# - Regular expressions for text processing
# - Hash tables (associative arrays) for data storage
# - References for complex data structures
# - CPAN-style modular design
# - Flexible syntax and TIMTOWTDI (There's More Than One Way To Do It)

use strict;
use warnings;
use v5.10;
use Time::HiRes qw(time);
use Data::Dumper;

# ===== Logger System =====
# Demonstrates: Blessed objects, hash references, method dispatch

package Logger {
    use strict;
    use warnings;
    
    my %LOG_LEVELS = (
        DEBUG => 0,
        INFO  => 1,
        WARN  => 2,
        ERROR => 3,
    );
    
    sub new {
        my ($class, $name, $level) = @_;
        $level //= 'INFO';
        
        my $self = {
            name  => $name,
            level => $level,
        };
        
        return bless $self, $class;
    }
    
    sub _current_timestamp {
        my @t = localtime(time);
        return sprintf("%02d:%02d:%02d", $t[2], $t[1], $t[0]);
    }
    
    sub _log {
        my ($self, $level, $message) = @_;
        
        if ($LOG_LEVELS{$level} >= $LOG_LEVELS{$self->{level}}) {
            my $timestamp = $self->_current_timestamp();
            say "[$timestamp] $level: $message";
        }
    }
    
    sub debug { $_[0]->_log('DEBUG', $_[1]) }
    sub info  { $_[0]->_log('INFO',  $_[1]) }
    sub warn  { $_[0]->_log('WARN',  $_[1]) }
    sub error { $_[0]->_log('ERROR', $_[1]) }
    
    sub set_level {
        my ($self, $level) = @_;
        $self->{level} = $level;
    }
}

# ===== Configuration Manager =====
# Demonstrates: Hash manipulation, file I/O, regex parsing

package Config {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        my $self = {
            data => {},
        };
        return bless $self, $class;
    }
    
    sub set {
        my ($self, $key, $value) = @_;
        $self->{data}{$key} = $value;
    }
    
    sub get {
        my ($self, $key, $default) = @_;
        $default //= '';
        return exists $self->{data}{$key} ? $self->{data}{$key} : $default;
    }
    
    sub has {
        my ($self, $key) = @_;
        return exists $self->{data}{$key};
    }
    
    sub load_from_file {
        my ($self, $filename) = @_;
        
        return 0 unless -e $filename;
        
        open(my $fh, '<', $filename) or return 0;
        
        while (my $line = <$fh>) {
            chomp $line;
            $line =~ s/^\s+|\s+$//g;  # Trim whitespace (Perl idiom)
            
            # Skip comments and empty lines
            next if $line =~ /^#/ || $line eq '';
            
            # Parse key=value pairs
            if ($line =~ /^([^=]+)=(.+)$/) {
                my ($key, $value) = ($1, $2);
                $key   =~ s/^\s+|\s+$//g;
                $value =~ s/^\s+|\s+$//g;
                $self->set($key, $value);
            }
        }
        
        close($fh);
        return 1;
    }
    
    sub dump {
        my ($self) = @_;
        for my $key (sort keys %{$self->{data}}) {
            say "$key = $self->{data}{$key}";
        }
    }
}

# ===== Timer Utility =====
# Demonstrates: High-resolution timing, closures

package Timer {
    use strict;
    use warnings;
    use Time::HiRes qw(time);
    
    sub new {
        my ($class, $name, $logger) = @_;
        my $self = {
            name       => $name,
            logger     => $logger,
            start_time => undef,
        };
        return bless $self, $class;
    }
    
    sub start {
        my ($self) = @_;
        $self->{start_time} = time();
    }
    
    sub stop {
        my ($self) = @_;
        return 0 unless defined $self->{start_time};
        
        my $elapsed = time() - $self->{start_time};
        if ($self->{logger}) {
            $self->{logger}->info(sprintf("%s completed in %.6f seconds", 
                                         $self->{name}, $elapsed));
        }
        return $elapsed;
    }
}

# ===== String Utilities =====
# Demonstrates: Regular expressions, array operations, map/grep

package StringUtils {
    use strict;
    use warnings;
    use Exporter 'import';
    our @EXPORT_OK = qw(str_split str_join to_lower to_upper trim 
                        camel_to_snake snake_to_camel reverse_str);
    
    sub str_split {
        my ($text, $delimiter) = @_;
        $delimiter //= ',';
        
        # Split and filter empty strings (using grep)
        return grep { $_ ne '' } 
               map { s/^\s+|\s+$//gr }  # Trim each element (non-destructive)
               split(quotemeta($delimiter), $text);
    }
    
    sub str_join {
        my ($strings, $delimiter) = @_;
        $delimiter //= ',';
        return join($delimiter, @$strings);
    }
    
    sub to_lower {
        my ($text) = @_;
        return lc($text);
    }
    
    sub to_upper {
        my ($text) = @_;
        return uc($text);
    }
    
    sub trim {
        my ($text) = @_;
        $text =~ s/^\s+|\s+$//g;
        return $text;
    }
    
    sub camel_to_snake {
        my ($text) = @_;
        $text =~ s/([A-Z])/_\L$1/g;  # Perl's case conversion in regex
        $text =~ s/^_//;
        return $text;
    }
    
    sub snake_to_camel {
        my ($text) = @_;
        $text =~ s/_([a-z])/\U$1/g;  # Uppercase after underscore
        return $text;
    }
    
    sub reverse_str {
        my ($text) = @_;
        return scalar reverse($text);  # Scalar context for string reverse
    }
}

# ===== Main Demonstration =====

package main;

use feature qw(say);

sub demonstrate_cogutil {
    say '=' x 70;
    say "OpenCog Cogutil - Perl Utility Library Demo";
    say "Showcasing Perl's strengths: Regex, hashes, TIMTOWTDI, text processing";
    say '=' x 70;
    say '';
    
    # Logger demonstration
    say "1. Logger Demonstration";
    say '-' x 50;
    my $logger = Logger->new("CogUtil", "INFO");
    $logger->info("Cogutil library initialized");
    $logger->debug("This debug message won't show (level too low)");
    $logger->warn("This is a warning message");
    $logger->error("This is an error message");
    
    $logger->set_level("DEBUG");
    $logger->debug("Now debug messages are visible");
    say '';
    
    # Config demonstration
    say "2. Configuration Manager";
    say '-' x 50;
    my $config = Config->new();
    $config->set("opencog.version", "1.0.0");
    $config->set("atomspace.enabled", "true");
    $config->set("cogserver.port", "17001");
    
    $logger->info("Configuration loaded:");
    $config->dump();
    say '';
    
    $logger->info("Port setting: " . $config->get("cogserver.port"));
    say '';
    
    # Timer demonstration
    say "3. Timer Utility";
    say '-' x 50;
    my $timer = Timer->new("Processing", $logger);
    $timer->start();
    $logger->info("Simulating some work...");
    
    # Simulate work using Perl idioms
    my $total = 0;
    $total += $_ for 1..1_000_000;
    
    $timer->stop();
    say '';
    
    # String utilities demonstration
    say "4. String Utilities with Regex";
    say '-' x 50;
    StringUtils->import(qw(str_split str_join to_lower to_upper trim 
                           camel_to_snake snake_to_camel reverse_str));
    
    $logger->info("String utilities demonstration:");
    my $text = "OpenCog,AtomSpace,CogServer,Cogutil";
    my @parts = str_split($text, ',');
    
    $logger->info("Split result:");
    say "  - $_" for @parts;
    
    my $joined = str_join(\@parts, " + ");
    $logger->info("Joined: $joined");
    
    $logger->info("Uppercase: " . to_upper("opencog rocks"));
    $logger->info("Lowercase: " . to_lower("OPENCOG ROCKS"));
    $logger->info("Trimmed: '" . trim("  spaced out  ") . "'");
    $logger->info("Reversed: " . reverse_str("Perl"));
    
    # Perl-specific: Case conversions with regex
    $logger->info("camelCase → snake_case: " . camel_to_snake("myVariableName"));
    $logger->info("snake_case → camelCase: " . snake_to_camel("my_variable_name"));
    say '';
    
    # Regular expression demonstration
    say "5. Regular Expression Power (Perl's Strength)";
    say '-' x 50;
    my $sample = "The AtomSpace uses Nodes and Links in OpenCog";
    
    # Pattern matching
    if ($sample =~ /AtomSpace/) {
        $logger->info("Found 'AtomSpace' in text");
    }
    
    # Capture groups
    if ($sample =~ /uses (\w+) and (\w+)/) {
        $logger->info("Captured: '$1' and '$2'");
    }
    
    # Substitution (non-destructive with /r modifier)
    my $modified = $sample =~ s/OpenCog/Perl/r;
    $logger->info("Substituted: $modified");
    
    # Word extraction (using match in list context)
    my @words = $sample =~ /\b([A-Z]\w+)\b/g;
    $logger->info("Capitalized words: " . join(", ", @words));
    say '';
    
    # Hash manipulation (Perl's associative arrays)
    say "6. Hash Operations (Associative Arrays)";
    say '-' x 50;
    my %atoms = (
        'Socrates' => 'human',
        'human'    => 'mortal',
        'mortal'   => 'being',
    );
    
    $logger->info("Hash contents:");
    say "  $_ => $atoms{$_}" for sort keys %atoms;
    
    # Hash slicing (Perl idiom)
    my @values = @atoms{qw(Socrates human)};
    $logger->info("Hash slice: " . join(", ", @values));
    say '';
    
    # Array and list operations
    say "7. Array Operations and List Processing";
    say '-' x 50;
    my @numbers = 1..10;
    
    # Map (transform)
    my @squares = map { $_ * $_ } @numbers;
    $logger->info("Squares: " . join(", ", @squares));
    
    # Grep (filter)
    my @evens = grep { $_ % 2 == 0 } @numbers;
    $logger->info("Even numbers: " . join(", ", @evens));
    
    # Reduce (using List::Util would be typical, but showing manual)
    my $sum = 0;
    $sum += $_ for @numbers;
    $logger->info("Sum: $sum");
    say '';
    
    # Context sensitivity (Perl's unique feature)
    say "8. Context Sensitivity (Perl Unique)";
    say '-' x 50;
    my @list = (1, 2, 3, 4, 5);
    
    # List context
    my @copy = @list;
    $logger->info("List context: " . join(", ", @copy));
    
    # Scalar context (gets length)
    my $count = @list;
    $logger->info("Scalar context (length): $count");
    say '';
    
    # Autovivification (Perl feature)
    say "9. Autovivification (Perl Magic)";
    say '-' x 50;
    my %deep;
    $deep{level1}{level2}{level3} = "value";  # Auto-creates intermediate hashes
    $logger->info("Deep hash value: " . $deep{level1}{level2}{level3});
    say '';
    
    $logger->info("Cogutil demonstration complete!");
    say '=' x 70;
    say "Perl strengths demonstrated:";
    say "  ✓ Powerful regular expressions (built into language)";
    say "  ✓ Hash tables (associative arrays) as first-class citizens";
    say "  ✓ Flexible syntax (TIMTOWTDI)";
    say "  ✓ Context sensitivity (list vs scalar)";
    say "  ✓ Text processing capabilities";
    say "  ✓ Autovivification for easy data structures";
    say "  ✓ Map/grep for functional programming";
    say '=' x 70;
}

# Run demonstration
demonstrate_cogutil();
