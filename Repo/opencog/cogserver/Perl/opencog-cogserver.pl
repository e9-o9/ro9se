#!/usr/bin/env perl
# opencog-cogserver.pl
#
# OpenCog CogServer - Network Server for AtomSpace Access in Perl
#
# This single-file implementation demonstrates Perl's strengths:
# - Hash-based command registry
# - Anonymous subroutines (closures)
# - Regular expressions for command parsing
# - CPAN-style modular design
# - Flexible I/O handling

use strict;
use warnings;
use v5.10;
use feature qw(say);
use Time::HiRes qw(time);

# ===== Command Result =====
# Demonstrates: Simple blessed hash for result objects

package CommandResult {
    use strict;
    use warnings;
    
    sub new {
        my ($class, $success, $message) = @_;
        my $self = {
            success => $success,
            message => $message,
        };
        return bless $self, $class;
    }
    
    sub success { $_[0]->{success} }
    sub message { $_[0]->{message} }
    
    sub to_string { $_[0]->{message} }
}

# ===== Command Registry =====
# Demonstrates: Hash of code references, closures

package CommandRegistry {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        my $self = {
            commands     => {},  # command_name => code_ref
            descriptions => {},  # command_name => description
        };
        return bless $self, $class;
    }
    
    sub register {
        my ($self, $name, $handler, $description) = @_;
        $description //= "No description";
        
        $self->{commands}{$name}     = $handler;
        $self->{descriptions}{$name} = $description;
    }
    
    sub execute {
        my ($self, $name, $args) = @_;
        $args //= [];
        
        unless (exists $self->{commands}{$name}) {
            return CommandResult->new(0, "Unknown command: $name");
        }
        
        # Execute command handler (code reference)
        eval {
            return $self->{commands}{$name}->($args);
        };
        
        if ($@) {
            return CommandResult->new(0, "Error executing $name: $@");
        }
        
        return $self->{commands}{$name}->($args);
    }
    
    sub list_commands {
        my ($self) = @_;
        my @commands;
        for my $name (sort keys %{$self->{commands}}) {
            push @commands, [$name, $self->{descriptions}{$name}];
        }
        return \@commands;
    }
    
    sub has_command {
        my ($self, $name) = @_;
        return exists $self->{commands}{$name};
    }
}

# ===== Session Management =====
# Demonstrates: Time tracking, mutable state

package Session {
    use strict;
    use warnings;
    use Time::HiRes qw(time);
    
    sub new {
        my ($class, $id) = @_;
        my $self = {
            id            => $id,
            created_at    => time(),
            data          => {},
            authenticated => 0,
        };
        return bless $self, $class;
    }
    
    sub set {
        my ($self, $key, $value) = @_;
        $self->{data}{$key} = $value;
    }
    
    sub get {
        my ($self, $key, $default) = @_;
        return exists $self->{data}{$key} ? $self->{data}{$key} : $default;
    }
    
    sub age {
        my ($self) = @_;
        return time() - $self->{created_at};
    }
    
    sub to_string {
        my ($self) = @_;
        return sprintf("Session(%s, age=%.1fs, auth=%s)", 
                      $self->{id}, 
                      $self->age(), 
                      $self->{authenticated} ? "true" : "false");
    }
    
    sub id            { $_[0]->{id} }
    sub authenticated { $_[0]->{authenticated} }
    sub set_authenticated { $_[0]->{authenticated} = $_[1] }
}

# ===== CogServer =====
# Demonstrates: Composition, encapsulation

package CogServer {
    use strict;
    use warnings;
    
    sub new {
        my ($class, $name) = @_;
        $name //= "OpenCog";
        
        my $self = {
            name     => $name,
            registry => CommandRegistry->new(),
            sessions => {},
        };
        return bless $self, $class;
    }
    
    sub register_command {
        my ($self, $name, $handler, $description) = @_;
        $self->{registry}->register($name, $handler, $description);
    }
    
    sub execute {
        my ($self, $name, $args) = @_;
        return $self->{registry}->execute($name, $args);
    }
    
    sub list_commands {
        my ($self) = @_;
        return $self->{registry}->list_commands();
    }
    
    # Session operations
    sub create_session {
        my ($self, $session_id) = @_;
        my $session = Session->new($session_id);
        $self->{sessions}{$session_id} = $session;
        return $session;
    }
    
    sub get_session {
        my ($self, $session_id) = @_;
        return $self->{sessions}{$session_id};
    }
    
    sub remove_session {
        my ($self, $session_id) = @_;
        delete $self->{sessions}{$session_id};
    }
    
    sub session_count {
        my ($self) = @_;
        return scalar keys %{$self->{sessions}};
    }
    
    sub name { $_[0]->{name} }
}

# ===== Built-in Commands =====
# Demonstrates: Closures capturing server state

sub register_builtin_commands {
    my ($server) = @_;
    
    # Help command (closure capturing $server)
    $server->register_command('help', sub {
        my ($args) = @_;
        my $cmds = $server->list_commands();
        my $output = "Available commands:\n";
        for my $cmd (@$cmds) {
            $output .= sprintf("  %-15s - %s\n", $cmd->[0], $cmd->[1]);
        }
        return CommandResult->new(1, $output);
    }, "Display available commands");
    
    # Info command
    $server->register_command('info', sub {
        my ($args) = @_;
        my $info = sprintf(
            "Server: %s\nCommands: %d\nSessions: %d\n",
            $server->name(),
            scalar @{$server->list_commands()},
            $server->session_count()
        );
        return CommandResult->new(1, $info);
    }, "Display server information");
    
    # Echo command
    $server->register_command('echo', sub {
        my ($args) = @_;
        return CommandResult->new(1, join(" ", @$args));
    }, "Echo back the arguments");
    
    # Version command
    $server->register_command('version', sub {
        my ($args) = @_;
        return CommandResult->new(1, "OpenCog Perl Implementation v1.0.0");
    }, "Display version information");
    
    # Status command
    $server->register_command('status', sub {
        my ($args) = @_;
        my $status = sprintf(
            "Status: Running\nSessions: %d\n",
            $server->session_count()
        );
        return CommandResult->new(1, $status);
    }, "Display server status");
    
    # List command
    $server->register_command('list', sub {
        my ($args) = @_;
        my $cmds = $server->list_commands();
        my @names = map { $_->[0] } @$cmds;
        return CommandResult->new(1, join(", ", @names));
    }, "List all commands");
    
    # Uptime command
    $server->register_command('uptime', sub {
        my ($args) = @_;
        return CommandResult->new(1, "Uptime: " . time() . " seconds");
    }, "Display server uptime");
}

# ===== Interactive Shell =====
# Demonstrates: REPL implementation, regex parsing

sub parse_command_line {
    my ($line) = @_;
    
    # Trim whitespace
    $line =~ s/^\s+|\s+$//g;
    
    return ('', []) if $line eq '';
    
    # Parse command and arguments
    my @parts = split(/\s+/, $line);
    my $cmd = shift @parts;
    
    return ($cmd, \@parts);
}

sub run_interactive_shell {
    my ($server) = @_;
    
    say "OpenCog CogServer - Interactive Shell";
    say "Type 'help' for available commands, 'exit' to quit";
    say '';
    
    while (1) {
        print "opencog> ";
        my $line = <STDIN>;
        
        # Handle EOF
        last unless defined $line;
        
        chomp $line;
        
        # Skip empty lines
        next if $line =~ /^\s*$/;
        
        # Exit commands
        if ($line =~ /^(exit|quit)$/i) {
            say "Goodbye!";
            last;
        }
        
        # Parse and execute
        my ($cmd, $args) = parse_command_line($line);
        my $result = $server->execute($cmd, $args);
        say $result->to_string();
    }
}

# ===== Regex-Based Command Parsing =====
# Demonstrates: Sophisticated regex patterns

sub parse_command_with_regex {
    my ($line) = @_;
    
    $line =~ s/^\s+|\s+$//g;
    
    # Pattern matching with named captures
    if ($line =~ /^help$/i) {
        return ('help', []);
    }
    elsif ($line =~ /^info$/i) {
        return ('info', []);
    }
    elsif ($line =~ /^echo\s+(.+)$/i) {
        my $text = $1;
        return ('echo', [split(/\s+/, $text)]);
    }
    elsif ($line =~ /^(\w+)\s*(.*)$/) {
        my ($cmd, $args_str) = ($1, $2);
        my @args = split(/\s+/, $args_str);
        return ($cmd, \@args);
    }
    else {
        return ('unknown', [$line]);
    }
}

# ===== Main Demonstration =====

package main;

sub demonstrate_cogserver {
    say '=' x 70;
    say "OpenCog CogServer - Network Server in Perl";
    say "Showcasing: Closures, hash-based registry, regex parsing, REPL";
    say '=' x 70;
    say '';
    
    # Create server
    say "1. Creating CogServer";
    say '-' x 50;
    my $server = CogServer->new("TestServer");
    say "Server created: " . $server->name();
    say '';
    
    # Register built-in commands
    say "2. Registering Built-in Commands";
    say '-' x 50;
    register_builtin_commands($server);
    say "Registered " . scalar(@{$server->list_commands()}) . " commands";
    say '';
    
    # List commands
    say "3. Available Commands";
    say '-' x 50;
    for my $cmd (@{$server->list_commands()}) {
        printf("  %-15s - %s\n", $cmd->[0], $cmd->[1]);
    }
    say '';
    
    # Execute commands
    say "4. Executing Commands";
    say '-' x 50;
    my @test_commands = (
        ['version', []],
        ['echo', ['Hello', 'World']],
        ['info', []],
        ['invalid', []],
    );
    
    for my $cmd (@test_commands) {
        my ($name, $args) = @$cmd;
        say "Executing: $name " . join(" ", @$args);
        my $result = $server->execute($name, $args);
        say "  Result: " . ($result->success() ? "SUCCESS" : "FAILURE");
        say "  Message: " . $result->to_string();
        say '';
    }
    
    # Session management
    say "5. Session Management";
    say '-' x 50;
    my $sess1 = $server->create_session("session-001");
    my $sess2 = $server->create_session("session-002");
    
    $sess1->set("username", "alice");
    $sess1->set("language", "perl");
    $sess2->set("username", "bob");
    
    say "Created " . $server->session_count() . " sessions";
    say "Session 1: " . $sess1->to_string();
    say "  username: " . $sess1->get("username");
    say "  language: " . $sess1->get("language");
    say "Session 2: " . $sess2->to_string();
    say "  username: " . $sess2->get("username");
    say '';
    
    # Custom command registration
    say "6. Registering Custom Command with Closure";
    say '-' x 50;
    $server->register_command('greet', sub {
        my ($args) = @_;
        my $name = @$args ? $args->[0] : "World";
        return CommandResult->new(1, "Hello, $name!");
    }, "Greet a user");
    
    my $greet_result = $server->execute('greet', ['Perl']);
    say "Custom command result: " . $greet_result->to_string();
    say '';
    
    # Regex parsing demonstration
    say "7. Regex-Based Command Parsing";
    say '-' x 50;
    my @test_inputs = ('help', 'info', 'echo testing 123', 'unknown cmd');
    for my $input (@test_inputs) {
        say "Input: '$input'";
        my ($cmd, $args) = parse_command_with_regex($input);
        say "  Parsed: cmd='$cmd', args=[" . join(", ", @$args) . "]";
    }
    say '';
    
    # Anonymous subroutine demonstration
    say "8. Anonymous Subroutines (Perl Closures)";
    say '-' x 50;
    my $counter = 0;
    my $increment = sub { ++$counter };
    my $get_count = sub { $counter };
    
    $increment->();
    $increment->();
    say "Counter value: " . $get_count->();
    say "Closures share lexical scope";
    say '';
    
    # Hash slicing demonstration
    say "9. Hash Operations (Perl Idioms)";
    say '-' x 50;
    my %user_data = (
        alice => 'perl',
        bob   => 'python',
        carol => 'ruby',
    );
    
    # Hash slice
    my @langs = @user_data{qw(alice bob)};
    say "Hash slice: " . join(", ", @langs);
    
    # Exists check
    say "Has 'alice': " . (exists $user_data{alice} ? "YES" : "NO");
    
    # Keys/values
    say "Users: " . join(", ", sort keys %user_data);
    say '';
    
    # Status command
    say "10. Server Status";
    say '-' x 50;
    my $status_result = $server->execute('status', []);
    say $status_result->to_string();
    say '';
    
    say '=' x 70;
    say "Perl CogServer strengths:";
    say "  ✓ Hash-based command registry (O(1) lookup)";
    say "  ✓ Anonymous subroutines (closures)";
    say "  ✓ Regular expressions for parsing";
    say "  ✓ Flexible I/O handling";
    say "  ✓ Code references as first-class values";
    say "  ✓ TIMTOWTDI (multiple approaches)";
    say "  ✓ Simple but powerful OOP (blessed refs)";
    say '=' x 70;
    say '';
    
    # Interactive mode
    say "Enter interactive mode? (y/n)";
    my $response = <STDIN>;
    chomp $response if defined $response;
    
    if (defined $response && $response =~ /^y$/i) {
        run_interactive_shell($server);
    }
}

# Run demonstration
demonstrate_cogserver();
