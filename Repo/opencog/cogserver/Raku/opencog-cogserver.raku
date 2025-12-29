#!/usr/bin/env raku
# opencog-cogserver.raku - Network Server in Raku
use v6;

class CommandResult {
    has Bool $.success;
    has Str $.message;
    method Str { $.message }
}

class CommandRegistry {
    has %.commands;  # name => { handler => &code, desc => Str }
    
    method register(Str $name, &handler, Str $desc = '') {
        %!commands{$name} = { handler => &handler, desc => $desc };
    }
    
    method execute(Str $name, @args) returns CommandResult {
        with %!commands{$name} {
            try return .<handler>(@args);
            CATCH { default { return CommandResult.new(success => False, message => "Error: $_") } }
        } else {
            return CommandResult.new(success => False, message => "Unknown command: $name");
        }
    }
    
    method list { %!commands.keys.sort }
    method has(Str $name) { %!commands{$name}:exists }
}

class Session {
    has Str $.id;
    has Instant $.created-at = now;
    has %.data;
    has Bool $.authenticated is rw = False;
    
    method set(Str $k, $v) { %!data{$k} = $v }
    method get(Str $k, $default = Nil) { %!data{$k} // $default }
    method age { now - $!created-at }
    method Str { "Session($.id, age={self.age.fmt('%.1f')}s)" }
}

class CogServer {
    has Str $.name;
    has CommandRegistry $.registry .= new;
    has %.sessions;
    
    method register-command(Str $n, &h, Str $d = '') { $!registry.register($n, &h, $d) }
    method execute(Str $n, @a) { $!registry.execute($n, @a) }
    method list-commands { $!registry.list }
    
    method create-session(Str $id) { %!sessions{$id} = Session.new(:$id) }
    method get-session(Str $id) { %!sessions{$id} }
    method session-count { %!sessions.elems }
}

sub register-builtin-commands($server) {
    $server.register-command('help', sub (@a) {
        CommandResult.new(success => True, message => "Commands: {$server.list-commands.join(', ')}")
    }, 'Show commands');
    
    $server.register-command('info', sub (@a) {
        CommandResult.new(success => True, message => "Server: {$server.name}, Sessions: {$server.session-count}")
    }, 'Server info');
    
    $server.register-command('echo', sub (@a) {
        CommandResult.new(success => True, message => @a.join(' '))
    }, 'Echo args');
    
    $server.register-command('version', sub (@a) {
        CommandResult.new(success => True, message => "OpenCog Raku v1.0.0")
    }, 'Show version');
}

sub demonstrate-cogserver {
    say '=' x 70;
    say "OpenCog CogServer - Raku Implementation";
    say '=' x 70;
    
    my $server = CogServer.new(name => "TestServer");
    register-builtin-commands($server);
    
    say "\n1. Commands: {$server.list-commands.join(', ')}";
    
    say "\n2. Executing Commands";
    say $server.execute('version', []);
    say $server.execute('echo', <Hello Raku>);
    
    say "\n3. Session Management";
    my $sess = $server.create-session("s001");
    $sess.set('user', 'alice');
    say $sess;
    
    say "\nRaku CogServer: Typed attributes, gradual typing, Unicode support";
}

demonstrate-cogserver();
