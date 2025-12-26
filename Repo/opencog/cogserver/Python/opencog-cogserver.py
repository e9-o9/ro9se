#!/usr/bin/env python3
"""
opencog-cogserver.py

OpenCog CogServer - Network Server for AtomSpace Access in Python

This single-file implementation demonstrates Python's strengths:
- Command pattern with first-class functions
- Duck typing for flexible command system
- Built-in readline for interactive shell
- Decorator-based command registration
"""

import cmd
import sys
from typing import Dict, List, Callable, Any, Optional
from functools import wraps
from datetime import datetime
import time


class CommandResult:
    """
    Result of command execution
    Demonstrates: Simple data class
    """
    def __init__(self, success: bool, message: str):
        self.success = success
        self.message = message
    
    def __str__(self) -> str:
        return self.message
    
    def __bool__(self) -> bool:
        return self.success


class CommandRegistry:
    """
    Command registry using decorators
    Demonstrates: Decorator pattern, first-class functions
    """
    def __init__(self):
        self.commands: Dict[str, Callable] = {}
        self.descriptions: Dict[str, str] = {}
    
    def register(self, name: str, description: str = ""):
        """Decorator to register commands"""
        def decorator(func: Callable) -> Callable:
            self.commands[name] = func
            self.descriptions[name] = description or func.__doc__ or "No description"
            @wraps(func)
            def wrapper(*args, **kwargs):
                return func(*args, **kwargs)
            return wrapper
        return decorator
    
    def execute(self, name: str, *args, **kwargs) -> CommandResult:
        """Execute a command by name"""
        if name not in self.commands:
            return CommandResult(False, f"Unknown command: {name}")
        
        try:
            return self.commands[name](*args, **kwargs)
        except Exception as e:
            return CommandResult(False, f"Error executing {name}: {str(e)}")
    
    def list_commands(self) -> List[tuple]:
        """List all registered commands"""
        return [(name, self.descriptions[name]) 
                for name in sorted(self.commands.keys())]


class Session:
    """
    Client session with state
    Demonstrates: State management, property access
    """
    def __init__(self, session_id: str):
        self.session_id = session_id
        self.created_at = datetime.now()
        self.data: Dict[str, Any] = {}
        self.authenticated = False
    
    def set(self, key: str, value: Any) -> None:
        """Set session data"""
        self.data[key] = value
    
    def get(self, key: str, default: Any = None) -> Any:
        """Get session data"""
        return self.data.get(key, default)
    
    @property
    def age(self) -> float:
        """Session age in seconds"""
        return (datetime.now() - self.created_at).total_seconds()
    
    def __str__(self) -> str:
        return f"Session({self.session_id}, age={self.age:.1f}s)"


class CogServerShell(cmd.Cmd):
    """
    Interactive shell using Python's cmd module
    Demonstrates: Inheritance from cmd.Cmd, interactive CLI
    """
    
    intro = """
╔═══════════════════════════════════════════════════════════════════╗
║         OpenCog CogServer - Python Interactive Shell              ║
║         Type 'help' or '?' to list commands                       ║
╚═══════════════════════════════════════════════════════════════════╝
"""
    prompt = 'cogserver> '
    
    def __init__(self, registry: CommandRegistry):
        super().__init__()
        self.registry = registry
        self.atom_count = 42  # Simulated
        self.start_time = time.time()
        self.running = True
    
    # Built-in commands using cmd.Cmd pattern
    
    def do_help(self, arg):
        """List available commands"""
        if arg:
            # Help for specific command
            if arg in self.registry.commands:
                print(f"{arg}: {self.registry.descriptions[arg]}")
            else:
                print(f"Unknown command: {arg}")
        else:
            # List all commands
            print("\nAvailable commands:")
            print("-" * 50)
            for name, desc in self.registry.list_commands():
                print(f"  {name:<15} - {desc}")
            print()
    
    def do_status(self, arg):
        """Display server status"""
        uptime = time.time() - self.start_time
        print(f"\nCogServer Status:")
        print(f"  Uptime: {uptime:.1f} seconds")
        print(f"  AtomSpace size: {self.atom_count} atoms")
        print(f"  Server: Running")
        print()
    
    def do_list(self, arg):
        """List atoms in AtomSpace"""
        print("\nSimulated AtomSpace contents:")
        print("  (ConceptNode \"human\")")
        print("  (ConceptNode \"mortal\")")
        print("  (ConceptNode \"Socrates\")")
        print("  (InheritanceLink")
        print("    (ConceptNode \"Socrates\")")
        print("    (ConceptNode \"human\"))")
        print()
    
    def do_addnode(self, arg):
        """Add a node: addnode <type> <name>"""
        args = arg.split()
        if len(args) < 2:
            print("Usage: addnode <type> <name>")
            return
        
        node_type, name = args[0], args[1]
        print(f"Added node: ({node_type} \"{name}\")")
        self.atom_count += 1
    
    def do_query(self, arg):
        """Query atoms by pattern"""
        if not arg:
            print("Usage: query <pattern>")
            return
        
        print(f"\nQuery results for '{arg}':")
        print(f"  Found 3 matching atoms")
        print(f"  (ConceptNode \"{arg}\")")
        print()
    
    def do_echo(self, arg):
        """Echo back the arguments"""
        print(arg)
    
    def do_eval(self, arg):
        """Evaluate Python expression (demonstrates metaprogramming)"""
        if not arg:
            print("Usage: eval <expression>")
            return
        
        try:
            result = eval(arg)
            print(f"Result: {result}")
        except Exception as e:
            print(f"Error: {e}")
    
    def do_exit(self, arg):
        """Exit the shell"""
        print("Shutting down CogServer...")
        return True
    
    def do_quit(self, arg):
        """Exit the shell"""
        return self.do_exit(arg)
    
    def do_EOF(self, arg):
        """Handle Ctrl+D"""
        print()
        return self.do_exit(arg)
    
    def emptyline(self):
        """Don't repeat last command on empty line"""
        pass
    
    def default(self, line):
        """Handle unknown commands"""
        cmd_name = line.split()[0] if line.split() else ""
        if cmd_name in self.registry.commands:
            result = self.registry.execute(cmd_name, *line.split()[1:])
            print(result)
        else:
            print(f"Unknown command: {cmd_name}")
            print("Type 'help' for available commands")


class CogServer:
    """
    Main CogServer class
    Demonstrates: Composition, delegation
    """
    def __init__(self, port: int = 17001):
        self.port = port
        self.registry = CommandRegistry()
        self.sessions: Dict[str, Session] = {}
        self._setup_commands()
    
    def _setup_commands(self):
        """Setup additional commands using registry"""
        
        @self.registry.register("info", "Display server information")
        def info_command(*args):
            info = f"""
CogServer Information:
  Port: {self.port}
  Active sessions: {len(self.sessions)}
  Commands registered: {len(self.registry.commands)}
"""
            return CommandResult(True, info)
        
        @self.registry.register("sessions", "List active sessions")
        def sessions_command(*args):
            if not self.sessions:
                return CommandResult(True, "No active sessions")
            
            result = "Active sessions:\n"
            for session in self.sessions.values():
                result += f"  {session}\n"
            return CommandResult(True, result)
    
    def create_session(self, session_id: str) -> Session:
        """Create a new session"""
        session = Session(session_id)
        self.sessions[session_id] = session
        return session
    
    def start_interactive(self):
        """Start interactive shell"""
        print(f"Starting CogServer on port {self.port}")
        print(f"Python implementation showcasing:")
        print("  ✓ cmd.Cmd module for interactive shells")
        print("  ✓ Decorator-based command registration")
        print("  ✓ First-class functions")
        print("  ✓ Duck typing for flexibility")
        print()
        
        # Create demo sessions
        self.create_session("client-001").set("username", "alice")
        self.create_session("client-002").set("username", "bob")
        
        # Start the shell
        shell = CogServerShell(self.registry)
        shell.cmdloop()


def demonstrate_features():
    """Demonstrate Python-specific features"""
    print("=" * 70)
    print("OpenCog CogServer - Python Implementation")
    print("=" * 70)
    print()
    
    print("1. Decorator-Based Command Registration")
    print("-" * 50)
    
    registry = CommandRegistry()
    
    @registry.register("greet", "Greet the user")
    def greet_command(name: str = "World"):
        return CommandResult(True, f"Hello, {name}!")
    
    result = registry.execute("greet", "Alice")
    print(f"Command result: {result}")
    print()
    
    print("2. Session Management")
    print("-" * 50)
    
    session = Session("demo-session")
    session.set("username", "alice")
    session.set("role", "admin")
    session.authenticated = True
    
    print(f"Session: {session}")
    print(f"Username: {session.get('username')}")
    print(f"Authenticated: {session.authenticated}")
    print()
    
    print("3. Lambda Functions and Functional Programming")
    print("-" * 50)
    
    # Commands can be lambdas too
    registry.commands["add"] = lambda a, b: CommandResult(True, f"Sum: {int(a) + int(b)}")
    result = registry.execute("add", "5", "7")
    print(f"Add command: {result}")
    print()
    
    print("4. List Comprehension for Data Processing")
    print("-" * 50)
    
    commands = [name for name in registry.commands.keys()]
    print(f"All commands: {commands}")
    
    # Filter commands by length
    short_commands = [cmd for cmd in commands if len(cmd) < 5]
    print(f"Short commands: {short_commands}")
    print()
    
    print("=" * 70)
    print("Starting interactive shell...")
    print("=" * 70)
    print()


def main():
    """Main entry point"""
    demonstrate_features()
    
    # Create and start server
    server = CogServer(port=17001)
    server.start_interactive()


if __name__ == "__main__":
    main()
