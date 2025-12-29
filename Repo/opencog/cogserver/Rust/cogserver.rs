/*
 * cogserver.rs
 * 
 * OpenCog CogServer - Rust Implementation
 * Network server and REPL interface for OpenCog
 * 
 * This implementation demonstrates Rust's strengths:
 * - Safe concurrency with threads and channels
 * - Ownership-based resource management
 * - Pattern matching for command dispatch
 * - Zero-cost abstractions
 * - Error handling with Result type
 */

use std::collections::HashMap;
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::thread;

// ===== Command System =====

// Command handler type: takes args, returns Result with response string
type CommandHandler = Arc<dyn Fn(&[String]) -> Result<String, String> + Send + Sync>;

pub struct Command {
    name: String,
    description: String,
    handler: CommandHandler,
}

impl Command {
    pub fn new<F>(name: &str, description: &str, handler: F) -> Self
    where
        F: Fn(&[String]) -> Result<String, String> + Send + Sync + 'static,
    {
        Command {
            name: name.to_string(),
            description: description.to_string(),
            handler: Arc::new(handler),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn description(&self) -> &str {
        &self.description
    }

    pub fn execute(&self, args: &[String]) -> Result<String, String> {
        (self.handler)(args)
    }
}

// ===== Module Interface =====

pub trait CogModule: Send + Sync {
    fn init(&mut self, server: &CogServer) -> Result<(), String>;
    fn get_commands(&self) -> Vec<Command>;
    fn shutdown(&mut self) -> Result<(), String>;
}

// ===== CogServer Implementation =====

pub struct CogServer {
    address: String,
    commands: Arc<Mutex<HashMap<String, Command>>>,
    modules: Arc<Mutex<HashMap<String, Box<dyn CogModule>>>>,
    running: Arc<Mutex<bool>>,
}

impl CogServer {
    pub fn new(address: &str) -> Self {
        let server = CogServer {
            address: address.to_string(),
            commands: Arc::new(Mutex::new(HashMap::new())),
            modules: Arc::new(Mutex::new(HashMap::new())),
            running: Arc::new(Mutex::new(false)),
        };

        // Register built-in commands
        server.register_builtin_commands();

        server
    }

    // Register all built-in commands
    fn register_builtin_commands(&self) {
        // Clone Arc for closures
        let commands_clone = Arc::clone(&self.commands);
        self.register_command(Command::new(
            "help",
            "Display available commands",
            move |_args| {
                let cmds = commands_clone.lock().unwrap();
                if cmds.is_empty() {
                    return Ok("No commands available".to_string());
                }

                let mut result = String::from("Available commands:\n");
                let mut cmd_list: Vec<_> = cmds.iter().collect();
                cmd_list.sort_by_key(|(name, _)| *name);
                
                for (name, cmd) in cmd_list {
                    result.push_str(&format!("  {:15} - {}\n", name, cmd.description()));
                }
                Ok(result)
            },
        ));

        let modules_clone = Arc::clone(&self.modules);
        self.register_command(Command::new(
            "list",
            "List loaded modules",
            move |_args| {
                let modules = modules_clone.lock().unwrap();
                if modules.is_empty() {
                    return Ok("No modules loaded".to_string());
                }

                let mut result = String::from("Loaded modules:\n");
                let mut mod_list: Vec<_> = modules.keys().collect();
                mod_list.sort();
                
                for name in mod_list {
                    result.push_str(&format!("  - {}\n", name));
                }
                Ok(result)
            },
        ));

        let running_clone = Arc::clone(&self.running);
        self.register_command(Command::new(
            "shutdown",
            "Shutdown the server",
            move |_args| {
                println!("[CogServer] Shutdown command received");
                *running_clone.lock().unwrap() = false;
                Ok("Shutting down server...".to_string())
            },
        ));

        let addr = self.address.clone();
        let commands_clone2 = Arc::clone(&self.commands);
        let modules_clone2 = Arc::clone(&self.modules);
        self.register_command(Command::new(
            "info",
            "Display server information",
            move |_args| {
                let cmds = commands_clone2.lock().unwrap();
                let modules = modules_clone2.lock().unwrap();
                
                let result = format!(
                    "CogServer Information:\n\
                     Address: {}\n\
                     Commands: {}\n\
                     Modules: {}\n",
                    addr,
                    cmds.len(),
                    modules.len()
                );
                Ok(result)
            },
        ));

        self.register_command(Command::new(
            "echo",
            "Echo back arguments",
            |args| {
                if args.is_empty() {
                    return Err("Usage: echo <message>".to_string());
                }
                Ok(args.join(" "))
            },
        ));
    }

    // Register a command
    pub fn register_command(&self, cmd: Command) {
        let mut commands = self.commands.lock().unwrap();
        commands.insert(cmd.name().to_string(), cmd);
    }

    // Unregister a command
    pub fn unregister_command(&self, name: &str) {
        let mut commands = self.commands.lock().unwrap();
        commands.remove(name);
    }

    // Load a module
    pub fn load_module(&self, name: &str, mut module: Box<dyn CogModule>) -> Result<(), String> {
        // Check if already loaded
        {
            let modules = self.modules.lock().unwrap();
            if modules.contains_key(name) {
                return Err(format!("Module {} already loaded", name));
            }
        }

        // Initialize module
        module.init(self)?;

        // Register module commands
        for cmd in module.get_commands() {
            self.register_command(cmd);
        }

        // Store module
        let mut modules = self.modules.lock().unwrap();
        modules.insert(name.to_string(), module);

        Ok(())
    }

    // Unload a module
    pub fn unload_module(&self, name: &str) -> Result<(), String> {
        let mut module = {
            let mut modules = self.modules.lock().unwrap();
            modules
                .remove(name)
                .ok_or_else(|| format!("Module {} not loaded", name))?
        };

        // Unregister module commands
        for cmd in module.get_commands() {
            self.unregister_command(cmd.name());
        }

        // Shutdown module
        module.shutdown()?;

        Ok(())
    }

    // Start the server
    pub fn start(&self) -> Result<(), String> {
        {
            let mut running = self.running.lock().unwrap();
            if *running {
                return Err("Server already running".to_string());
            }
            *running = true;
        }

        let listener = TcpListener::bind(&self.address)
            .map_err(|e| format!("Failed to bind to {}: {}", self.address, e))?;

        println!("[CogServer] Listening on {}", self.address);

        // Accept connections
        for stream in listener.incoming() {
            // Check if should keep running
            {
                let running = self.running.lock().unwrap();
                if !*running {
                    break;
                }
            }

            match stream {
                Ok(stream) => {
                    let commands = Arc::clone(&self.commands);
                    let running = Arc::clone(&self.running);

                    // Handle client in separate thread
                    thread::spawn(move || {
                        if let Err(e) = handle_client(stream, commands, running) {
                            eprintln!("[CogServer] Client error: {}", e);
                        }
                    });
                }
                Err(e) => {
                    eprintln!("[CogServer] Connection error: {}", e);
                }
            }
        }

        println!("[CogServer] Server stopped");
        Ok(())
    }

    // Stop the server
    pub fn stop(&self) {
        println!("[CogServer] Stopping server...");
        let mut running = self.running.lock().unwrap();
        *running = false;
    }
}

// ===== Client Handler =====

fn handle_client(
    stream: TcpStream,
    commands: Arc<Mutex<HashMap<String, Command>>>,
    running: Arc<Mutex<bool>>,
) -> Result<(), String> {
    let mut reader = BufReader::new(stream.try_clone().map_err(|e| e.to_string())?);
    let mut writer = stream;

    // Send welcome message
    writeln!(writer, "Welcome to CogServer (Rust)")
        .map_err(|e| format!("Write error: {}", e))?;
    writeln!(writer, "Type 'help' for available commands")
        .map_err(|e| format!("Write error: {}", e))?;
    write!(writer, "cogserver> ").map_err(|e| format!("Write error: {}", e))?;
    writer.flush().map_err(|e| format!("Flush error: {}", e))?;

    // Read-eval-print loop
    loop {
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {
                let line = line.trim();

                if line.is_empty() {
                    write!(writer, "cogserver> ").map_err(|e| format!("Write error: {}", e))?;
                    writer.flush().map_err(|e| format!("Flush error: {}", e))?;
                    continue;
                }

                // Execute command
                let response = execute_command(line, &commands);
                writeln!(writer, "{}", response).map_err(|e| format!("Write error: {}", e))?;

                // Check if should keep running
                {
                    let running = running.lock().unwrap();
                    if !*running {
                        writeln!(writer, "Server shutting down...")
                            .map_err(|e| format!("Write error: {}", e))?;
                        break;
                    }
                }

                write!(writer, "cogserver> ").map_err(|e| format!("Write error: {}", e))?;
                writer.flush().map_err(|e| format!("Flush error: {}", e))?;
            }
            Err(e) => return Err(format!("Read error: {}", e)),
        }
    }

    Ok(())
}

// Execute a command line
fn execute_command(line: &str, commands: &Arc<Mutex<HashMap<String, Command>>>) -> String {
    let parts: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

    if parts.is_empty() {
        return String::new();
    }

    let cmd_name = &parts[0];
    let args = &parts[1..];

    // Look up command
    let cmds = commands.lock().unwrap();
    match cmds.get(cmd_name) {
        Some(cmd) => match cmd.execute(args) {
            Ok(result) => result,
            Err(e) => format!("Error: {}", e),
        },
        None => format!("Unknown command: {}", cmd_name),
    }
}

// ===== Demo Function =====

#[cfg(not(test))]
fn main() {
    println!("{}", "=".repeat(70));
    println!("OpenCog CogServer - Rust Implementation Demo");
    println!("Network server and REPL interface");
    println!("{}", "=".repeat(70));
    println!();

    println!("1. Creating CogServer");
    println!("{}", "-".repeat(70));
    let server = CogServer::new("127.0.0.1:17001");
    println!("  Server created on 127.0.0.1:17001");
    {
        let cmds = server.commands.lock().unwrap();
        println!("  Built-in commands: {}", cmds.len());
    }
    println!();

    println!("2. Testing Commands Locally");
    println!("{}", "-".repeat(70));
    let test_commands = vec![
        "help",
        "info",
        "list",
        "echo Hello OpenCog!",
    ];
    
    for cmd_line in &test_commands {
        println!("  > {}", cmd_line);
        let response = execute_command(cmd_line, &server.commands);
        println!("{}", response);
    }
    println!();

    println!("3. Starting Server");
    println!("{}", "-".repeat(70));
    println!("Connect with: telnet 127.0.0.1 17001");
    println!("Or: nc 127.0.0.1 17001");
    println!("Press Ctrl+C to stop the server");
    println!();

    // Start server (blocks until stopped)
    if let Err(e) = server.start() {
        eprintln!("Server error: {}", e);
    }

    println!();
    println!("{}", "=".repeat(70));
    println!("Rust CogServer strengths demonstrated:");
    println!("  ✓ Safe concurrency with threads and Arc/Mutex");
    println!("  ✓ Ownership-based resource management");
    println!("  ✓ Pattern matching for command dispatch");
    println!("  ✓ Error handling with Result type");
    println!("  ✓ Zero-cost abstractions");
    println!("  ✓ Memory safety without garbage collection");
    println!("{}", "=".repeat(70));
}
