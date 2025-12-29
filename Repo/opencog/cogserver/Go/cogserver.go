// Package cogserver provides a Go implementation of the OpenCog CogServer
// network server and REPL interface.
//
// The CogServer provides network access to an AtomSpace and supports
// command execution, module loading, and agent scheduling.
package cogserver

import (
	"bufio"
	"fmt"
	"io"
	"net"
	"strings"
	"sync"
)

// CommandHandler is a function that processes a command
type CommandHandler func(args []string) (string, error)

// Command represents a registered command
type Command struct {
	Name        string
	Description string
	Handler     CommandHandler
}

// Module represents a loadable module
type Module interface {
	Init(server *CogServer) error
	GetCommands() []Command
	Shutdown() error
}

// CogServer is the main server structure
type CogServer struct {
	address  string
	listener net.Listener
	commands map[string]Command
	modules  map[string]Module
	running  bool
	mu       sync.RWMutex
	wg       sync.WaitGroup
}

// NewCogServer creates a new CogServer
func NewCogServer(address string) *CogServer {
	server := &CogServer{
		address:  address,
		commands: make(map[string]Command),
		modules:  make(map[string]Module),
	}
	
	// Register built-in commands
	server.registerBuiltinCommands()
	
	return server
}

// registerBuiltinCommands registers the default commands
func (cs *CogServer) registerBuiltinCommands() {
	cs.RegisterCommand(Command{
		Name:        "help",
		Description: "Display available commands",
		Handler:     cs.handleHelp,
	})
	
	cs.RegisterCommand(Command{
		Name:        "list",
		Description: "List loaded modules",
		Handler:     cs.handleList,
	})
	
	cs.RegisterCommand(Command{
		Name:        "shutdown",
		Description: "Shutdown the server",
		Handler:     cs.handleShutdown,
	})
}

// RegisterCommand registers a new command
func (cs *CogServer) RegisterCommand(cmd Command) {
	cs.mu.Lock()
	defer cs.mu.Unlock()
	cs.commands[cmd.Name] = cmd
}

// UnregisterCommand removes a command
func (cs *CogServer) UnregisterCommand(name string) {
	cs.mu.Lock()
	defer cs.mu.Unlock()
	delete(cs.commands, name)
}

// LoadModule loads a module and registers its commands
func (cs *CogServer) LoadModule(name string, module Module) error {
	cs.mu.Lock()
	defer cs.mu.Unlock()
	
	if _, exists := cs.modules[name]; exists {
		return fmt.Errorf("module %s already loaded", name)
	}
	
	if err := module.Init(cs); err != nil {
		return fmt.Errorf("failed to initialize module %s: %w", name, err)
	}
	
	// Register module commands
	for _, cmd := range module.GetCommands() {
		cs.commands[cmd.Name] = cmd
	}
	
	cs.modules[name] = module
	return nil
}

// UnloadModule unloads a module and unregisters its commands
func (cs *CogServer) UnloadModule(name string) error {
	cs.mu.Lock()
	defer cs.mu.Unlock()
	
	module, exists := cs.modules[name]
	if !exists {
		return fmt.Errorf("module %s not loaded", name)
	}
	
	// Unregister module commands
	for _, cmd := range module.GetCommands() {
		delete(cs.commands, cmd.Name)
	}
	
	if err := module.Shutdown(); err != nil {
		return fmt.Errorf("failed to shutdown module %s: %w", name, err)
	}
	
	delete(cs.modules, name)
	return nil
}

// Start starts the server
func (cs *CogServer) Start() error {
	cs.mu.Lock()
	if cs.running {
		cs.mu.Unlock()
		return fmt.Errorf("server already running")
	}
	
	listener, err := net.Listen("tcp", cs.address)
	if err != nil {
		cs.mu.Unlock()
		return fmt.Errorf("failed to start listener: %w", err)
	}
	
	cs.listener = listener
	cs.running = true
	cs.mu.Unlock()
	
	fmt.Printf("CogServer listening on %s\n", cs.address)
	
	// Accept connections
	cs.wg.Add(1)
	go cs.acceptConnections()
	
	return nil
}

// acceptConnections accepts incoming connections
func (cs *CogServer) acceptConnections() {
	defer cs.wg.Done()
	
	for {
		conn, err := cs.listener.Accept()
		if err != nil {
			cs.mu.RLock()
			running := cs.running
			cs.mu.RUnlock()
			
			if !running {
				return
			}
			fmt.Printf("Error accepting connection: %v\n", err)
			continue
		}
		
		cs.wg.Add(1)
		go cs.handleConnection(conn)
	}
}

// handleConnection handles a client connection
func (cs *CogServer) handleConnection(conn net.Conn) {
	defer cs.wg.Done()
	defer conn.Close()
	
	fmt.Fprintf(conn, "Welcome to CogServer\n")
	fmt.Fprintf(conn, "Type 'help' for available commands\n")
	fmt.Fprintf(conn, "cogserver> ")
	
	scanner := bufio.NewScanner(conn)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		
		if line == "" {
			fmt.Fprintf(conn, "cogserver> ")
			continue
		}
		
		response := cs.executeCommand(line)
		fmt.Fprintf(conn, "%s\n", response)
		
		// Check if server is shutting down
		cs.mu.RLock()
		running := cs.running
		cs.mu.RUnlock()
		
		if !running {
			fmt.Fprintf(conn, "Server shutting down...\n")
			return
		}
		
		fmt.Fprintf(conn, "cogserver> ")
	}
	
	if err := scanner.Err(); err != nil && err != io.EOF {
		fmt.Printf("Connection error: %v\n", err)
	}
}

// executeCommand executes a command string
func (cs *CogServer) executeCommand(line string) string {
	parts := strings.Fields(line)
	if len(parts) == 0 {
		return ""
	}
	
	cmdName := parts[0]
	args := parts[1:]
	
	cs.mu.RLock()
	cmd, exists := cs.commands[cmdName]
	cs.mu.RUnlock()
	
	if !exists {
		return fmt.Sprintf("Unknown command: %s", cmdName)
	}
	
	result, err := cmd.Handler(args)
	if err != nil {
		return fmt.Sprintf("Error: %v", err)
	}
	
	return result
}

// Stop stops the server
func (cs *CogServer) Stop() error {
	cs.mu.Lock()
	if !cs.running {
		cs.mu.Unlock()
		return fmt.Errorf("server not running")
	}
	
	cs.running = false
	
	// Close listener
	if cs.listener != nil {
		cs.listener.Close()
	}
	cs.mu.Unlock()
	
	// Wait for all connections to close
	cs.wg.Wait()
	
	// Unload all modules
	cs.mu.Lock()
	for name := range cs.modules {
		cs.modules[name].Shutdown()
	}
	cs.modules = make(map[string]Module)
	cs.mu.Unlock()
	
	fmt.Println("CogServer stopped")
	return nil
}

// Built-in command handlers

func (cs *CogServer) handleHelp(args []string) (string, error) {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	
	var sb strings.Builder
	sb.WriteString("Available commands:\n")
	
	for name, cmd := range cs.commands {
		sb.WriteString(fmt.Sprintf("  %-15s %s\n", name, cmd.Description))
	}
	
	return sb.String(), nil
}

func (cs *CogServer) handleList(args []string) (string, error) {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	
	if len(cs.modules) == 0 {
		return "No modules loaded", nil
	}
	
	var sb strings.Builder
	sb.WriteString("Loaded modules:\n")
	
	for name := range cs.modules {
		sb.WriteString(fmt.Sprintf("  %s\n", name))
	}
	
	return sb.String(), nil
}

func (cs *CogServer) handleShutdown(args []string) (string, error) {
	go func() {
		cs.Stop()
	}()
	return "Shutting down server...", nil
}

// IsRunning returns whether the server is running
func (cs *CogServer) IsRunning() bool {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	return cs.running
}

// GetAddress returns the server address
func (cs *CogServer) GetAddress() string {
	return cs.address
}
