package cogserver

import (
	"bufio"
	"fmt"
	"net"
	"strings"
	"testing"
	"time"
)

func TestNewCogServer(t *testing.T) {
	server := NewCogServer("localhost:17001")
	if server == nil {
		t.Fatal("NewCogServer returned nil")
	}
	
	if server.GetAddress() != "localhost:17001" {
		t.Errorf("Expected address localhost:17001, got %s", server.GetAddress())
	}
}

func TestRegisterCommand(t *testing.T) {
	server := NewCogServer("localhost:17002")
	
	called := false
	server.RegisterCommand(Command{
		Name:        "test",
		Description: "Test command",
		Handler: func(args []string) (string, error) {
			called = true
			return "test response", nil
		},
	})
	
	result := server.executeCommand("test")
	if !called {
		t.Error("Command handler was not called")
	}
	
	if result != "test response" {
		t.Errorf("Expected 'test response', got '%s'", result)
	}
}

func TestUnregisterCommand(t *testing.T) {
	server := NewCogServer("localhost:17003")
	
	server.RegisterCommand(Command{
		Name:        "test",
		Description: "Test command",
		Handler: func(args []string) (string, error) {
			return "test", nil
		},
	})
	
	server.UnregisterCommand("test")
	
	result := server.executeCommand("test")
	if !strings.Contains(result, "Unknown command") {
		t.Error("Unregistered command should return unknown command error")
	}
}

func TestBuiltinHelp(t *testing.T) {
	server := NewCogServer("localhost:17004")
	
	result := server.executeCommand("help")
	if !strings.Contains(result, "Available commands") {
		t.Error("Help command should list available commands")
	}
}

func TestBuiltinList(t *testing.T) {
	server := NewCogServer("localhost:17005")
	
	result := server.executeCommand("list")
	if !strings.Contains(result, "No modules loaded") {
		t.Error("List command should show no modules initially")
	}
}

type TestModule struct {
	initialized bool
	shutdown    bool
}

func (tm *TestModule) Init(server *CogServer) error {
	tm.initialized = true
	return nil
}

func (tm *TestModule) GetCommands() []Command {
	return []Command{
		{
			Name:        "testmodule",
			Description: "Test module command",
			Handler: func(args []string) (string, error) {
				return "module response", nil
			},
		},
	}
}

func (tm *TestModule) Shutdown() error {
	tm.shutdown = true
	return nil
}

func TestLoadModule(t *testing.T) {
	server := NewCogServer("localhost:17006")
	module := &TestModule{}
	
	err := server.LoadModule("test", module)
	if err != nil {
		t.Fatalf("Failed to load module: %v", err)
	}
	
	if !module.initialized {
		t.Error("Module should be initialized")
	}
	
	result := server.executeCommand("testmodule")
	if result != "module response" {
		t.Errorf("Expected 'module response', got '%s'", result)
	}
}

func TestUnloadModule(t *testing.T) {
	server := NewCogServer("localhost:17007")
	module := &TestModule{}
	
	server.LoadModule("test", module)
	
	err := server.UnloadModule("test")
	if err != nil {
		t.Fatalf("Failed to unload module: %v", err)
	}
	
	if !module.shutdown {
		t.Error("Module should be shut down")
	}
	
	result := server.executeCommand("testmodule")
	if !strings.Contains(result, "Unknown command") {
		t.Error("Module command should be unregistered")
	}
}

func TestStartStop(t *testing.T) {
	server := NewCogServer("localhost:17008")
	
	err := server.Start()
	if err != nil {
		t.Fatalf("Failed to start server: %v", err)
	}
	
	if !server.IsRunning() {
		t.Error("Server should be running")
	}
	
	// Give server time to start
	time.Sleep(100 * time.Millisecond)
	
	err = server.Stop()
	if err != nil {
		t.Fatalf("Failed to stop server: %v", err)
	}
	
	if server.IsRunning() {
		t.Error("Server should not be running")
	}
}

func TestClientConnection(t *testing.T) {
	server := NewCogServer("localhost:17009")
	
	err := server.Start()
	if err != nil {
		t.Fatalf("Failed to start server: %v", err)
	}
	defer server.Stop()
	
	// Give server time to start
	time.Sleep(100 * time.Millisecond)
	
	// Connect as client
	conn, err := net.Dial("tcp", "localhost:17009")
	if err != nil {
		t.Fatalf("Failed to connect to server: %v", err)
	}
	defer conn.Close()
	
	reader := bufio.NewReader(conn)
	
	// Read welcome message
	line, err := reader.ReadString('\n')
	if err != nil {
		t.Fatalf("Failed to read welcome: %v", err)
	}
	
	if !strings.Contains(line, "Welcome") {
		t.Error("Expected welcome message")
	}
	
	// Read prompt
	reader.ReadString('>')
	
	// Send help command
	fmt.Fprintf(conn, "help\n")
	
	// Read response
	response := ""
	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			break
		}
		response += line
		if strings.Contains(line, "cogserver>") {
			break
		}
	}
	
	if !strings.Contains(response, "Available commands") {
		t.Error("Expected help response")
	}
}

func TestCommandWithArgs(t *testing.T) {
	server := NewCogServer("localhost:17010")
	
	var receivedArgs []string
	server.RegisterCommand(Command{
		Name:        "echo",
		Description: "Echo arguments",
		Handler: func(args []string) (string, error) {
			receivedArgs = args
			return strings.Join(args, " "), nil
		},
	})
	
	result := server.executeCommand("echo hello world")
	
	if len(receivedArgs) != 2 {
		t.Errorf("Expected 2 args, got %d", len(receivedArgs))
	}
	
	if result != "hello world" {
		t.Errorf("Expected 'hello world', got '%s'", result)
	}
}

func TestCommandError(t *testing.T) {
	server := NewCogServer("localhost:17011")
	
	server.RegisterCommand(Command{
		Name:        "error",
		Description: "Error command",
		Handler: func(args []string) (string, error) {
			return "", fmt.Errorf("test error")
		},
	})
	
	result := server.executeCommand("error")
	
	if !strings.Contains(result, "Error") {
		t.Error("Expected error message")
	}
}

func TestConcurrentConnections(t *testing.T) {
	server := NewCogServer("localhost:17012")
	
	err := server.Start()
	if err != nil {
		t.Fatalf("Failed to start server: %v", err)
	}
	defer server.Stop()
	
	time.Sleep(100 * time.Millisecond)
	
	// Create multiple concurrent connections
	done := make(chan bool)
	
	for i := 0; i < 5; i++ {
		go func(id int) {
			conn, err := net.Dial("tcp", "localhost:17012")
			if err != nil {
				t.Errorf("Client %d failed to connect: %v", id, err)
				done <- false
				return
			}
			defer conn.Close()
			
			reader := bufio.NewReader(conn)
			
			// Read welcome and prompt
			reader.ReadString('>')
			
			// Send command
			fmt.Fprintf(conn, "help\n")
			
			// Read response
			response := ""
			for {
				line, err := reader.ReadString('\n')
				if err != nil {
					break
				}
				response += line
				if strings.Contains(line, "cogserver>") {
					break
				}
			}
			
			if strings.Contains(response, "Available commands") {
				done <- true
			} else {
				done <- false
			}
		}(i)
	}
	
	// Wait for all clients
	success := 0
	for i := 0; i < 5; i++ {
		if <-done {
			success++
		}
	}
	
	if success != 5 {
		t.Errorf("Expected 5 successful connections, got %d", success)
	}
}
