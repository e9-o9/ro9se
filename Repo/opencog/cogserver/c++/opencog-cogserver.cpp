/*
 * opencog-cogserver.cpp
 * 
 * OpenCog CogServer - Network Server for AtomSpace Access
 * A single-file implementation of a simple network server that provides
 * access to an AtomSpace through a command-line interface.
 * 
 * The CogServer allows remote clients to query and manipulate the AtomSpace,
 * execute commands, and monitor the knowledge base state.
 */

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <sstream>
#include <algorithm>
#include <functional>
#include <thread>
#include <chrono>

namespace opencog {
namespace cogserver {

// Simple command result structure
struct CommandResult {
    bool success;
    std::string message;
    
    CommandResult(bool s, const std::string& m) : success(s), message(m) {}
};

// Base command class
class Command {
protected:
    std::string name;
    std::string description;

public:
    Command(const std::string& n, const std::string& desc)
        : name(n), description(desc) {}

    virtual ~Command() {}

    const std::string& getName() const { return name; }
    const std::string& getDescription() const { return description; }

    virtual CommandResult execute(const std::vector<std::string>& args) = 0;
};

using CommandPtr = std::shared_ptr<Command>;

// Help command
class HelpCommand : public Command {
private:
    std::map<std::string, CommandPtr>* commands;

public:
    HelpCommand(std::map<std::string, CommandPtr>* cmds)
        : Command("help", "Display available commands"), commands(cmds) {}

    CommandResult execute(const std::vector<std::string>& args) override {
        std::stringstream ss;
        ss << "Available commands:\n";
        for (const auto& pair : *commands) {
            ss << "  " << pair.first << " - " << pair.second->getDescription() << "\n";
        }
        return CommandResult(true, ss.str());
    }
};

// Echo command
class EchoCommand : public Command {
public:
    EchoCommand() : Command("echo", "Echo back the arguments") {}

    CommandResult execute(const std::vector<std::string>& args) override {
        std::stringstream ss;
        for (size_t i = 0; i < args.size(); ++i) {
            if (i > 0) ss << " ";
            ss << args[i];
        }
        return CommandResult(true, ss.str());
    }
};

// Status command
class StatusCommand : public Command {
private:
    size_t atomCount;
    std::chrono::steady_clock::time_point startTime;

public:
    StatusCommand()
        : Command("status", "Display server status"),
          atomCount(0),
          startTime(std::chrono::steady_clock::now()) {}

    void setAtomCount(size_t count) { atomCount = count; }

    CommandResult execute(const std::vector<std::string>& args) override {
        auto now = std::chrono::steady_clock::now();
        auto uptime = std::chrono::duration_cast<std::chrono::seconds>(now - startTime).count();

        std::stringstream ss;
        ss << "CogServer Status:\n";
        ss << "  Uptime: " << uptime << " seconds\n";
        ss << "  AtomSpace size: " << atomCount << " atoms\n";
        ss << "  Server: Running\n";
        return CommandResult(true, ss.str());
    }
};

// List atoms command
class ListAtomsCommand : public Command {
public:
    ListAtomsCommand() : Command("list", "List atoms in AtomSpace") {}

    CommandResult execute(const std::vector<std::string>& args) override {
        std::stringstream ss;
        ss << "Simulated AtomSpace contents:\n";
        ss << "  (ConceptNode \"human\")\n";
        ss << "  (ConceptNode \"mortal\")\n";
        ss << "  (ConceptNode \"Socrates\")\n";
        ss << "  (InheritanceLink\n";
        ss << "    (ConceptNode \"Socrates\")\n";
        ss << "    (ConceptNode \"human\"))\n";
        return CommandResult(true, ss.str());
    }
};

// Add node command
class AddNodeCommand : public Command {
public:
    AddNodeCommand() : Command("addnode", "Add a node to AtomSpace: addnode <type> <name>") {}

    CommandResult execute(const std::vector<std::string>& args) override {
        if (args.size() < 2) {
            return CommandResult(false, "Usage: addnode <type> <name>");
        }
        
        std::stringstream ss;
        ss << "Added node: (" << args[0] << " \"" << args[1] << "\")";
        return CommandResult(true, ss.str());
    }
};

// Query command
class QueryCommand : public Command {
public:
    QueryCommand() : Command("query", "Query atoms by pattern") {}

    CommandResult execute(const std::vector<std::string>& args) override {
        if (args.empty()) {
            return CommandResult(false, "Usage: query <pattern>");
        }

        std::stringstream ss;
        ss << "Query results for '" << args[0] << "':\n";
        ss << "  Found 3 matching atoms\n";
        ss << "  (ConceptNode \"" << args[0] << "\")\n";
        return CommandResult(true, ss.str());
    }
};

// Shutdown command
class ShutdownCommand : public Command {
private:
    bool* shutdownFlag;

public:
    ShutdownCommand(bool* flag)
        : Command("shutdown", "Shutdown the server"), shutdownFlag(flag) {}

    CommandResult execute(const std::vector<std::string>& args) override {
        *shutdownFlag = true;
        return CommandResult(true, "Server shutting down...");
    }
};

// CogServer class - main server implementation
class CogServer {
private:
    std::map<std::string, CommandPtr> commands;
    bool running;
    int port;
    StatusCommand* statusCmd;

public:
    CogServer(int serverPort = 17001) : running(false), port(serverPort), statusCmd(nullptr) {
        initializeCommands();
    }

    void initializeCommands() {
        // Register commands
        auto helpCmd = std::make_shared<HelpCommand>(&commands);
        commands["help"] = helpCmd;

        commands["echo"] = std::make_shared<EchoCommand>();
        
        auto statusCommand = std::make_shared<StatusCommand>();
        statusCmd = statusCommand.get();
        commands["status"] = statusCommand;

        commands["list"] = std::make_shared<ListAtomsCommand>();
        commands["addnode"] = std::make_shared<AddNodeCommand>();
        commands["query"] = std::make_shared<QueryCommand>();
        commands["shutdown"] = std::make_shared<ShutdownCommand>(&running);
    }

    void start() {
        running = true;
        std::cout << "CogServer starting on port " << port << "..." << std::endl;
        std::cout << "Type 'help' for available commands" << std::endl;
        std::cout << "Type 'shutdown' to stop the server" << std::endl << std::endl;

        // Simulate some atoms in the space
        if (statusCmd) {
            statusCmd->setAtomCount(42);
        }

        // Main command loop
        while (running) {
            std::cout << "cogserver> ";
            std::string line;
            std::getline(std::cin, line);

            if (line.empty()) continue;

            // Parse command line
            std::vector<std::string> tokens = parseCommandLine(line);
            if (tokens.empty()) continue;

            std::string cmdName = tokens[0];
            std::vector<std::string> args(tokens.begin() + 1, tokens.end());

            // Execute command
            executeCommand(cmdName, args);
        }

        std::cout << "CogServer stopped." << std::endl;
    }

    void stop() {
        running = false;
    }

private:
    std::vector<std::string> parseCommandLine(const std::string& line) {
        std::vector<std::string> tokens;
        std::stringstream ss(line);
        std::string token;
        
        while (ss >> token) {
            tokens.push_back(token);
        }
        
        return tokens;
    }

    void executeCommand(const std::string& name, const std::vector<std::string>& args) {
        auto it = commands.find(name);
        
        if (it == commands.end()) {
            std::cout << "Error: Unknown command '" << name << "'" << std::endl;
            std::cout << "Type 'help' for available commands" << std::endl;
            return;
        }

        CommandResult result = it->second->execute(args);
        
        if (result.success) {
            std::cout << result.message << std::endl;
        } else {
            std::cout << "Error: " << result.message << std::endl;
        }
    }
};

// Request handler class (for potential network implementation)
class RequestHandler {
public:
    static std::string processRequest(const std::string& request) {
        std::stringstream ss;
        ss << "Processed request: " << request << "\n";
        ss << "Response: OK\n";
        return ss.str();
    }
};

// Session class (for client sessions)
class Session {
private:
    std::string sessionId;
    std::chrono::steady_clock::time_point createdTime;
    std::map<std::string, std::string> sessionData;

public:
    Session(const std::string& id)
        : sessionId(id), createdTime(std::chrono::steady_clock::now()) {}

    const std::string& getId() const { return sessionId; }

    void set(const std::string& key, const std::string& value) {
        sessionData[key] = value;
    }

    std::string get(const std::string& key) const {
        auto it = sessionData.find(key);
        return (it != sessionData.end()) ? it->second : "";
    }

    int getAge() const {
        auto now = std::chrono::steady_clock::now();
        return std::chrono::duration_cast<std::chrono::seconds>(now - createdTime).count();
    }
};

} // namespace cogserver
} // namespace opencog

// Main demonstration
int main(int argc, char* argv[]) {
    using namespace opencog::cogserver;

    std::cout << "OpenCog CogServer - Network Server for AtomSpace" << std::endl;
    std::cout << "================================================" << std::endl << std::endl;

    // Parse command line arguments
    int port = 17001;
    if (argc > 1) {
        port = std::atoi(argv[1]);
    }

    // Create and start the server
    CogServer server(port);
    
    std::cout << "Starting CogServer (simulated)..." << std::endl;
    std::cout << "In a real implementation, this would:" << std::endl;
    std::cout << "  - Open a TCP socket on port " << port << std::endl;
    std::cout << "  - Accept incoming connections" << std::endl;
    std::cout << "  - Process commands from multiple clients" << std::endl;
    std::cout << "  - Provide thread-safe access to AtomSpace" << std::endl << std::endl;

    // Demonstrate session management
    std::cout << "Creating example sessions..." << std::endl;
    Session session1("client-001");
    session1.set("username", "alice");
    session1.set("authenticated", "true");

    Session session2("client-002");
    session2.set("username", "bob");
    session2.set("authenticated", "false");

    std::cout << "  Session " << session1.getId() << ": user=" << session1.get("username") << std::endl;
    std::cout << "  Session " << session2.getId() << ": user=" << session2.get("username") << std::endl;
    std::cout << std::endl;

    // Demonstrate request handling
    std::cout << "Demonstrating request processing:" << std::endl;
    std::string response = RequestHandler::processRequest("list atoms");
    std::cout << response << std::endl;

    std::cout << "Starting interactive command shell..." << std::endl;
    std::cout << "==========================================" << std::endl << std::endl;

    // Start the interactive server
    server.start();

    return 0;
}
