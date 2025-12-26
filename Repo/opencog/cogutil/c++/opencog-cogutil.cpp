/*
 * opencog-cogutil.cpp
 * 
 * OpenCog Cogutil - C++ Utility Library
 * A collection of utility functions and classes for OpenCog framework
 * 
 * This single-file implementation demonstrates core utility functionality
 * including logging, configuration management, and common data structures.
 */

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <sstream>
#include <fstream>
#include <chrono>
#include <iomanip>
#include <cctype>

namespace opencog {
namespace cogutil {

// Logger class for debugging and informational output
class Logger {
public:
    enum Level {
        DEBUG,
        INFO,
        WARN,
        ERROR
    };

private:
    Level minLevel;
    std::ostream& output;

public:
    Logger(Level level = INFO, std::ostream& out = std::cout) 
        : minLevel(level), output(out) {}

    void log(Level level, const std::string& message) {
        if (level >= minLevel) {
            output << "[" << getCurrentTime() << "] " 
                   << levelToString(level) << ": " 
                   << message << std::endl;
        }
    }

    void debug(const std::string& msg) { log(DEBUG, msg); }
    void info(const std::string& msg) { log(INFO, msg); }
    void warn(const std::string& msg) { log(WARN, msg); }
    void error(const std::string& msg) { log(ERROR, msg); }

    void setLevel(Level level) { minLevel = level; }

private:
    std::string levelToString(Level level) const {
        switch(level) {
            case DEBUG: return "DEBUG";
            case INFO:  return "INFO";
            case WARN:  return "WARN";
            case ERROR: return "ERROR";
            default:    return "UNKNOWN";
        }
    }

    std::string getCurrentTime() const {
        auto now = std::chrono::system_clock::now();
        auto time = std::chrono::system_clock::to_time_t(now);
        std::stringstream ss;
        ss << std::put_time(std::localtime(&time), "%Y-%m-%d %H:%M:%S");
        return ss.str();
    }
};

// Configuration manager for storing key-value pairs
class Config {
private:
    std::map<std::string, std::string> data;

public:
    void set(const std::string& key, const std::string& value) {
        data[key] = value;
    }

    std::string get(const std::string& key, const std::string& defaultValue = "") const {
        auto it = data.find(key);
        return (it != data.end()) ? it->second : defaultValue;
    }

    bool has(const std::string& key) const {
        return data.find(key) != data.end();
    }

    bool loadFromFile(const std::string& filename) {
        std::ifstream file(filename);
        if (!file.is_open()) return false;

        std::string line;
        while (std::getline(file, line)) {
            // Skip comments and empty lines
            if (line.empty() || line[0] == '#') continue;

            size_t pos = line.find('=');
            if (pos != std::string::npos) {
                std::string key = line.substr(0, pos);
                std::string value = line.substr(pos + 1);
                // Trim whitespace
                key.erase(0, key.find_first_not_of(" \t"));
                key.erase(key.find_last_not_of(" \t") + 1);
                value.erase(0, value.find_first_not_of(" \t"));
                value.erase(value.find_last_not_of(" \t") + 1);
                set(key, value);
            }
        }
        return true;
    }

    void dump() const {
        for (const auto& pair : data) {
            std::cout << pair.first << " = " << pair.second << std::endl;
        }
    }
};

// Timer utility for performance measurement
class Timer {
private:
    std::chrono::high_resolution_clock::time_point startTime;
    std::string name;
    bool running;

public:
    Timer(const std::string& timerName = "Timer") 
        : name(timerName), running(false) {}

    void start() {
        startTime = std::chrono::high_resolution_clock::now();
        running = true;
    }

    double elapsed() const {
        if (!running) return 0.0;
        auto endTime = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> diff = endTime - startTime;
        return diff.count();
    }

    void stop(Logger* logger = nullptr) {
        if (running) {
            double time = elapsed();
            running = false;
            if (logger) {
                std::stringstream ss;
                ss << name << " completed in " << time << " seconds";
                logger->info(ss.str());
            }
        }
    }
};

// String utilities
class StringUtils {
public:
    static std::vector<std::string> split(const std::string& str, char delimiter) {
        std::vector<std::string> tokens;
        std::stringstream ss(str);
        std::string token;
        while (std::getline(ss, token, delimiter)) {
            if (!token.empty()) {
                tokens.push_back(token);
            }
        }
        return tokens;
    }

    static std::string join(const std::vector<std::string>& strings, const std::string& delimiter) {
        std::stringstream ss;
        for (size_t i = 0; i < strings.size(); ++i) {
            if (i > 0) ss << delimiter;
            ss << strings[i];
        }
        return ss.str();
    }

    static std::string toLower(const std::string& str) {
        std::string result = str;
        for (char& c : result) {
            c = std::tolower(c);
        }
        return result;
    }

    static std::string toUpper(const std::string& str) {
        std::string result = str;
        for (char& c : result) {
            c = std::toupper(c);
        }
        return result;
    }

    static std::string trim(const std::string& str) {
        size_t start = str.find_first_not_of(" \t\n\r");
        size_t end = str.find_last_not_of(" \t\n\r");
        return (start == std::string::npos) ? "" : str.substr(start, end - start + 1);
    }
};

} // namespace cogutil
} // namespace opencog

// Demonstration of cogutil functionality
int main() {
    using namespace opencog::cogutil;

    std::cout << "OpenCog Cogutil - C++ Utility Library Demo" << std::endl;
    std::cout << "===========================================" << std::endl << std::endl;

    // Logger demonstration
    Logger logger(Logger::INFO);
    logger.info("Cogutil library initialized");
    logger.debug("This debug message won't show (level too low)");
    logger.warn("This is a warning message");
    logger.error("This is an error message");

    logger.setLevel(Logger::DEBUG);
    logger.debug("Now debug messages are visible");
    std::cout << std::endl;

    // Config demonstration
    Config config;
    config.set("opencog.version", "1.0.0");
    config.set("atomspace.enabled", "true");
    config.set("cogserver.port", "17001");
    
    logger.info("Configuration loaded:");
    config.dump();
    std::cout << std::endl;

    logger.info("Retrieving specific config: port = " + 
                config.get("cogserver.port"));
    std::cout << std::endl;

    // Timer demonstration
    Timer timer("Processing");
    timer.start();
    
    logger.info("Simulating some work...");
    // Simulate work
    int sum = 0;
    for (int i = 0; i < 1000000; ++i) {
        sum += i;
    }
    
    timer.stop(&logger);
    std::cout << std::endl;

    // String utilities demonstration
    logger.info("String utilities demonstration:");
    std::string text = "OpenCog,AtomSpace,CogServer,Cogutil";
    auto parts = StringUtils::split(text, ',');
    
    logger.info("Split result:");
    for (const auto& part : parts) {
        std::cout << "  - " << part << std::endl;
    }

    std::string joined = StringUtils::join(parts, " + ");
    logger.info("Joined: " + joined);

    logger.info("Uppercase: " + StringUtils::toUpper("opencog rocks"));
    logger.info("Lowercase: " + StringUtils::toLower("OPENCOG ROCKS"));
    logger.info("Trimmed: '" + StringUtils::trim("  spaced out  ") + "'");

    std::cout << std::endl;
    logger.info("Cogutil demonstration complete!");

    return 0;
}
