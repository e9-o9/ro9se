#!/usr/bin/env python3
"""
opencog-cogutil.py

OpenCog Cogutil - Python Utility Library
A collection of utility functions and classes for OpenCog framework

This single-file implementation demonstrates Python's strengths:
- Dynamic typing and duck typing
- Rich standard library
- Decorator pattern for elegant code
- Context managers for resource management
"""

import sys
import time
import logging
from datetime import datetime
from typing import Any, Dict, List, Optional, Callable
from functools import wraps
from contextlib import contextmanager


class Logger:
    """
    Logger class showcasing Python's logging module integration
    Demonstrates: Standard library usage, singleton pattern
    """
    
    def __init__(self, name: str = "OpenCog", level: int = logging.INFO):
        self.logger = logging.getLogger(name)
        self.logger.setLevel(level)
        
        # Create console handler with formatting
        handler = logging.StreamHandler(sys.stdout)
        formatter = logging.Formatter(
            '[%(asctime)s] %(levelname)s: %(message)s',
            datefmt='%Y-%m-%d %H:%M:%S'
        )
        handler.setFormatter(formatter)
        self.logger.addHandler(handler)
    
    def debug(self, msg: str) -> None:
        self.logger.debug(msg)
    
    def info(self, msg: str) -> None:
        self.logger.info(msg)
    
    def warn(self, msg: str) -> None:
        self.logger.warning(msg)
    
    def error(self, msg: str) -> None:
        self.logger.error(msg)
    
    def set_level(self, level: int) -> None:
        self.logger.setLevel(level)


class Config:
    """
    Configuration manager using dictionary
    Demonstrates: Dictionary operations, file I/O, property access
    """
    
    def __init__(self):
        self._data: Dict[str, str] = {}
    
    def set(self, key: str, value: str) -> None:
        """Set a configuration value"""
        self._data[key] = value
    
    def get(self, key: str, default: str = "") -> str:
        """Get a configuration value with default"""
        return self._data.get(key, default)
    
    def has(self, key: str) -> bool:
        """Check if key exists"""
        return key in self._data
    
    def load_from_file(self, filename: str) -> bool:
        """Load configuration from file"""
        try:
            with open(filename, 'r') as f:
                for line in f:
                    line = line.strip()
                    # Skip comments and empty lines
                    if not line or line.startswith('#'):
                        continue
                    
                    if '=' in line:
                        key, value = line.split('=', 1)
                        self.set(key.strip(), value.strip())
            return True
        except FileNotFoundError:
            return False
    
    def dump(self) -> None:
        """Print all configuration"""
        for key, value in sorted(self._data.items()):
            print(f"{key} = {value}")
    
    def __getitem__(self, key: str) -> str:
        """Allow dictionary-style access"""
        return self._data[key]
    
    def __setitem__(self, key: str, value: str) -> None:
        """Allow dictionary-style setting"""
        self._data[key] = value


class Timer:
    """
    Timer utility using context manager
    Demonstrates: Context manager protocol, decorators
    """
    
    def __init__(self, name: str = "Timer", logger: Optional[Logger] = None):
        self.name = name
        self.logger = logger
        self.start_time: Optional[float] = None
        self.elapsed_time: Optional[float] = None
    
    def start(self) -> None:
        """Start the timer"""
        self.start_time = time.time()
    
    def stop(self) -> float:
        """Stop the timer and return elapsed time"""
        if self.start_time is None:
            return 0.0
        self.elapsed_time = time.time() - self.start_time
        if self.logger:
            self.logger.info(f"{self.name} completed in {self.elapsed_time:.6f} seconds")
        return self.elapsed_time
    
    def __enter__(self):
        """Context manager entry"""
        self.start()
        return self
    
    def __exit__(self, *args):
        """Context manager exit"""
        self.stop()


def timeit(func: Callable) -> Callable:
    """
    Decorator to time function execution
    Demonstrates: Python decorators, functional programming
    """
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        elapsed = time.time() - start
        print(f"[TIMER] {func.__name__} took {elapsed:.6f} seconds")
        return result
    return wrapper


class StringUtils:
    """
    String utilities showcasing Python's string methods
    Demonstrates: Static methods, string manipulation, list comprehensions
    """
    
    @staticmethod
    def split(text: str, delimiter: str = ',') -> List[str]:
        """Split string by delimiter, filtering empty strings"""
        return [s.strip() for s in text.split(delimiter) if s.strip()]
    
    @staticmethod
    def join(strings: List[str], delimiter: str = ',') -> str:
        """Join strings with delimiter"""
        return delimiter.join(strings)
    
    @staticmethod
    def to_lower(text: str) -> str:
        """Convert to lowercase"""
        return text.lower()
    
    @staticmethod
    def to_upper(text: str) -> str:
        """Convert to uppercase"""
        return text.upper()
    
    @staticmethod
    def trim(text: str) -> str:
        """Remove leading/trailing whitespace"""
        return text.strip()
    
    @staticmethod
    def camel_to_snake(text: str) -> str:
        """Convert camelCase to snake_case (Python convention)"""
        import re
        return re.sub(r'(?<!^)(?=[A-Z])', '_', text).lower()
    
    @staticmethod
    def snake_to_camel(text: str) -> str:
        """Convert snake_case to camelCase"""
        components = text.split('_')
        return components[0] + ''.join(x.title() for x in components[1:])


@contextmanager
def measure_performance(operation: str):
    """
    Context manager for measuring performance
    Demonstrates: Context manager decorator, Python idioms
    """
    print(f"[PERF] Starting: {operation}")
    start = time.time()
    try:
        yield
    finally:
        elapsed = time.time() - start
        print(f"[PERF] Completed: {operation} in {elapsed:.6f}s")


def demonstrate_cogutil():
    """Main demonstration function"""
    print("=" * 70)
    print("OpenCog Cogutil - Python Utility Library Demo")
    print("Showcasing Python's strengths: Dynamic typing, rich stdlib, elegance")
    print("=" * 70)
    print()
    
    # Logger demonstration
    print("1. Logger Demonstration")
    print("-" * 50)
    logger = Logger("CogUtil", logging.INFO)
    logger.info("Cogutil library initialized")
    logger.debug("This debug message won't show (level too low)")
    logger.warn("This is a warning message")
    logger.error("This is an error message")
    
    logger.set_level(logging.DEBUG)
    logger.debug("Now debug messages are visible")
    print()
    
    # Config demonstration
    print("2. Configuration Manager")
    print("-" * 50)
    config = Config()
    config.set("opencog.version", "1.0.0")
    config["atomspace.enabled"] = "true"  # Dictionary-style access
    config["cogserver.port"] = "17001"
    
    logger.info("Configuration loaded:")
    config.dump()
    print()
    
    logger.info(f"Port setting: {config.get('cogserver.port')}")
    print()
    
    # Timer demonstration with context manager
    print("3. Timer Context Manager")
    print("-" * 50)
    with Timer("Processing", logger) as timer:
        logger.info("Simulating some work...")
        # Simulate work
        total = sum(range(1_000_000))
    print()
    
    # Performance measurement context manager
    print("4. Performance Context Manager")
    print("-" * 50)
    with measure_performance("Data processing"):
        time.sleep(0.01)  # Simulate work
    print()
    
    # String utilities demonstration
    print("5. String Utilities")
    print("-" * 50)
    logger.info("String utilities demonstration:")
    text = "OpenCog,AtomSpace,CogServer,Cogutil"
    parts = StringUtils.split(text, ',')
    
    logger.info("Split result:")
    for part in parts:
        print(f"  - {part}")
    
    joined = StringUtils.join(parts, " + ")
    logger.info(f"Joined: {joined}")
    
    logger.info(f"Uppercase: {StringUtils.to_upper('opencog rocks')}")
    logger.info(f"Lowercase: {StringUtils.to_lower('OPENCOG ROCKS')}")
    logger.info(f"Trimmed: '{StringUtils.trim('  spaced out  ')}'")
    
    # Python-specific: Case conversions
    logger.info(f"camelCase → snake_case: {StringUtils.camel_to_snake('myVariableName')}")
    logger.info(f"snake_case → camelCase: {StringUtils.snake_to_camel('my_variable_name')}")
    print()
    
    # Decorator demonstration
    print("6. Function Decorator")
    print("-" * 50)
    
    @timeit
    def fibonacci(n: int) -> int:
        """Calculate Fibonacci number"""
        if n <= 1:
            return n
        return fibonacci(n-1) + fibonacci(n-2)
    
    result = fibonacci(20)
    logger.info(f"Fibonacci(20) = {result}")
    print()
    
    # List comprehension demonstration
    print("7. List Comprehensions (Pythonic)")
    print("-" * 50)
    squares = [x**2 for x in range(10)]
    logger.info(f"Squares: {squares}")
    
    evens = [x for x in range(20) if x % 2 == 0]
    logger.info(f"Even numbers: {evens}")
    print()
    
    logger.info("Cogutil demonstration complete!")
    print("=" * 70)
    print("Python strengths demonstrated:")
    print("  ✓ Clean, readable syntax")
    print("  ✓ Rich standard library (logging, time, etc.)")
    print("  ✓ Context managers for resource management")
    print("  ✓ Decorators for code elegance")
    print("  ✓ List comprehensions for conciseness")
    print("  ✓ Duck typing and dynamic behavior")
    print("=" * 70)


if __name__ == "__main__":
    demonstrate_cogutil()
