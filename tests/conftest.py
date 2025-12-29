"""
Pytest configuration and shared fixtures for RosettaCog test suite.
"""

import pytest
import sys
import tempfile
import shutil
from pathlib import Path

# Add repository root and opencog/lib to path for imports
ROOT_DIR = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT_DIR))
sys.path.insert(0, str(ROOT_DIR / "opencog"))
sys.path.insert(0, str(ROOT_DIR / "opencog" / "lib"))


@pytest.fixture(scope="session")
def root_dir():
    """Get the repository root directory."""
    return ROOT_DIR


@pytest.fixture(scope="session")
def opencog_dir(root_dir):
    """Get the opencog directory."""
    return root_dir / "opencog"


@pytest.fixture(scope="session")
def data_dir(opencog_dir):
    """Get the data directory with AI task categories."""
    return opencog_dir / "data"


@pytest.fixture(scope="session")
def lang_dir(root_dir):
    """Get the Lang directory containing language implementations."""
    return root_dir / "Lang"


@pytest.fixture(scope="session")
def task_dir(root_dir):
    """Get the Task directory containing task implementations."""
    return root_dir / "Task"


@pytest.fixture(scope="session")
def categories_file(data_dir):
    """Get the AI task categories YAML file."""
    return data_dir / "ai-task-categories.yaml"


@pytest.fixture
def temp_dir():
    """Create a temporary directory for test outputs."""
    tmpdir = tempfile.mkdtemp(prefix="rosettacog_test_")
    yield Path(tmpdir)
    shutil.rmtree(tmpdir, ignore_errors=True)


@pytest.fixture(scope="session")
def sample_languages():
    """Sample of languages for testing (to avoid full iteration)."""
    return ["Python", "C", "Java", "Ruby", "Go", "Rust", "Haskell", "Lisp", "Prolog"]


@pytest.fixture(scope="session")
def sample_tasks():
    """Sample of tasks for testing."""
    return [
        "Hello_world",
        "Fibonacci_sequence",
        "FizzBuzz",
        "Sorting_algorithms",
        "Binary_search",
    ]


@pytest.fixture(scope="session")
def sample_paradigms():
    """Sample paradigms for testing."""
    return ["imperative", "functional", "object_oriented", "logic", "concurrent"]


@pytest.fixture(scope="session")
def sample_domains():
    """Sample cognitive domains for testing."""
    return [
        "symbolic_reasoning",
        "pattern_recognition",
        "machine_learning",
        "natural_language",
        "meta_learning",
    ]


# Markers for test categorization
def pytest_configure(config):
    """Configure custom pytest markers."""
    config.addinivalue_line("markers", "slow: mark test as slow running")
    config.addinivalue_line("markers", "integration: mark as integration test")
    config.addinivalue_line("markers", "requires_data: mark test as requiring full dataset")
