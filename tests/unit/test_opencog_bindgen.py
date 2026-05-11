"""
Unit tests for the OpenCog binding coverage generator.

These tests protect the parity rule that the three OpenCog core components
(cogutil, atomspace, cogserver) must be compared against the union of all
implemented languages, not only against one reference component.
"""

import importlib.machinery
import importlib.util
import json
import subprocess
import sys
from pathlib import Path


def load_bindgen(root_dir: Path):
    """Load the executable opencog-bindgen script as a Python module."""
    bindgen_path = root_dir / "opencog" / "bin" / "opencog-bindgen"
    loader = importlib.machinery.SourceFileLoader("opencog_bindgen", str(bindgen_path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


def test_coverage_report_uses_union_language_set(root_dir):
    """All component missing sets must be checked against the language union."""
    bindgen = load_bindgen(root_dir)
    report = bindgen.get_coverage_report()

    assert "prolog" in report["all_languages"]
    assert set(report["components"]) == {"cogutil", "atomspace", "cogserver"}
    assert report["missing"] == {
        "cogutil": [],
        "atomspace": [],
        "cogserver": [],
    }


def test_coverage_json_cli_is_machine_readable(root_dir):
    """The CLI should emit parseable JSON for CI and downstream tooling."""
    bindgen_path = root_dir / "opencog" / "bin" / "opencog-bindgen"
    completed = subprocess.run(
        [sys.executable, str(bindgen_path), "--coverage-json"],
        cwd=root_dir,
        check=True,
        text=True,
        capture_output=True,
    )

    report = json.loads(completed.stdout)
    assert "all_languages" in report
    assert "missing" in report
    assert all(not missing for missing in report["missing"].values())
