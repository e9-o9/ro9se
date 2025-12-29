#!/usr/bin/env python3
"""
Data Validation Module for RosettaCog

Provides validation utilities for:
- YAML/JSON configuration files
- Data integrity checking
- Schema validation
- Error reporting with context
"""

import os
import yaml
import json
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass
from enum import Enum


class ValidationSeverity(Enum):
    """Severity levels for validation issues."""
    ERROR = "error"
    WARNING = "warning"
    INFO = "info"


@dataclass
class ValidationIssue:
    """Represents a single validation issue."""
    severity: ValidationSeverity
    message: str
    location: Optional[str] = None
    details: Optional[str] = None

    def __str__(self) -> str:
        loc = f" at {self.location}" if self.location else ""
        det = f" - {self.details}" if self.details else ""
        return f"[{self.severity.value.upper()}]{loc}: {self.message}{det}"


@dataclass
class ValidationResult:
    """Result of a validation operation."""
    is_valid: bool
    issues: List[ValidationIssue]

    @property
    def errors(self) -> List[ValidationIssue]:
        """Get only error-level issues."""
        return [i for i in self.issues if i.severity == ValidationSeverity.ERROR]

    @property
    def warnings(self) -> List[ValidationIssue]:
        """Get only warning-level issues."""
        return [i for i in self.issues if i.severity == ValidationSeverity.WARNING]

    def add_issue(self, issue: ValidationIssue):
        """Add an issue to the result."""
        self.issues.append(issue)
        if issue.severity == ValidationSeverity.ERROR:
            self.is_valid = False

    def merge(self, other: 'ValidationResult'):
        """Merge another validation result into this one."""
        self.issues.extend(other.issues)
        if not other.is_valid:
            self.is_valid = False

    def __str__(self) -> str:
        status = "VALID" if self.is_valid else "INVALID"
        lines = [f"Validation Result: {status}"]
        lines.append(f"  Errors: {len(self.errors)}")
        lines.append(f"  Warnings: {len(self.warnings)}")
        for issue in self.issues:
            lines.append(f"  - {issue}")
        return "\n".join(lines)


class DataValidator:
    """Validates RosettaCog data structures and files."""

    def __init__(self, root_dir: str):
        self.root_dir = Path(root_dir)
        self.lang_dir = self.root_dir / "Lang"
        self.task_dir = self.root_dir / "Task"
        self.opencog_dir = self.root_dir / "opencog"
        self.data_dir = self.opencog_dir / "data"

    def validate_all(self) -> ValidationResult:
        """Run all validation checks."""
        result = ValidationResult(is_valid=True, issues=[])

        # Validate directory structure
        result.merge(self.validate_directory_structure())

        # Validate categories file
        result.merge(self.validate_categories_file())

        # Validate language directories
        result.merge(self.validate_languages())

        return result

    def validate_directory_structure(self) -> ValidationResult:
        """Validate that required directories exist."""
        result = ValidationResult(is_valid=True, issues=[])

        required_dirs = [
            (self.lang_dir, "Lang directory"),
            (self.task_dir, "Task directory"),
            (self.opencog_dir, "opencog directory"),
            (self.data_dir, "opencog/data directory"),
        ]

        for dir_path, name in required_dirs:
            if not dir_path.exists():
                result.add_issue(ValidationIssue(
                    severity=ValidationSeverity.ERROR,
                    message=f"{name} not found",
                    location=str(dir_path)
                ))
            elif not dir_path.is_dir():
                result.add_issue(ValidationIssue(
                    severity=ValidationSeverity.ERROR,
                    message=f"{name} exists but is not a directory",
                    location=str(dir_path)
                ))

        return result

    def validate_categories_file(self) -> ValidationResult:
        """Validate the AI task categories YAML file."""
        result = ValidationResult(is_valid=True, issues=[])
        categories_file = self.data_dir / "ai-task-categories.yaml"

        if not categories_file.exists():
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.ERROR,
                message="AI task categories file not found",
                location=str(categories_file)
            ))
            return result

        try:
            with open(categories_file, 'r') as f:
                data = yaml.safe_load(f)
        except yaml.YAMLError as e:
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.ERROR,
                message="Invalid YAML syntax",
                location=str(categories_file),
                details=str(e)
            ))
            return result

        if not isinstance(data, dict):
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.ERROR,
                message="Categories file must contain a dictionary",
                location=str(categories_file)
            ))
            return result

        # Validate categories section
        if 'categories' not in data:
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.WARNING,
                message="No 'categories' section found",
                location=str(categories_file)
            ))
        else:
            result.merge(self._validate_categories_section(data['categories']))

        # Validate paradigms section
        if 'paradigms' not in data:
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.WARNING,
                message="No 'paradigms' section found",
                location=str(categories_file)
            ))
        else:
            result.merge(self._validate_paradigms_section(data['paradigms']))

        return result

    def _validate_categories_section(self, categories: Dict) -> ValidationResult:
        """Validate the categories section of the config."""
        result = ValidationResult(is_valid=True, issues=[])

        if not isinstance(categories, dict):
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.ERROR,
                message="'categories' must be a dictionary",
                location="categories"
            ))
            return result

        for cat_name, cat_data in categories.items():
            if not isinstance(cat_data, dict):
                result.add_issue(ValidationIssue(
                    severity=ValidationSeverity.WARNING,
                    message=f"Category '{cat_name}' should be a dictionary",
                    location=f"categories.{cat_name}"
                ))
                continue

            # Check for description
            if 'description' not in cat_data:
                result.add_issue(ValidationIssue(
                    severity=ValidationSeverity.INFO,
                    message=f"Category '{cat_name}' missing description",
                    location=f"categories.{cat_name}"
                ))

            # Check for subcategories
            if 'subcategories' in cat_data:
                subcats = cat_data['subcategories']
                if not isinstance(subcats, dict):
                    result.add_issue(ValidationIssue(
                        severity=ValidationSeverity.WARNING,
                        message=f"Subcategories for '{cat_name}' should be a dictionary",
                        location=f"categories.{cat_name}.subcategories"
                    ))

        return result

    def _validate_paradigms_section(self, paradigms: Dict) -> ValidationResult:
        """Validate the paradigms section of the config."""
        result = ValidationResult(is_valid=True, issues=[])

        if not isinstance(paradigms, dict):
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.ERROR,
                message="'paradigms' must be a dictionary",
                location="paradigms"
            ))
            return result

        for para_name, para_data in paradigms.items():
            if not isinstance(para_data, dict):
                result.add_issue(ValidationIssue(
                    severity=ValidationSeverity.WARNING,
                    message=f"Paradigm '{para_name}' should be a dictionary",
                    location=f"paradigms.{para_name}"
                ))
                continue

            # Check for languages list
            if 'languages' not in para_data:
                result.add_issue(ValidationIssue(
                    severity=ValidationSeverity.INFO,
                    message=f"Paradigm '{para_name}' missing languages list",
                    location=f"paradigms.{para_name}"
                ))
            elif not isinstance(para_data['languages'], list):
                result.add_issue(ValidationIssue(
                    severity=ValidationSeverity.WARNING,
                    message=f"Languages for '{para_name}' should be a list",
                    location=f"paradigms.{para_name}.languages"
                ))

        return result

    def validate_languages(self, sample_size: int = 10) -> ValidationResult:
        """Validate language directory structure (samples for performance)."""
        result = ValidationResult(is_valid=True, issues=[])

        if not self.lang_dir.exists():
            return result  # Already reported by directory validation

        languages = []
        for lang_path in sorted(self.lang_dir.iterdir()):
            if lang_path.is_dir():
                languages.append(lang_path.name)

        if len(languages) == 0:
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.WARNING,
                message="No language directories found",
                location=str(self.lang_dir)
            ))
            return result

        # Sample languages for validation
        import random
        sample = random.sample(languages, min(sample_size, len(languages)))

        for lang in sample:
            lang_path = self.lang_dir / lang
            # Check for at least one task implementation
            tasks = list(lang_path.iterdir())
            if len(tasks) == 0:
                result.add_issue(ValidationIssue(
                    severity=ValidationSeverity.INFO,
                    message=f"Language '{lang}' has no task implementations",
                    location=str(lang_path)
                ))

        return result

    def validate_json_file(self, filepath: str) -> ValidationResult:
        """Validate a JSON file."""
        result = ValidationResult(is_valid=True, issues=[])
        path = Path(filepath)

        if not path.exists():
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.ERROR,
                message="File not found",
                location=str(path)
            ))
            return result

        try:
            with open(path, 'r') as f:
                json.load(f)
        except json.JSONDecodeError as e:
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.ERROR,
                message="Invalid JSON syntax",
                location=str(path),
                details=str(e)
            ))

        return result

    def validate_yaml_file(self, filepath: str) -> ValidationResult:
        """Validate a YAML file."""
        result = ValidationResult(is_valid=True, issues=[])
        path = Path(filepath)

        if not path.exists():
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.ERROR,
                message="File not found",
                location=str(path)
            ))
            return result

        try:
            with open(path, 'r') as f:
                yaml.safe_load(f)
        except yaml.YAMLError as e:
            result.add_issue(ValidationIssue(
                severity=ValidationSeverity.ERROR,
                message="Invalid YAML syntax",
                location=str(path),
                details=str(e)
            ))

        return result


def validate_rosettacog(root_dir: str) -> ValidationResult:
    """Convenience function to validate RosettaCog installation."""
    validator = DataValidator(root_dir)
    return validator.validate_all()


def main():
    """CLI for data validation."""
    import sys

    if len(sys.argv) < 2:
        print("Usage: python validation.py <root_dir>")
        sys.exit(1)

    root_dir = sys.argv[1]
    result = validate_rosettacog(root_dir)

    print(result)

    sys.exit(0 if result.is_valid else 1)


if __name__ == '__main__':
    main()
