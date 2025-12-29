"""
Unit tests for validation.py

Tests the data validation module for RosettaCog.
"""

import pytest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "opencog" / "lib"))

from validation import (
    ValidationSeverity,
    ValidationIssue,
    ValidationResult,
    DataValidator,
    validate_rosettacog,
)


class TestValidationSeverity:
    """Tests for ValidationSeverity enum."""

    def test_error_severity(self):
        """Test ERROR severity value."""
        assert ValidationSeverity.ERROR.value == "error"

    def test_warning_severity(self):
        """Test WARNING severity value."""
        assert ValidationSeverity.WARNING.value == "warning"

    def test_info_severity(self):
        """Test INFO severity value."""
        assert ValidationSeverity.INFO.value == "info"


class TestValidationIssue:
    """Tests for ValidationIssue dataclass."""

    def test_basic_issue(self):
        """Test creating a basic issue."""
        issue = ValidationIssue(
            severity=ValidationSeverity.ERROR,
            message="Test error"
        )
        assert issue.severity == ValidationSeverity.ERROR
        assert issue.message == "Test error"
        assert issue.location is None
        assert issue.details is None

    def test_issue_with_location(self):
        """Test issue with location."""
        issue = ValidationIssue(
            severity=ValidationSeverity.WARNING,
            message="Test warning",
            location="/path/to/file"
        )
        assert issue.location == "/path/to/file"

    def test_issue_str_representation(self):
        """Test string representation of issue."""
        issue = ValidationIssue(
            severity=ValidationSeverity.ERROR,
            message="Something went wrong",
            location="/file.yaml"
        )
        s = str(issue)
        assert "[ERROR]" in s
        assert "Something went wrong" in s
        assert "/file.yaml" in s


class TestValidationResult:
    """Tests for ValidationResult dataclass."""

    def test_valid_result(self):
        """Test creating a valid result."""
        result = ValidationResult(is_valid=True, issues=[])
        assert result.is_valid
        assert len(result.issues) == 0

    def test_add_error_invalidates(self):
        """Test that adding error invalidates result."""
        result = ValidationResult(is_valid=True, issues=[])
        result.add_issue(ValidationIssue(
            severity=ValidationSeverity.ERROR,
            message="Error"
        ))
        assert not result.is_valid

    def test_add_warning_keeps_valid(self):
        """Test that adding warning keeps result valid."""
        result = ValidationResult(is_valid=True, issues=[])
        result.add_issue(ValidationIssue(
            severity=ValidationSeverity.WARNING,
            message="Warning"
        ))
        assert result.is_valid

    def test_errors_property(self):
        """Test errors property filters correctly."""
        result = ValidationResult(is_valid=True, issues=[])
        result.add_issue(ValidationIssue(severity=ValidationSeverity.ERROR, message="E1"))
        result.add_issue(ValidationIssue(severity=ValidationSeverity.WARNING, message="W1"))
        result.add_issue(ValidationIssue(severity=ValidationSeverity.ERROR, message="E2"))

        assert len(result.errors) == 2
        assert all(e.severity == ValidationSeverity.ERROR for e in result.errors)

    def test_warnings_property(self):
        """Test warnings property filters correctly."""
        result = ValidationResult(is_valid=True, issues=[])
        result.add_issue(ValidationIssue(severity=ValidationSeverity.ERROR, message="E1"))
        result.add_issue(ValidationIssue(severity=ValidationSeverity.WARNING, message="W1"))
        result.add_issue(ValidationIssue(severity=ValidationSeverity.WARNING, message="W2"))

        assert len(result.warnings) == 2
        assert all(w.severity == ValidationSeverity.WARNING for w in result.warnings)

    def test_merge_results(self):
        """Test merging validation results."""
        result1 = ValidationResult(is_valid=True, issues=[])
        result1.add_issue(ValidationIssue(severity=ValidationSeverity.WARNING, message="W1"))

        result2 = ValidationResult(is_valid=True, issues=[])
        result2.add_issue(ValidationIssue(severity=ValidationSeverity.ERROR, message="E1"))

        result1.merge(result2)

        assert not result1.is_valid
        assert len(result1.issues) == 2


class TestDataValidator:
    """Tests for DataValidator class."""

    def test_initialization(self, root_dir):
        """Test validator initialization."""
        validator = DataValidator(str(root_dir))
        assert validator.root_dir == root_dir
        assert validator.lang_dir == root_dir / "Lang"
        assert validator.task_dir == root_dir / "Task"

    def test_validate_directory_structure(self, root_dir):
        """Test directory structure validation."""
        validator = DataValidator(str(root_dir))
        result = validator.validate_directory_structure()
        # Should be valid for the actual repo
        assert result.is_valid

    def test_validate_categories_file(self, root_dir):
        """Test categories file validation."""
        validator = DataValidator(str(root_dir))
        result = validator.validate_categories_file()
        # Should be valid for the actual repo
        assert result.is_valid

    def test_validate_languages(self, root_dir):
        """Test language directory validation."""
        validator = DataValidator(str(root_dir))
        result = validator.validate_languages(sample_size=5)
        # Should be valid for the actual repo
        assert result.is_valid

    def test_validate_all(self, root_dir):
        """Test complete validation."""
        validator = DataValidator(str(root_dir))
        result = validator.validate_all()
        # Should be valid for the actual repo
        assert result.is_valid


class TestValidateRosettacog:
    """Tests for validate_rosettacog convenience function."""

    def test_validate_valid_repo(self, root_dir):
        """Test validation of valid repository."""
        result = validate_rosettacog(str(root_dir))
        assert result.is_valid

    def test_validate_invalid_path(self, temp_dir):
        """Test validation of invalid path."""
        result = validate_rosettacog(str(temp_dir))
        # Should have errors for missing directories
        assert not result.is_valid
        assert len(result.errors) > 0
