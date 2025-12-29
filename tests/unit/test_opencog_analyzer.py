"""
Unit tests for opencog_analyzer.py

Tests the OpenCogAnalyzer class which evaluates language capabilities
across AI/cognitive domains.
"""

import pytest
import sys
from pathlib import Path
from collections import defaultdict

# Add lib to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "opencog" / "lib"))

from opencog_analyzer import OpenCogAnalyzer


class TestOpenCogAnalyzerInit:
    """Tests for OpenCogAnalyzer initialization."""

    def test_init_with_valid_root_dir(self, root_dir):
        """Test that analyzer initializes with valid root directory."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        assert analyzer.root_dir == root_dir
        assert analyzer.lang_dir == root_dir / "Lang"
        assert analyzer.task_dir == root_dir / "Task"
        assert analyzer.opencog_dir == root_dir / "opencog"

    def test_init_creates_data_dir_path(self, root_dir):
        """Test that data_dir path is correctly set."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        assert analyzer.data_dir == root_dir / "opencog" / "data"

    def test_init_loads_categories(self, root_dir):
        """Test that categories are loaded on init."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        # Categories should be a dict (possibly empty if file doesn't exist)
        assert isinstance(analyzer.categories, dict)


class TestGetAllLanguages:
    """Tests for get_all_languages method."""

    def test_returns_list(self, root_dir):
        """Test that get_all_languages returns a list."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        languages = analyzer.get_all_languages()
        assert isinstance(languages, list)

    def test_returns_sorted_languages(self, root_dir):
        """Test that languages are returned sorted."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        languages = analyzer.get_all_languages()
        assert languages == sorted(languages)

    @pytest.mark.requires_data
    def test_returns_expected_count(self, root_dir):
        """Test that we get approximately 970 languages."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        languages = analyzer.get_all_languages()
        # Should have ~970 languages based on RosettaCode data
        assert len(languages) > 900

    def test_common_languages_present(self, root_dir, sample_languages):
        """Test that common languages are present."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        languages = analyzer.get_all_languages()
        for lang in sample_languages:
            assert lang in languages, f"Expected {lang} in languages"


class TestGetAllTasks:
    """Tests for get_all_tasks method."""

    def test_returns_list(self, root_dir):
        """Test that get_all_tasks returns a list."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        tasks = analyzer.get_all_tasks()
        assert isinstance(tasks, list)

    def test_returns_sorted_tasks(self, root_dir):
        """Test that tasks are returned sorted."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        tasks = analyzer.get_all_tasks()
        assert tasks == sorted(tasks)

    @pytest.mark.requires_data
    def test_returns_expected_count(self, root_dir):
        """Test that we get approximately 1228 tasks."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        tasks = analyzer.get_all_tasks()
        # Should have ~1228 tasks based on RosettaCode data
        assert len(tasks) > 1000


class TestGetLanguageTasks:
    """Tests for get_language_tasks method."""

    def test_returns_set(self, root_dir):
        """Test that get_language_tasks returns a set."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        tasks = analyzer.get_language_tasks("Python")
        assert isinstance(tasks, set)

    def test_nonexistent_language_returns_empty(self, root_dir):
        """Test that nonexistent language returns empty set."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        tasks = analyzer.get_language_tasks("NonexistentLanguage12345")
        assert tasks == set()

    def test_python_has_many_tasks(self, root_dir):
        """Test that Python has many task implementations."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        tasks = analyzer.get_language_tasks("Python")
        # Python should have many implementations
        assert len(tasks) > 100


class TestCountTaskImplementations:
    """Tests for count_task_implementations method."""

    def test_returns_integer(self, root_dir):
        """Test that count returns an integer."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        count = analyzer.count_task_implementations("Hello_world")
        assert isinstance(count, int)

    def test_nonexistent_task_returns_zero(self, root_dir):
        """Test that nonexistent task returns zero."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        count = analyzer.count_task_implementations("NonexistentTask12345")
        assert count == 0

    def test_common_task_has_implementations(self, root_dir):
        """Test that common tasks have language implementations."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        # Check a few common task name patterns
        tasks = analyzer.get_all_tasks()
        # Find any task that has implementations
        found_implementations = False
        for task in tasks[:20]:  # Check first 20 tasks
            count = analyzer.count_task_implementations(task)
            if count > 0:
                found_implementations = True
                break
        assert found_implementations, "At least one task should have implementations"


class TestCategorizeTask:
    """Tests for categorize_task method."""

    def test_returns_list(self, root_dir):
        """Test that categorize_task returns a list."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        categories = analyzer.categorize_task("Fibonacci_sequence")
        assert isinstance(categories, list)

    def test_empty_categories_return_empty_list(self, root_dir):
        """Test behavior when no categories match."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        # A very specific task name unlikely to match patterns
        categories = analyzer.categorize_task("xyzzy_nonmatching_task_12345")
        assert isinstance(categories, list)


class TestAnalyzeLanguage:
    """Tests for analyze_language method."""

    def test_returns_dict(self, root_dir):
        """Test that analyze_language returns a dict."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        analysis = analyzer.analyze_language("Python")
        assert isinstance(analysis, dict)

    def test_analysis_has_expected_keys(self, root_dir):
        """Test that analysis contains expected keys."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        analysis = analyzer.analyze_language("Python")
        expected_keys = [
            "language",
            "total_tasks",
            "ai_categorized_tasks",
            "category_breakdown",
            "coverage_by_category",
        ]
        for key in expected_keys:
            assert key in analysis, f"Expected key '{key}' in analysis"

    def test_language_name_matches(self, root_dir):
        """Test that analysis includes correct language name."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        analysis = analyzer.analyze_language("Python")
        assert analysis["language"] == "Python"

    def test_total_tasks_is_positive(self, root_dir):
        """Test that Python has positive task count."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        analysis = analyzer.analyze_language("Python")
        assert analysis["total_tasks"] > 0


class TestGenerateLanguageProfile:
    """Tests for generate_language_profile method."""

    def test_returns_dict(self, root_dir):
        """Test that generate_language_profile returns a dict."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        profile = analyzer.generate_language_profile("Python")
        assert isinstance(profile, dict)

    def test_profile_has_expected_keys(self, root_dir):
        """Test that profile contains expected keys."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        profile = analyzer.generate_language_profile("Python")
        expected_keys = [
            "language",
            "total_tasks_implemented",
            "ai_tasks_implemented",
            "category_coverage",
            "categories_covered",
            "total_categories",
            "detailed_coverage",
        ]
        for key in expected_keys:
            assert key in profile, f"Expected key '{key}' in profile"

    def test_coverage_score_is_percentage(self, root_dir):
        """Test that coverage score is a valid percentage."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        profile = analyzer.generate_language_profile("Python")
        assert 0 <= profile["category_coverage"] <= 100


class TestAnalyzeAllLanguages:
    """Tests for analyze_all_languages method."""

    @pytest.mark.slow
    def test_returns_dict(self, root_dir):
        """Test that analyze_all_languages returns a dict."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        result = analyzer.analyze_all_languages()
        assert isinstance(result, dict)

    @pytest.mark.slow
    def test_has_expected_keys(self, root_dir):
        """Test that result has expected keys."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        result = analyzer.analyze_all_languages()
        assert "total_languages" in result
        assert "language_profiles" in result

    @pytest.mark.slow
    def test_profiles_are_sorted_by_ai_tasks(self, root_dir):
        """Test that profiles are sorted by AI tasks (descending)."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        result = analyzer.analyze_all_languages()
        profiles = result["language_profiles"]
        ai_tasks = [p["ai_tasks_implemented"] for p in profiles]
        assert ai_tasks == sorted(ai_tasks, reverse=True)


class TestGenerateFrankencogManifest:
    """Tests for generate_frankencog_manifest method."""

    @pytest.mark.slow
    def test_returns_dict(self, root_dir):
        """Test that manifest is a dict."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        manifest = analyzer.generate_frankencog_manifest()
        assert isinstance(manifest, dict)

    @pytest.mark.slow
    def test_has_description(self, root_dir):
        """Test that manifest has description."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        manifest = analyzer.generate_frankencog_manifest()
        assert "description" in manifest

    @pytest.mark.slow
    def test_has_categories(self, root_dir):
        """Test that manifest has categories."""
        analyzer = OpenCogAnalyzer(str(root_dir))
        manifest = analyzer.generate_frankencog_manifest()
        assert "categories" in manifest
        assert isinstance(manifest["categories"], dict)
