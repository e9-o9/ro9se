"""
Unit tests for hypergraph_analyzer.py

Tests the HypergraphAnalyzer class which extends OpenCogAnalyzer with
subcategory analysis and hypergraph generation capabilities.
"""

import pytest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "opencog" / "lib"))

from hypergraph_analyzer import HypergraphAnalyzer


class TestHypergraphAnalyzerInit:
    """Tests for HypergraphAnalyzer initialization."""

    def test_inherits_from_opencog_analyzer(self, root_dir):
        """Test that HypergraphAnalyzer inherits correctly."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        assert hasattr(analyzer, "get_all_languages")
        assert hasattr(analyzer, "get_all_tasks")
        assert hasattr(analyzer, "analyze_language")

    def test_has_paradigms(self, root_dir):
        """Test that paradigms are loaded."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        assert hasattr(analyzer, "paradigms")
        assert isinstance(analyzer.paradigms, dict)


class TestGetSubcategories:
    """Tests for get_subcategories method."""

    def test_returns_dict(self, root_dir):
        """Test that get_subcategories returns a dict."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        subcats = analyzer.get_subcategories("symbolic_reasoning")
        assert isinstance(subcats, dict)

    def test_nonexistent_category_returns_empty(self, root_dir):
        """Test that nonexistent category returns empty dict."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        subcats = analyzer.get_subcategories("nonexistent_category_12345")
        assert subcats == {}


class TestCategorizeTaskWithSubcategory:
    """Tests for categorize_task_with_subcategory method."""

    def test_returns_list_of_tuples(self, root_dir):
        """Test that method returns list of tuples."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        results = analyzer.categorize_task_with_subcategory("Sorting_algorithms")
        assert isinstance(results, list)
        for item in results:
            assert isinstance(item, tuple)
            assert len(item) == 2


class TestAnalyzeLanguageSubcategories:
    """Tests for analyze_language_subcategories method."""

    def test_returns_dict(self, root_dir):
        """Test that method returns a dict."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        result = analyzer.analyze_language_subcategories("Python")
        assert isinstance(result, dict)

    def test_keys_are_tuples(self, root_dir):
        """Test that dict keys are (category, subcategory) tuples."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        result = analyzer.analyze_language_subcategories("Python")
        for key in result.keys():
            assert isinstance(key, tuple)
            assert len(key) == 2

    def test_values_are_lists(self, root_dir):
        """Test that dict values are lists of tasks."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        result = analyzer.analyze_language_subcategories("Python")
        for value in result.values():
            assert isinstance(value, list)


class TestGetLanguageParadigms:
    """Tests for get_language_paradigms method."""

    def test_returns_list(self, root_dir):
        """Test that method returns a list."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        paradigms = analyzer.get_language_paradigms("Python")
        assert isinstance(paradigms, list)

    def test_python_has_paradigms(self, root_dir):
        """Test that Python has multiple paradigms."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        paradigms = analyzer.get_language_paradigms("Python")
        # Python is multi-paradigm
        assert len(paradigms) >= 1

    def test_unknown_language_returns_empty(self, root_dir):
        """Test that unknown language returns empty list."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        paradigms = analyzer.get_language_paradigms("UnknownLang12345")
        assert paradigms == []


class TestGenerateHypergraph:
    """Tests for generate_hypergraph method."""

    @pytest.mark.slow
    def test_returns_dict(self, root_dir):
        """Test that hypergraph is a dict."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        hypergraph = analyzer.generate_hypergraph()
        assert isinstance(hypergraph, dict)

    @pytest.mark.slow
    def test_has_nodes(self, root_dir):
        """Test that hypergraph has nodes."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        hypergraph = analyzer.generate_hypergraph()
        assert "nodes" in hypergraph
        assert isinstance(hypergraph["nodes"], dict)

    @pytest.mark.slow
    def test_nodes_has_categories(self, root_dir):
        """Test that nodes have language, subcategory, and paradigm categories."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        hypergraph = analyzer.generate_hypergraph()
        nodes = hypergraph["nodes"]
        assert "languages" in nodes
        assert "subcategories" in nodes
        assert "paradigms" in nodes

    @pytest.mark.slow
    def test_has_edges(self, root_dir):
        """Test that hypergraph has edges."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        hypergraph = analyzer.generate_hypergraph()
        assert "edges" in hypergraph
        assert isinstance(hypergraph["edges"], dict)

    @pytest.mark.slow
    def test_edges_has_categories(self, root_dir):
        """Test that edges have the expected categories."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        hypergraph = analyzer.generate_hypergraph()
        edges = hypergraph["edges"]
        assert "language_to_subcategory" in edges
        assert "language_to_paradigm" in edges
        assert "subcategory_performance" in edges

    @pytest.mark.slow
    def test_has_statistics(self, root_dir):
        """Test that hypergraph has statistics."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        hypergraph = analyzer.generate_hypergraph()
        assert "statistics" in hypergraph
        stats = hypergraph["statistics"]
        assert "total_languages" in stats
        assert "total_subcategories" in stats
        assert "total_paradigms" in stats
        assert "total_edges" in stats


class TestGenerateParadigmPerformanceMatrix:
    """Tests for generate_paradigm_performance_matrix method."""

    @pytest.mark.slow
    def test_returns_dict(self, root_dir):
        """Test that matrix is a dict."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        matrix = analyzer.generate_paradigm_performance_matrix()
        assert isinstance(matrix, dict)

    @pytest.mark.slow
    def test_keys_are_subcategories(self, root_dir):
        """Test that keys are subcategory identifiers."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        matrix = analyzer.generate_paradigm_performance_matrix()
        for key in matrix.keys():
            assert "/" in key  # Subcategories are formatted as "category/subcategory"

    @pytest.mark.slow
    def test_values_have_paradigm_rankings(self, root_dir):
        """Test that values contain paradigm rankings."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        matrix = analyzer.generate_paradigm_performance_matrix()
        for value in matrix.values():
            assert "paradigm_rankings" in value
            assert isinstance(value["paradigm_rankings"], list)


class TestExportHypergraph:
    """Tests for export_hypergraph method."""

    @pytest.mark.slow
    def test_exports_json_file(self, root_dir, temp_dir):
        """Test that hypergraph is exported to JSON file."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        output_file = temp_dir / "test_hypergraph.json"
        analyzer.export_hypergraph(str(output_file))
        assert output_file.exists()

    @pytest.mark.slow
    def test_exported_file_is_valid_json(self, root_dir, temp_dir):
        """Test that exported file is valid JSON."""
        import json

        analyzer = HypergraphAnalyzer(str(root_dir))
        output_file = temp_dir / "test_hypergraph.json"
        analyzer.export_hypergraph(str(output_file))

        with open(output_file) as f:
            data = json.load(f)
        assert isinstance(data, dict)


class TestExportParadigmMatrix:
    """Tests for export_paradigm_matrix method."""

    @pytest.mark.slow
    def test_exports_json_file(self, root_dir, temp_dir):
        """Test that matrix is exported to JSON file."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        output_file = temp_dir / "test_matrix.json"
        analyzer.export_paradigm_matrix(str(output_file))
        assert output_file.exists()

    @pytest.mark.slow
    def test_exported_file_is_valid_json(self, root_dir, temp_dir):
        """Test that exported file is valid JSON."""
        import json

        analyzer = HypergraphAnalyzer(str(root_dir))
        output_file = temp_dir / "test_matrix.json"
        analyzer.export_paradigm_matrix(str(output_file))

        with open(output_file) as f:
            data = json.load(f)
        assert isinstance(data, dict)
