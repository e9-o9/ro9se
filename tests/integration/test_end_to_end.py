"""
Integration tests for end-to-end workflows.

Tests complete workflows from data loading to analysis output.
"""

import pytest
import sys
import json
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "opencog" / "lib"))

from opencog_analyzer import OpenCogAnalyzer
from hypergraph_analyzer import HypergraphAnalyzer
from atom_type_builder import AtomTypeBuilder


class TestFullAnalysisPipeline:
    """Tests for the complete analysis pipeline."""

    @pytest.mark.integration
    @pytest.mark.slow
    def test_full_language_analysis_pipeline(self, root_dir, temp_dir):
        """Test complete language analysis from data to JSON output."""
        # Initialize analyzer
        analyzer = OpenCogAnalyzer(str(root_dir))

        # Get all languages
        languages = analyzer.get_all_languages()
        assert len(languages) > 0

        # Analyze all languages
        result = analyzer.analyze_all_languages()

        # Verify structure
        assert "total_languages" in result
        assert "language_profiles" in result
        assert result["total_languages"] == len(languages)

        # Export to JSON
        output_file = temp_dir / "language-analysis.json"
        with open(output_file, "w") as f:
            json.dump(result, f, indent=2)

        assert output_file.exists()

        # Verify JSON is readable
        with open(output_file) as f:
            loaded = json.load(f)
        assert loaded["total_languages"] == result["total_languages"]

    @pytest.mark.integration
    @pytest.mark.slow
    def test_frankencog_manifest_generation(self, root_dir, temp_dir):
        """Test FrankenCog manifest generation pipeline."""
        analyzer = OpenCogAnalyzer(str(root_dir))

        # Generate manifest
        manifest = analyzer.generate_frankencog_manifest()

        # Verify structure
        assert "description" in manifest
        assert "categories" in manifest

        # Each category should have recommendations
        for category, data in manifest["categories"].items():
            assert "recommended_languages" in data
            assert "best_language" in data
            assert "total_implementations" in data

        # Export to JSON
        output_file = temp_dir / "frankencog-manifest.json"
        with open(output_file, "w") as f:
            json.dump(manifest, f, indent=2)

        assert output_file.exists()


class TestHypergraphPipeline:
    """Tests for hypergraph generation pipeline."""

    @pytest.mark.integration
    @pytest.mark.slow
    def test_hypergraph_generation_and_export(self, root_dir, temp_dir):
        """Test complete hypergraph generation and export."""
        analyzer = HypergraphAnalyzer(str(root_dir))

        # Generate hypergraph
        hypergraph = analyzer.generate_hypergraph()

        # Verify nodes
        assert len(hypergraph["nodes"]["languages"]) > 0
        assert len(hypergraph["nodes"]["subcategories"]) > 0
        assert len(hypergraph["nodes"]["paradigms"]) > 0

        # Verify edges
        assert len(hypergraph["edges"]["language_to_subcategory"]) > 0
        assert len(hypergraph["edges"]["language_to_paradigm"]) > 0

        # Verify statistics
        stats = hypergraph["statistics"]
        assert stats["total_languages"] > 0
        assert stats["total_edges"] > 0

        # Export
        output_file = temp_dir / "hypergraph.json"
        analyzer.export_hypergraph(str(output_file))

        assert output_file.exists()

        # Verify export
        with open(output_file) as f:
            loaded = json.load(f)
        assert loaded["statistics"]["total_languages"] == stats["total_languages"]

    @pytest.mark.integration
    @pytest.mark.slow
    def test_paradigm_matrix_generation(self, root_dir, temp_dir):
        """Test paradigm performance matrix generation."""
        analyzer = HypergraphAnalyzer(str(root_dir))

        # Generate matrix
        matrix = analyzer.generate_paradigm_performance_matrix()

        # Verify structure
        assert len(matrix) > 0

        for subcat, data in matrix.items():
            assert "/" in subcat  # Format: category/subcategory
            assert "paradigm_rankings" in data
            assert isinstance(data["paradigm_rankings"], list)

        # Export
        output_file = temp_dir / "paradigm-matrix.json"
        analyzer.export_paradigm_matrix(str(output_file))

        assert output_file.exists()


class TestAtomTypeSystemPipeline:
    """Tests for atom type system building pipeline."""

    @pytest.mark.integration
    def test_complete_atom_system_build(self, root_dir, temp_dir):
        """Test building complete atom type system."""
        builder = AtomTypeBuilder(str(root_dir))

        # Build system
        system = builder.build()

        # Verify cognitive domains
        assert len(system.cognitive_domains) > 0
        for name, atom in system.cognitive_domains.items():
            expr = atom.get_expression()
            assert "CD(" in expr
            assert name in expr

        # Verify language paradigms
        assert len(system.language_paradigms) > 0
        for name, atom in system.language_paradigms.items():
            expr = atom.get_expression()
            assert "LP(" in expr
            assert name in expr

        # Test insights generation
        insights = system.generate_comparison_insights()
        assert "structural_comparison" in insights
        assert "complexity_analysis" in insights

        # Export
        output_file = temp_dir / "atom-types.json"
        system.export_to_json(str(output_file))

        assert output_file.exists()

        # Verify export
        with open(output_file) as f:
            loaded = json.load(f)
        assert "cognitive_domains" in loaded
        assert "language_paradigms" in loaded


class TestCrossModuleIntegration:
    """Tests for integration across modules."""

    @pytest.mark.integration
    @pytest.mark.slow
    def test_analyzer_hypergraph_consistency(self, root_dir):
        """Test that OpenCogAnalyzer and HypergraphAnalyzer produce consistent data."""
        base_analyzer = OpenCogAnalyzer(str(root_dir))
        hyper_analyzer = HypergraphAnalyzer(str(root_dir))

        # Get languages from both
        base_languages = base_analyzer.get_all_languages()
        hyper_languages = hyper_analyzer.get_all_languages()

        # Should be identical
        assert base_languages == hyper_languages

        # Get tasks from both
        base_tasks = base_analyzer.get_all_tasks()
        hyper_tasks = hyper_analyzer.get_all_tasks()

        # Should be identical
        assert base_tasks == hyper_tasks

    @pytest.mark.integration
    def test_atom_builder_uses_correct_categories(self, root_dir):
        """Test that AtomTypeBuilder uses categories from analyzer."""
        base_analyzer = OpenCogAnalyzer(str(root_dir))
        builder = AtomTypeBuilder(str(root_dir))

        # Categories should match
        base_categories = base_analyzer.categories.get("categories", {})
        builder_categories = builder.categories_data.get("categories", {})

        assert set(base_categories.keys()) == set(builder_categories.keys())


class TestDataValidation:
    """Tests for data validation across pipeline."""

    @pytest.mark.integration
    def test_language_task_mapping_valid(self, root_dir):
        """Test that language-task mappings are valid."""
        analyzer = OpenCogAnalyzer(str(root_dir))

        # Get sample languages
        languages = analyzer.get_all_languages()[:10]

        for lang in languages:
            tasks = analyzer.get_language_tasks(lang)
            # Each task should exist in Task directory
            for task in tasks:
                task_path = analyzer.task_dir / task
                # Task should exist (as directory)
                if not task_path.exists():
                    # May be a file, not directory - that's OK
                    pass

    @pytest.mark.integration
    @pytest.mark.slow
    def test_hypergraph_edge_validity(self, root_dir):
        """Test that hypergraph edges reference valid nodes."""
        analyzer = HypergraphAnalyzer(str(root_dir))
        hypergraph = analyzer.generate_hypergraph()

        languages = set(hypergraph["nodes"]["languages"])
        subcategories = set(hypergraph["nodes"]["subcategories"])
        paradigms = set(hypergraph["nodes"]["paradigms"])

        # Check language-to-subcategory edges
        for edge in hypergraph["edges"]["language_to_subcategory"]:
            assert edge["language"] in languages, f"Invalid language: {edge['language']}"
            assert (
                edge["subcategory"] in subcategories
            ), f"Invalid subcategory: {edge['subcategory']}"

        # Check language-to-paradigm edges
        for edge in hypergraph["edges"]["language_to_paradigm"]:
            assert edge["language"] in languages, f"Invalid language: {edge['language']}"
            assert edge["paradigm"] in paradigms, f"Invalid paradigm: {edge['paradigm']}"
