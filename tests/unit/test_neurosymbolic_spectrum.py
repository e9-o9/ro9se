"""
Unit tests for the neuro-symbolic spectrum analyzer.
"""

import pytest
from pathlib import Path
from opencog.lib.neurosymbolic_spectrum import (
    NeuroSymbolicAnalyzer,
    SpectrumPosition,
    SpectrumFeatures,
    LanguageSpectrumProfile
)


@pytest.fixture
def analyzer():
    """Create analyzer instance for tests."""
    return NeuroSymbolicAnalyzer('.')


class TestSpectrumFeatures:
    """Test the SpectrumFeatures class."""
    
    def test_neural_score_calculation(self):
        """Test that neural score is calculated correctly."""
        features = SpectrumFeatures(
            structure_preservation=1.0,
            meaning_distribution=0.9,
            evaluation_deferral=1.0,
            control_model=0.8,
            syntactic_topology=1.0,
            semantic_model=0.9
        )
        
        # Should average all 6 features
        expected = (1.0 + 0.9 + 1.0 + 0.8 + 1.0 + 0.9) / 6.0
        assert abs(features.neural_score - expected) < 0.01
    
    def test_position_from_score(self):
        """Test position calculation from neural score."""
        # Neural extreme
        features = SpectrumFeatures(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
        assert features.neural_score == 1.0
        assert features.position == SpectrumPosition.NEURAL_EXTREME
        
        # Symbolic extreme
        features = SpectrumFeatures(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
        assert features.neural_score == 0.0
        assert features.position == SpectrumPosition.SYMBOLIC_EXTREME
        
        # Equilibrium
        features = SpectrumFeatures(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
        assert features.position == SpectrumPosition.EQUILIBRIUM
    
    def test_features_to_dict(self):
        """Test feature serialization."""
        features = SpectrumFeatures(0.8, 0.7, 0.9, 0.6, 0.8, 0.7)
        data = features.to_dict()
        
        assert 'structure_preservation' in data
        assert 'neural_score' in data
        assert 'position' in data
        assert data['structure_preservation'] == 0.8


class TestNeuroSymbolicAnalyzer:
    """Test the NeuroSymbolicAnalyzer class."""
    
    def test_predefined_profiles_exist(self, analyzer):
        """Test that predefined language profiles exist."""
        key_languages = ['Lisp', 'Scheme', 'C', 'Prolog', 'Haskell', 'Python']
        
        for lang in key_languages:
            assert lang in analyzer.LANGUAGE_PROFILES
    
    def test_analyze_lisp(self, analyzer):
        """Test Lisp analysis - should be neural-aligned."""
        profile = analyzer.analyze_language('Lisp')
        
        assert profile.language == 'Lisp'
        assert profile.features.neural_score > 0.8
        assert profile.position in [SpectrumPosition.NEURAL_EXTREME, SpectrumPosition.NEURAL_LEANING]
        assert profile.homoiconic is True
        assert profile.has_explicit_topology is True
        assert profile.preserves_possibility is True
    
    def test_analyze_c(self, analyzer):
        """Test C analysis - should be symbolic-leaning."""
        profile = analyzer.analyze_language('C')
        
        assert profile.language == 'C'
        assert profile.features.neural_score < 0.3
        assert profile.position == SpectrumPosition.SYMBOLIC_LEANING
        assert profile.homoiconic is False
        assert profile.preserves_possibility is False
    
    def test_analyze_prolog(self, analyzer):
        """Test Prolog analysis - should be balanced neural."""
        profile = analyzer.analyze_language('Prolog')
        
        assert profile.language == 'Prolog'
        assert 0.6 < profile.features.neural_score < 0.9
        assert profile.position in [SpectrumPosition.BALANCED_NEURAL, SpectrumPosition.NEURAL_LEANING]
    
    def test_analyze_assembly(self, analyzer):
        """Test Assembly analysis - should be symbolic extreme."""
        profile = analyzer.analyze_language('Assembly')
        
        assert profile.features.neural_score == 0.0
        assert profile.position == SpectrumPosition.SYMBOLIC_EXTREME
    
    def test_paradigm_estimation(self, analyzer):
        """Test paradigm-based feature estimation."""
        # Functional paradigm should be neural-leaning
        features = analyzer._estimate_from_paradigm('functional')
        assert features.neural_score > 0.5
        
        # Imperative paradigm should be symbolic-leaning
        features = analyzer._estimate_from_paradigm('imperative')
        assert features.neural_score < 0.5
        
        # Logic paradigm should be balanced-neural
        features = analyzer._estimate_from_paradigm('logic')
        assert features.neural_score > 0.6
    
    def test_analyze_unknown_language(self, analyzer):
        """Test analysis of language not in predefined profiles."""
        profile = analyzer.analyze_language('UnknownLang')
        
        # Should return balanced default
        assert profile.features.neural_score == 0.5
        assert profile.position == SpectrumPosition.EQUILIBRIUM
    
    def test_profile_serialization(self, analyzer):
        """Test profile export to dictionary."""
        profile = analyzer.analyze_language('Lisp')
        data = profile.to_dict()
        
        assert 'language' in data
        assert 'position' in data
        assert 'neural_score' in data
        assert 'features' in data
        assert 'characteristics' in data
        assert 'rationale' in data
        assert data['language'] == 'Lisp'
    
    def test_analyze_multiple_languages(self, analyzer):
        """Test analyzing multiple languages."""
        languages = ['Lisp', 'C', 'Python', 'Haskell']
        results = analyzer.analyze_all(languages)
        
        assert len(results) == len(languages)
        for lang in languages:
            assert lang in results
            assert results[lang].language == lang
    
    def test_spectrum_distribution(self, analyzer):
        """Test spectrum distribution calculation."""
        languages = ['Lisp', 'C', 'Prolog', 'Python', 'Assembly']
        profiles = [analyzer.analyze_language(lang) for lang in languages]
        distribution = analyzer.get_spectrum_distribution(profiles)
        
        # Should have counts for all positions
        assert sum(distribution.values()) == len(languages)
        
        # Assembly should be in symbolic_extreme
        assert distribution['symbolic_extreme'] >= 1
    
    def test_export_functionality(self, analyzer):
        """Test export to dictionary."""
        languages = ['Lisp', 'C', 'Prolog']
        profiles = [analyzer.analyze_language(lang) for lang in languages]
        export_data = analyzer.export_to_dict(profiles)
        
        assert 'total_languages' in export_data
        assert 'distribution' in export_data
        assert 'profiles' in export_data
        assert export_data['total_languages'] == len(languages)


class TestLanguageSpectrumProfile:
    """Test the LanguageSpectrumProfile class."""
    
    def test_profile_creation(self, analyzer):
        """Test profile object creation."""
        profile = analyzer.analyze_language('Lisp')
        
        assert isinstance(profile, LanguageSpectrumProfile)
        assert profile.language == 'Lisp'
        assert isinstance(profile.features, SpectrumFeatures)
        assert isinstance(profile.position, SpectrumPosition)
        assert len(profile.rationale) > 0
    
    def test_characteristics(self, analyzer):
        """Test language characteristics."""
        # Lisp characteristics
        lisp = analyzer.analyze_language('Lisp')
        assert lisp.homoiconic is True
        assert lisp.has_explicit_topology is True
        assert lisp.supports_metaprogramming is True
        assert lisp.preserves_possibility is True
        
        # C characteristics
        c = analyzer.analyze_language('C')
        assert c.homoiconic is False
        assert c.has_explicit_topology is False
        assert c.preserves_possibility is False


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
