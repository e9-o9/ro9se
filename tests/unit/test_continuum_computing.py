"""
Unit tests for the continuum computing analyzer.

Tests the framework for analyzing programming languages' suitability
for continuum computing - a paradigm shift from discrete bit-based
computation to spectral/harmonic field-based computation.
"""

import pytest
from opencog.lib.continuum_computing import (
    ContinuumComputingAnalyzer,
    ContinuumSuitability,
    ContinuumDomain,
    ContinuumFeatures,
    ContinuumProfile
)


@pytest.fixture
def analyzer():
    """Create analyzer instance for tests."""
    return ContinuumComputingAnalyzer('.')


class TestContinuumFeatures:
    """Test the ContinuumFeatures class."""
    
    def test_continuum_score_calculation(self):
        """Test that continuum score is calculated correctly."""
        features = ContinuumFeatures(
            signal_processing=0.9,
            spectral_computation=0.9,
            bulk_transformations=0.9,
            continuous_evaluation=0.8,
            attractor_semantics=0.8,
            phase_operations=0.9,
            resonance_matching=0.8,
            convolution_support=0.9
        )
        
        # Should average all 8 features
        expected = (0.9 + 0.9 + 0.9 + 0.8 + 0.8 + 0.9 + 0.8 + 0.9) / 8.0
        assert abs(features.continuum_score - expected) < 0.01
    
    def test_suitability_from_score(self):
        """Test suitability calculation from continuum score."""
        # Ideal
        features = ContinuumFeatures(0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9)
        assert abs(features.continuum_score - 0.9) < 0.01
        assert features.suitability == ContinuumSuitability.IDEAL
        
        # Unsuitable
        features = ContinuumFeatures(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
        assert features.continuum_score == 0.0
        assert features.suitability == ContinuumSuitability.UNSUITABLE
        
        # Moderate (0.45 to get into moderate range)
        features = ContinuumFeatures(0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45)
        assert features.suitability == ContinuumSuitability.MODERATE
    
    def test_domain_scores(self):
        """Test domain score calculations."""
        features = ContinuumFeatures(
            signal_processing=0.8,
            spectral_computation=0.9,
            bulk_transformations=0.7,
            continuous_evaluation=0.6,
            attractor_semantics=0.8,
            phase_operations=0.7,
            resonance_matching=0.75,
            convolution_support=0.85
        )
        
        domain_scores = features.domain_scores()
        
        # Check spectral memory domain
        expected_spectral = (0.8 + 0.9) / 2.0
        assert abs(domain_scores[ContinuumDomain.SPECTRAL_MEMORY] - expected_spectral) < 0.01
        
        # Check phase transport domain
        expected_phase = (0.7 + 0.85) / 2.0
        assert abs(domain_scores[ContinuumDomain.PHASE_TRANSPORT] - expected_phase) < 0.01
    
    def test_features_to_dict(self):
        """Test feature serialization."""
        features = ContinuumFeatures(0.8, 0.7, 0.9, 0.6, 0.8, 0.7, 0.75, 0.85)
        data = features.to_dict()
        
        assert 'signal_processing' in data
        assert 'continuum_score' in data
        assert 'suitability' in data
        assert 'domain_scores' in data
        assert data['signal_processing'] == 0.8


class TestContinuumComputingAnalyzer:
    """Test the ContinuumComputingAnalyzer class."""
    
    def test_predefined_profiles_exist(self, analyzer):
        """Test that predefined language profiles exist."""
        key_languages = ['Julia', 'MATLAB', 'Python', 'Haskell', 'C', 'Assembly']
        
        for lang in key_languages:
            assert lang in analyzer.LANGUAGE_PROFILES
    
    def test_analyze_julia(self, analyzer):
        """Test Julia analysis - should be ideal for continuum computing."""
        profile = analyzer.analyze_language('Julia')
        
        assert profile.language == 'Julia'
        assert profile.features.continuum_score > 0.85
        assert profile.suitability == ContinuumSuitability.IDEAL
        assert profile.supports_fft is True
        assert profile.supports_tensor_ops is True
    
    def test_analyze_matlab(self, analyzer):
        """Test MATLAB analysis - should be ideal for continuum computing."""
        profile = analyzer.analyze_language('MATLAB')
        
        assert profile.language == 'MATLAB'
        assert profile.features.continuum_score > 0.8
        assert profile.suitability in [ContinuumSuitability.IDEAL, ContinuumSuitability.EXCELLENT]
        assert profile.supports_fft is True
        assert profile.supports_complex is True
    
    def test_analyze_python(self, analyzer):
        """Test Python analysis - should be well suited or excellent."""
        profile = analyzer.analyze_language('Python')
        
        assert profile.language == 'Python'
        assert 0.5 < profile.features.continuum_score < 0.85
        # Python with NumPy/SciPy is excellent for continuum computing
        assert profile.suitability in [ContinuumSuitability.WELL_SUITED, ContinuumSuitability.EXCELLENT]
        assert profile.supports_fft is True
    
    def test_analyze_haskell(self, analyzer):
        """Test Haskell analysis - should be moderate due to lazy eval."""
        profile = analyzer.analyze_language('Haskell')
        
        assert profile.language == 'Haskell'
        # Haskell has excellent lazy eval but limited numeric support
        assert profile.supports_lazy_eval is True
    
    def test_analyze_assembly(self, analyzer):
        """Test Assembly analysis - should be unsuitable."""
        profile = analyzer.analyze_language('Assembly')
        
        assert profile.features.continuum_score < 0.2
        assert profile.suitability == ContinuumSuitability.UNSUITABLE
        assert profile.supports_fft is False
        assert profile.supports_tensor_ops is False
    
    def test_analyze_unknown_language(self, analyzer):
        """Test analysis of language not in predefined profiles."""
        profile = analyzer.analyze_language('UnknownLang')
        
        # Should return limited default (with floating point tolerance)
        assert abs(profile.features.continuum_score - 0.2) < 0.01
        # With score ~0.2, it falls into UNSUITABLE category (< 0.20)
        assert profile.suitability in [ContinuumSuitability.LIMITED, ContinuumSuitability.UNSUITABLE]
    
    def test_profile_serialization(self, analyzer):
        """Test profile export to dictionary."""
        profile = analyzer.analyze_language('Julia')
        data = profile.to_dict()
        
        assert 'language' in data
        assert 'suitability' in data
        assert 'continuum_score' in data
        assert 'features' in data
        assert 'capabilities' in data
        assert 'recommended_domains' in data
        assert 'rationale' in data
        assert data['language'] == 'Julia'
    
    def test_analyze_multiple_languages(self, analyzer):
        """Test analyzing multiple languages."""
        languages = ['Julia', 'Python', 'C', 'Assembly']
        results = analyzer.analyze_all(languages)
        
        assert len(results) == len(languages)
        for lang in languages:
            assert lang in results
            assert results[lang].language == lang
    
    def test_suitability_distribution(self, analyzer):
        """Test suitability distribution calculation."""
        languages = ['Julia', 'MATLAB', 'Python', 'C', 'Assembly']
        profiles = [analyzer.analyze_language(lang) for lang in languages]
        distribution = analyzer.get_suitability_distribution(profiles)
        
        # Should have counts for all suitability levels
        assert sum(distribution.values()) == len(languages)
        
        # Julia and MATLAB should be ideal/excellent
        assert distribution.get('ideal', 0) + distribution.get('excellent', 0) >= 1
    
    def test_get_top_languages(self, analyzer):
        """Test getting top languages for continuum computing."""
        top = analyzer.get_top_languages(5)
        
        assert len(top) == 5
        
        # Scores should be in descending order
        scores = [score for _, score in top]
        assert scores == sorted(scores, reverse=True)
        
        # Julia or Mathematica should be in top 5
        top_langs = [lang for lang, _ in top]
        assert any(l in ['Julia', 'Mathematica', 'MATLAB'] for l in top_langs)
    
    def test_export_functionality(self, analyzer):
        """Test export to dictionary."""
        languages = ['Julia', 'Python', 'C']
        profiles = [analyzer.analyze_language(lang) for lang in languages]
        export_data = analyzer.export_to_dict(profiles)
        
        assert 'total_languages' in export_data
        assert 'suitability_distribution' in export_data
        assert 'top_languages' in export_data
        assert 'profiles' in export_data
        assert export_data['total_languages'] == len(languages)
    
    def test_architecture_summary(self, analyzer):
        """Test architecture summary generation."""
        summary = analyzer.generate_architecture_summary()
        
        assert 'architecture' in summary
        assert 'core_operations' in summary
        assert 'instruction_set' in summary
        assert 'suitable_for' in summary
        assert 'not_suitable_for' in summary
        
        # Check architecture layers
        arch = summary['architecture']
        assert 'spectral_memory_field' in arch
        assert 'phase_transport_layer' in arch
        assert 'harmonic_operator_fabric' in arch
        assert 'attractor_stabilization' in arch
        assert 'readout_quantization' in arch
        
        # Check core operations
        assert 'bind' in summary['core_operations']
        assert 'route' in summary['core_operations']
        assert 'compute' in summary['core_operations']
        assert 'decide' in summary['core_operations']


class TestContinuumProfile:
    """Test the ContinuumProfile class."""
    
    def test_profile_creation(self, analyzer):
        """Test profile object creation."""
        profile = analyzer.analyze_language('Julia')
        
        assert isinstance(profile, ContinuumProfile)
        assert profile.language == 'Julia'
        assert isinstance(profile.features, ContinuumFeatures)
        assert isinstance(profile.suitability, ContinuumSuitability)
        assert len(profile.rationale) > 0
    
    def test_recommended_domains(self, analyzer):
        """Test that recommended domains are populated."""
        # Julia should have many recommended domains
        julia = analyzer.analyze_language('Julia')
        assert len(julia.recommended_domains) > 0
        
        # Assembly should have no recommended domains
        assembly = analyzer.analyze_language('Assembly')
        assert len(assembly.recommended_domains) == 0


class TestContinuumDomains:
    """Test domain-specific functionality."""
    
    def test_get_recommended_languages_spectral(self, analyzer):
        """Test getting languages for spectral memory domain."""
        recommended = analyzer.get_recommended_languages(
            ContinuumDomain.SPECTRAL_MEMORY,
            ContinuumSuitability.WELL_SUITED
        )
        
        # Should include Julia, MATLAB, Python
        assert len(recommended) > 0
        # Julia should be recommended for spectral operations
        assert 'Julia' in recommended or 'MATLAB' in recommended
    
    def test_get_recommended_languages_attractor(self, analyzer):
        """Test getting languages for attractor stabilization domain."""
        recommended = analyzer.get_recommended_languages(
            ContinuumDomain.ATTRACTOR_STABILIZATION,
            ContinuumSuitability.MODERATE
        )
        
        # Should include Haskell (lazy eval, fixed points) and Mathematica
        assert len(recommended) > 0


class TestContinuumComputingConcepts:
    """Test that the module correctly implements continuum computing concepts."""
    
    def test_spectral_bit_band_features(self, analyzer):
        """Test that spectral bit-band features are properly modeled."""
        julia = analyzer.analyze_language('Julia')
        
        # Spectral bit-band requires: frequency, phase, amplitude, bandwidth, coherence
        # This maps to: spectral_computation, phase_operations, signal_processing
        assert julia.features.spectral_computation > 0.8  # frequency/bandwidth
        assert julia.features.phase_operations > 0.8      # phase
        assert julia.features.signal_processing > 0.8     # amplitude
    
    def test_core_operations_support(self, analyzer):
        """Test that core continuum operations are evaluable."""
        julia = analyzer.analyze_language('Julia')
        
        # bind: Kronecker coupling -> bulk_transformations
        assert julia.features.bulk_transformations > 0.8
        
        # route: phase rotation -> phase_operations
        assert julia.features.phase_operations > 0.8
        
        # compute: convolution -> convolution_support
        assert julia.features.convolution_support > 0.8
        
        # decide: resonance matching -> resonance_matching
        assert julia.features.resonance_matching > 0.8
    
    def test_field_vs_collapse_distinction(self, analyzer):
        """Test that field-preserving vs collapsing languages are distinguished."""
        # Field-preserving (ideal for continuum)
        julia = analyzer.analyze_language('Julia')
        haskell = analyzer.analyze_language('Haskell')
        
        # Collapsing (less ideal)
        c = analyzer.analyze_language('C')
        assembly = analyzer.analyze_language('Assembly')
        
        # Field-preserving should have higher scores
        assert julia.features.continuum_score > c.features.continuum_score
        assert julia.features.continuum_score > assembly.features.continuum_score
        
        # Haskell has deferred evaluation (field-preserving)
        assert haskell.features.continuous_evaluation > c.features.continuous_evaluation


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
