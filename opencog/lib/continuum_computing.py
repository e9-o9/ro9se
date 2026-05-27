"""
Continuum Computing Analyzer

Analyzes programming languages for their suitability in continuum computing -
a paradigm shift from discrete bit-based computation to spectral/harmonic
field-based computation.

Continuum computing replaces:
    bits → gates → clock steps → memory cells
with:
    spectral fields → harmonic coupling → phase transport → attractor stabilization

Instead of a bit (0/1), continuum computing uses a spectral bit-band B_i(ω, φ, A)
with frequency, phase, amplitude, bandwidth, and coherence.

Machine state becomes a signal field:
    Ψ(t) = Σ_i A_i(t)sin(ω_i t + φ_i(t))

Core operations:
    - bind(a,b) = a ⊗ b       (spectral entanglement / Kronecker coupling)
    - route(a) = e^(iθ)a      (phase rotation as address/path change)
    - compute(a,b) = a * b    (convolution as transformation)
    - decide(Ψ) = argmax_k |⟨Ψ, Φ_k⟩|  (readout by resonance against basis patterns)

Best suited languages for continuum computing:
    - Analog AI, control systems, perception
    - Physical simulation, signal reasoning
    - Semantic manifolds, recursive self-models

Not suited for:
    - Exact accounting, legal contracts
    - Anything requiring decimal certainty
"""

import logging
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum


logger = logging.getLogger(__name__)


class ContinuumSuitability(Enum):
    """Suitability level for continuum computing"""
    IDEAL = "ideal"                      # Perfect for continuum computing
    EXCELLENT = "excellent"              # Strongly suited for continuum computing
    WELL_SUITED = "well_suited"          # Good support for continuum paradigms
    MODERATE = "moderate"                # Some support, with limitations
    LIMITED = "limited"                  # Minimal support
    UNSUITABLE = "unsuitable"            # Not suited for continuum computing


class ContinuumDomain(Enum):
    """Application domains for continuum computing"""
    SPECTRAL_MEMORY = "spectral_memory"               # Stores harmonic attractors
    PHASE_TRANSPORT = "phase_transport"               # Gauge/phase transport layer
    HARMONIC_OPERATORS = "harmonic_operators"         # Convolution, interference, resonance
    ATTRACTOR_STABILIZATION = "attractor_stabilization"  # Fixed-point stabilization
    READOUT_QUANTIZATION = "readout_quantization"     # Collapse continuum to discrete


@dataclass
class ContinuumFeatures:
    """
    Features that determine a language's suitability for continuum computing.
    
    Based on the continuum computing architecture:
    - Spectral Memory Field: stores harmonic attractors
    - Gauge/Phase Transport Layer: preserves coherence across context transformations
    - Harmonic Operator Fabric: convolution, interference, resonance, cancellation
    - Attractor/Fixed-Point Layer: stabilizes outputs as μ-states
    - Readout/Quantization Layer: collapses continuum bands into discrete symbols
    """
    
    # Signal/waveform support (0.0 = none, 1.0 = native)
    signal_processing: float = 0.0
    
    # Complex number / Fourier transform support
    spectral_computation: float = 0.0
    
    # Array/tensor bulk operations
    bulk_transformations: float = 0.0
    
    # Lazy/stream evaluation (continuous rather than discrete)
    continuous_evaluation: float = 0.0
    
    # Fixed-point / attractor semantics support
    attractor_semantics: float = 0.0
    
    # Phase/rotation operation support
    phase_operations: float = 0.0
    
    # Resonance / pattern matching via similarity
    resonance_matching: float = 0.0
    
    # Convolution / field transformation operations
    convolution_support: float = 0.0
    
    @property
    def continuum_score(self) -> float:
        """Overall continuum computing suitability score (0.0-1.0)"""
        return (
            self.signal_processing +
            self.spectral_computation +
            self.bulk_transformations +
            self.continuous_evaluation +
            self.attractor_semantics +
            self.phase_operations +
            self.resonance_matching +
            self.convolution_support
        ) / 8.0
    
    @property
    def suitability(self) -> ContinuumSuitability:
        """Determine suitability level from features"""
        score = self.continuum_score
        
        if score >= 0.85:
            return ContinuumSuitability.IDEAL
        elif score >= 0.70:
            return ContinuumSuitability.EXCELLENT
        elif score >= 0.55:
            return ContinuumSuitability.WELL_SUITED
        elif score >= 0.40:
            return ContinuumSuitability.MODERATE
        elif score >= 0.20:
            return ContinuumSuitability.LIMITED
        else:
            return ContinuumSuitability.UNSUITABLE
    
    def domain_scores(self) -> Dict[ContinuumDomain, float]:
        """Calculate scores for each continuum computing domain"""
        return {
            ContinuumDomain.SPECTRAL_MEMORY: (
                self.signal_processing + self.spectral_computation
            ) / 2.0,
            ContinuumDomain.PHASE_TRANSPORT: (
                self.phase_operations + self.convolution_support
            ) / 2.0,
            ContinuumDomain.HARMONIC_OPERATORS: (
                self.convolution_support + self.resonance_matching + self.spectral_computation
            ) / 3.0,
            ContinuumDomain.ATTRACTOR_STABILIZATION: (
                self.attractor_semantics + self.continuous_evaluation
            ) / 2.0,
            ContinuumDomain.READOUT_QUANTIZATION: (
                self.bulk_transformations + self.resonance_matching
            ) / 2.0
        }
    
    def to_dict(self) -> Dict[str, Any]:
        """Export to dictionary"""
        return {
            "signal_processing": self.signal_processing,
            "spectral_computation": self.spectral_computation,
            "bulk_transformations": self.bulk_transformations,
            "continuous_evaluation": self.continuous_evaluation,
            "attractor_semantics": self.attractor_semantics,
            "phase_operations": self.phase_operations,
            "resonance_matching": self.resonance_matching,
            "convolution_support": self.convolution_support,
            "continuum_score": self.continuum_score,
            "suitability": self.suitability.value,
            "domain_scores": {k.value: v for k, v in self.domain_scores().items()}
        }


@dataclass
class ContinuumProfile:
    """Complete continuum computing profile for a language"""
    language: str
    features: ContinuumFeatures
    suitability: ContinuumSuitability
    rationale: str
    
    # Continuum computing capabilities
    supports_fft: bool = False
    supports_complex: bool = False
    supports_tensor_ops: bool = False
    supports_lazy_eval: bool = False
    supports_pattern_matching: bool = False
    
    # Recommended continuum domains
    recommended_domains: List[ContinuumDomain] = field(default_factory=list)
    
    def to_dict(self) -> Dict[str, Any]:
        """Export to dictionary"""
        return {
            "language": self.language,
            "suitability": self.suitability.value,
            "continuum_score": self.features.continuum_score,
            "features": self.features.to_dict(),
            "capabilities": {
                "supports_fft": self.supports_fft,
                "supports_complex": self.supports_complex,
                "supports_tensor_ops": self.supports_tensor_ops,
                "supports_lazy_eval": self.supports_lazy_eval,
                "supports_pattern_matching": self.supports_pattern_matching
            },
            "recommended_domains": [d.value for d in self.recommended_domains],
            "rationale": self.rationale
        }


class ContinuumComputingAnalyzer:
    """
    Analyzes languages for their suitability in continuum computing.
    
    Continuum computing is a paradigm where:
    - A "bit" is not 0/1 but a spectral packet B_i(ω, φ, A) with frequency,
      phase, amplitude, bandwidth, and coherence
    - Machine state is a signal field Ψ(t) = Σ_i A_i(t)sin(ω_i t + φ_i(t))
    - Computation is shaping a spectrum until the desired attractor resonates
    
    Architecture layers:
    1. Spectral Memory Field - stores harmonic attractors
    2. Gauge/Phase Transport Layer - preserves coherence across context transformations
    3. Harmonic Operator Fabric - convolution, interference, resonance, cancellation
    4. Attractor/Fixed-Point Layer - stabilizes outputs as μ-states
    5. Readout/Quantization Layer - collapses continuum bands into discrete symbols
    """
    
    # Predefined language profiles for continuum computing suitability
    LANGUAGE_PROFILES = {
        # IDEAL: Languages with native spectral/array operations
        "Julia": ContinuumFeatures(
            signal_processing=0.95,
            spectral_computation=0.95,
            bulk_transformations=0.95,
            continuous_evaluation=0.70,
            attractor_semantics=0.80,
            phase_operations=0.90,
            resonance_matching=0.85,
            convolution_support=0.95
        ),
        "MATLAB": ContinuumFeatures(
            signal_processing=0.95,
            spectral_computation=0.95,
            bulk_transformations=0.95,
            continuous_evaluation=0.50,
            attractor_semantics=0.75,
            phase_operations=0.90,
            resonance_matching=0.80,
            convolution_support=0.95
        ),
        "Octave": ContinuumFeatures(
            signal_processing=0.90,
            spectral_computation=0.90,
            bulk_transformations=0.90,
            continuous_evaluation=0.50,
            attractor_semantics=0.70,
            phase_operations=0.85,
            resonance_matching=0.75,
            convolution_support=0.90
        ),
        
        # EXCELLENT: Array/scientific languages
        "APL": ContinuumFeatures(
            signal_processing=0.70,
            spectral_computation=0.75,
            bulk_transformations=0.95,
            continuous_evaluation=0.60,
            attractor_semantics=0.70,
            phase_operations=0.70,
            resonance_matching=0.80,
            convolution_support=0.80
        ),
        "J": ContinuumFeatures(
            signal_processing=0.75,
            spectral_computation=0.80,
            bulk_transformations=0.95,
            continuous_evaluation=0.65,
            attractor_semantics=0.75,
            phase_operations=0.75,
            resonance_matching=0.85,
            convolution_support=0.85
        ),
        "K": ContinuumFeatures(
            signal_processing=0.70,
            spectral_computation=0.70,
            bulk_transformations=0.90,
            continuous_evaluation=0.65,
            attractor_semantics=0.70,
            phase_operations=0.70,
            resonance_matching=0.75,
            convolution_support=0.80
        ),
        "R": ContinuumFeatures(
            signal_processing=0.85,
            spectral_computation=0.85,
            bulk_transformations=0.85,
            continuous_evaluation=0.50,
            attractor_semantics=0.60,
            phase_operations=0.75,
            resonance_matching=0.80,
            convolution_support=0.85
        ),
        "Mathematica": ContinuumFeatures(
            signal_processing=0.90,
            spectral_computation=0.95,
            bulk_transformations=0.90,
            continuous_evaluation=0.85,
            attractor_semantics=0.90,
            phase_operations=0.90,
            resonance_matching=0.85,
            convolution_support=0.90
        ),
        
        # WELL_SUITED: Languages with good scientific computing support
        "Python": ContinuumFeatures(
            signal_processing=0.80,  # With NumPy/SciPy
            spectral_computation=0.85,  # With FFT libraries
            bulk_transformations=0.85,  # NumPy arrays
            continuous_evaluation=0.70,  # Generators
            attractor_semantics=0.50,
            phase_operations=0.75,
            resonance_matching=0.65,
            convolution_support=0.80
        ),
        "Fortran": ContinuumFeatures(
            signal_processing=0.85,
            spectral_computation=0.80,
            bulk_transformations=0.85,
            continuous_evaluation=0.30,
            attractor_semantics=0.40,
            phase_operations=0.75,
            resonance_matching=0.50,
            convolution_support=0.85
        ),
        "C++": ContinuumFeatures(
            signal_processing=0.75,  # With Eigen/FFTW
            spectral_computation=0.75,
            bulk_transformations=0.70,
            continuous_evaluation=0.50,  # Ranges
            attractor_semantics=0.40,
            phase_operations=0.70,
            resonance_matching=0.50,
            convolution_support=0.75
        ),
        "Rust": ContinuumFeatures(
            signal_processing=0.65,
            spectral_computation=0.65,
            bulk_transformations=0.60,
            continuous_evaluation=0.70,  # Iterators
            attractor_semantics=0.55,
            phase_operations=0.60,
            resonance_matching=0.55,
            convolution_support=0.65
        ),
        
        # MODERATE: Functional languages (good for deferred evaluation)
        "Haskell": ContinuumFeatures(
            signal_processing=0.40,
            spectral_computation=0.45,
            bulk_transformations=0.50,
            continuous_evaluation=0.95,  # Lazy evaluation
            attractor_semantics=0.80,  # Fixed points
            phase_operations=0.50,
            resonance_matching=0.60,
            convolution_support=0.45
        ),
        "Lisp": ContinuumFeatures(
            signal_processing=0.35,
            spectral_computation=0.40,
            bulk_transformations=0.45,
            continuous_evaluation=0.75,  # Deferred evaluation
            attractor_semantics=0.70,  # Recursion/fix
            phase_operations=0.45,
            resonance_matching=0.65,  # Pattern matching
            convolution_support=0.40
        ),
        "Scheme": ContinuumFeatures(
            signal_processing=0.35,
            spectral_computation=0.40,
            bulk_transformations=0.40,
            continuous_evaluation=0.80,  # Continuations
            attractor_semantics=0.75,
            phase_operations=0.45,
            resonance_matching=0.60,
            convolution_support=0.40
        ),
        "Clojure": ContinuumFeatures(
            signal_processing=0.50,
            spectral_computation=0.55,
            bulk_transformations=0.60,
            continuous_evaluation=0.85,  # Lazy sequences
            attractor_semantics=0.70,
            phase_operations=0.50,
            resonance_matching=0.60,
            convolution_support=0.50
        ),
        
        # LIMITED: General purpose languages
        "Java": ContinuumFeatures(
            signal_processing=0.40,
            spectral_computation=0.40,
            bulk_transformations=0.35,
            continuous_evaluation=0.30,  # Streams
            attractor_semantics=0.25,
            phase_operations=0.35,
            resonance_matching=0.30,
            convolution_support=0.40
        ),
        "C": ContinuumFeatures(
            signal_processing=0.50,  # Can use FFTW
            spectral_computation=0.40,
            bulk_transformations=0.30,
            continuous_evaluation=0.10,
            attractor_semantics=0.20,
            phase_operations=0.40,
            resonance_matching=0.20,
            convolution_support=0.50
        ),
        "Go": ContinuumFeatures(
            signal_processing=0.35,
            spectral_computation=0.35,
            bulk_transformations=0.30,
            continuous_evaluation=0.50,  # Goroutines
            attractor_semantics=0.30,
            phase_operations=0.30,
            resonance_matching=0.25,
            convolution_support=0.35
        ),
        
        # UNSUITABLE: Assembly, scripts without numeric support
        "Assembly": ContinuumFeatures(
            signal_processing=0.10,  # Manual
            spectral_computation=0.05,
            bulk_transformations=0.05,
            continuous_evaluation=0.00,
            attractor_semantics=0.00,
            phase_operations=0.05,
            resonance_matching=0.00,
            convolution_support=0.05
        ),
        "Bash": ContinuumFeatures(
            signal_processing=0.00,
            spectral_computation=0.00,
            bulk_transformations=0.05,
            continuous_evaluation=0.10,
            attractor_semantics=0.00,
            phase_operations=0.00,
            resonance_matching=0.05,
            convolution_support=0.00
        ),
    }
    
    def __init__(self, repo_path: str = "."):
        """Initialize analyzer"""
        self.repo_path = repo_path
        self.profiles_cache: Dict[str, ContinuumProfile] = {}
    
    def analyze_language(self, language: str) -> ContinuumProfile:
        """Analyze a specific language's continuum computing suitability"""
        
        # Check cache
        if language in self.profiles_cache:
            return self.profiles_cache[language]
        
        # Get features
        features = self._get_language_features(language)
        suitability = features.suitability
        
        # Determine capabilities
        capabilities = self._determine_capabilities(language, features)
        
        # Determine recommended domains
        domains = self._determine_recommended_domains(features)
        
        # Generate rationale
        rationale = self._generate_rationale(language, features, suitability)
        
        # Create profile
        profile = ContinuumProfile(
            language=language,
            features=features,
            suitability=suitability,
            rationale=rationale,
            recommended_domains=domains,
            **capabilities
        )
        
        # Cache and return
        self.profiles_cache[language] = profile
        return profile
    
    def _get_language_features(self, language: str) -> ContinuumFeatures:
        """Get or estimate language features"""
        
        # Check if we have a predefined profile
        if language in self.LANGUAGE_PROFILES:
            return self.LANGUAGE_PROFILES[language]
        
        # Default to limited
        return ContinuumFeatures(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
    
    def _determine_capabilities(
        self, 
        language: str, 
        features: ContinuumFeatures
    ) -> Dict[str, bool]:
        """Determine continuum computing capabilities"""
        
        return {
            "supports_fft": features.spectral_computation > 0.7,
            "supports_complex": features.spectral_computation > 0.6,
            "supports_tensor_ops": features.bulk_transformations > 0.6,
            "supports_lazy_eval": features.continuous_evaluation > 0.6,
            "supports_pattern_matching": features.resonance_matching > 0.6
        }
    
    def _determine_recommended_domains(
        self, 
        features: ContinuumFeatures
    ) -> List[ContinuumDomain]:
        """Determine recommended continuum computing domains"""
        
        recommended = []
        domain_scores = features.domain_scores()
        
        for domain, score in domain_scores.items():
            if score >= 0.6:
                recommended.append(domain)
        
        return recommended
    
    def _generate_rationale(
        self,
        language: str,
        features: ContinuumFeatures,
        suitability: ContinuumSuitability
    ) -> str:
        """Generate human-readable rationale for suitability assessment"""
        
        score = features.continuum_score
        
        if suitability == ContinuumSuitability.IDEAL:
            return (
                f"{language} is IDEAL for continuum computing with score {score:.2f}. "
                f"It provides native support for spectral operations, FFT/convolution, "
                f"bulk array transformations, and pattern-based resonance matching. "
                f"Excellent for analog AI, signal processing, physical simulation, "
                f"and semantic manifold computation."
            )
        elif suitability == ContinuumSuitability.EXCELLENT:
            return (
                f"{language} is EXCELLENT for continuum computing with score {score:.2f}. "
                f"Strong support for array/tensor operations, spectral computation, "
                f"and field-based transformations make it highly suitable for "
                f"harmonic operator fabrics and attractor stabilization."
            )
        elif suitability == ContinuumSuitability.WELL_SUITED:
            return (
                f"{language} is WELL_SUITED for continuum computing with score {score:.2f}. "
                f"Good support for scientific computing, signal processing libraries, "
                f"and numeric operations enable effective continuum computation "
                f"with appropriate library support."
            )
        elif suitability == ContinuumSuitability.MODERATE:
            return (
                f"{language} has MODERATE continuum computing suitability with score {score:.2f}. "
                f"While it offers some relevant features (e.g., lazy evaluation, "
                f"fixed-point semantics), it may require significant library support "
                f"for full spectral field computation."
            )
        elif suitability == ContinuumSuitability.LIMITED:
            return (
                f"{language} has LIMITED continuum computing suitability with score {score:.2f}. "
                f"Basic numeric operations are available but native support for "
                f"spectral operations, phase transport, and resonance matching is minimal. "
                f"Consider languages better suited for field-based computation."
            )
        else:
            return (
                f"{language} is UNSUITABLE for continuum computing with score {score:.2f}. "
                f"The discrete, step-based execution model and lack of numeric/spectral "
                f"primitives make it fundamentally incompatible with continuum computing "
                f"paradigms. Use for exact accounting, not harmonic field evolution."
            )
    
    def analyze_all(self, languages: List[str]) -> Dict[str, ContinuumProfile]:
        """Analyze multiple languages"""
        results = {}
        for lang in languages:
            try:
                results[lang] = self.analyze_language(lang)
            except Exception as e:
                logger.error(f"Error analyzing {lang}: {e}")
        return results
    
    def get_suitability_distribution(
        self, 
        profiles: List[ContinuumProfile]
    ) -> Dict[str, int]:
        """Get distribution of languages across suitability levels"""
        distribution = {s.value: 0 for s in ContinuumSuitability}
        for profile in profiles:
            distribution[profile.suitability.value] += 1
        return distribution
    
    def get_recommended_languages(
        self, 
        domain: ContinuumDomain,
        min_suitability: ContinuumSuitability = ContinuumSuitability.MODERATE
    ) -> List[str]:
        """Get recommended languages for a specific continuum computing domain"""
        
        suitability_order = [
            ContinuumSuitability.IDEAL,
            ContinuumSuitability.EXCELLENT,
            ContinuumSuitability.WELL_SUITED,
            ContinuumSuitability.MODERATE,
            ContinuumSuitability.LIMITED,
            ContinuumSuitability.UNSUITABLE
        ]
        
        min_index = suitability_order.index(min_suitability)
        acceptable_levels = set(suitability_order[:min_index + 1])
        
        recommended = []
        for lang, features in self.LANGUAGE_PROFILES.items():
            if features.suitability in acceptable_levels:
                domain_scores = features.domain_scores()
                if domain_scores.get(domain, 0) >= 0.5:
                    recommended.append(lang)
        
        # Sort by continuum score (descending)
        recommended.sort(
            key=lambda l: self.LANGUAGE_PROFILES[l].continuum_score,
            reverse=True
        )
        
        return recommended
    
    def get_top_languages(self, n: int = 10) -> List[Tuple[str, float]]:
        """Get top N languages for continuum computing"""
        
        sorted_langs = sorted(
            self.LANGUAGE_PROFILES.items(),
            key=lambda x: x[1].continuum_score,
            reverse=True
        )
        
        return [(lang, features.continuum_score) for lang, features in sorted_langs[:n]]
    
    def export_to_dict(self, profiles: List[ContinuumProfile]) -> Dict[str, Any]:
        """Export analysis results to dictionary"""
        return {
            "total_languages": len(profiles),
            "suitability_distribution": self.get_suitability_distribution(profiles),
            "top_languages": self.get_top_languages(10),
            "profiles": {p.language: p.to_dict() for p in profiles}
        }
    
    def generate_architecture_summary(self) -> Dict[str, Any]:
        """
        Generate a summary of continuum computing architecture with
        recommended languages for each layer.
        """
        
        return {
            "architecture": {
                "spectral_memory_field": {
                    "description": "Stores harmonic attractors",
                    "key_operations": ["FFT", "spectral storage", "harmonic patterns"],
                    "recommended_languages": self.get_recommended_languages(
                        ContinuumDomain.SPECTRAL_MEMORY
                    )
                },
                "phase_transport_layer": {
                    "description": "Preserves coherence across context transformations",
                    "key_operations": ["phase rotation", "gauge transport", "coherence"],
                    "recommended_languages": self.get_recommended_languages(
                        ContinuumDomain.PHASE_TRANSPORT
                    )
                },
                "harmonic_operator_fabric": {
                    "description": "Convolution, interference, resonance, cancellation",
                    "key_operations": ["convolution", "interference gates", "harmonic mixing"],
                    "recommended_languages": self.get_recommended_languages(
                        ContinuumDomain.HARMONIC_OPERATORS
                    )
                },
                "attractor_stabilization": {
                    "description": "Stabilizes outputs as μ-states",
                    "key_operations": ["fixed-point iteration", "attractor convergence"],
                    "recommended_languages": self.get_recommended_languages(
                        ContinuumDomain.ATTRACTOR_STABILIZATION
                    )
                },
                "readout_quantization": {
                    "description": "Collapses continuum bands into discrete symbols",
                    "key_operations": ["argmax resonance", "quantization", "symbolic collapse"],
                    "recommended_languages": self.get_recommended_languages(
                        ContinuumDomain.READOUT_QUANTIZATION
                    )
                }
            },
            "core_operations": {
                "bind": "a ⊗ b - spectral entanglement / Kronecker coupling",
                "route": "e^(iθ)a - phase rotation as address/path change",
                "compute": "a * b - convolution as transformation",
                "decide": "argmax_k |⟨Ψ, Φ_k⟩| - readout by resonance against basis patterns"
            },
            "instruction_set": [
                "shift phase",
                "modulate amplitude", 
                "mix bands",
                "filter harmonics",
                "lock resonance",
                "transport form",
                "collapse readout",
                "renormalize field"
            ],
            "suitable_for": [
                "analog AI",
                "control systems",
                "perception",
                "physical simulation",
                "signal reasoning",
                "semantic manifolds",
                "recursive self-models"
            ],
            "not_suitable_for": [
                "exact accounting",
                "legal contracts",
                "tax forms",
                "anything requiring decimal certainty"
            ]
        }
