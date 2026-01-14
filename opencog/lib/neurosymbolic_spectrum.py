"""
Neuro-Symbolic Spectrum Analyzer

Analyzes programming languages based on their position on the neuro-symbolic spectrum,
which ranges from continuous field computation (neural) to discrete collapse (symbolic).

Based on the framework defined in neuro-symbo.md, this module classifies languages by:
1. How long they preserve possibility before collapsing to commitment
2. Whether they preserve structure explicitly
3. How they handle evaluation (deferred vs. immediate)
4. Their topological markers (parentheses, nesting)

Spectrum positions:
- Neural-aligned: Preserves potential, distributed meaning, deferred evaluation
- Balanced: Equilibrium between field and collapse (e.g., Lisp, functional languages)
- Symbolic-aligned: Immediate collapse, localized meaning, command-based
"""

import logging
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum


logger = logging.getLogger(__name__)


class SpectrumPosition(Enum):
    """Position on the neuro-symbolic spectrum"""
    NEURAL_EXTREME = "neural_extreme"           # Maximum field preservation (e.g., tensor DSLs)
    NEURAL_LEANING = "neural_leaning"           # Strong field orientation (e.g., array languages)
    BALANCED_NEURAL = "balanced_neural"         # Slight neural bias (e.g., logic programming)
    EQUILIBRIUM = "equilibrium"                  # Perfect balance (e.g., Lisp, Scheme)
    BALANCED_SYMBOLIC = "balanced_symbolic"     # Slight symbolic bias (e.g., ML, Haskell)
    SYMBOLIC_LEANING = "symbolic_leaning"       # Strong collapse orientation (e.g., imperative)
    SYMBOLIC_EXTREME = "symbolic_extreme"       # Maximum collapse (e.g., assembly)


class ParadigmSpectrumMapping(Enum):
    """Maps programming paradigms to their typical spectrum positions"""
    FUNCTIONAL_LAMBDA = SpectrumPosition.EQUILIBRIUM           # Lisp, Scheme, lambda calculus
    FUNCTIONAL_PURE = SpectrumPosition.BALANCED_SYMBOLIC       # Haskell, ML
    LOGIC = SpectrumPosition.BALANCED_NEURAL                   # Prolog, Datalog
    ARRAY_DATAFLOW = SpectrumPosition.NEURAL_LEANING           # APL, J, Julia
    TENSOR_NEURAL = SpectrumPosition.NEURAL_EXTREME            # Tensor DSLs
    IMPERATIVE = SpectrumPosition.SYMBOLIC_LEANING             # C, Rust, Java
    OBJECT_ORIENTED = SpectrumPosition.SYMBOLIC_LEANING        # Pure OOP languages
    ASSEMBLY = SpectrumPosition.SYMBOLIC_EXTREME               # Machine code, assembly
    MULTI_PARADIGM = SpectrumPosition.EQUILIBRIUM              # Languages supporting multiple paradigms


@dataclass
class SpectrumFeatures:
    """Features that determine a language's position on the spectrum"""
    
    # Structure preservation (0.0 = linear/flattened, 1.0 = nested/geometric)
    structure_preservation: float = 0.5
    
    # Meaning distribution (0.0 = localized, 1.0 = distributed)
    meaning_distribution: float = 0.5
    
    # Evaluation model (0.0 = immediate, 1.0 = deferred)
    evaluation_deferral: float = 0.5
    
    # Control model (0.0 = command-based, 1.0 = constraint-based)
    control_model: float = 0.5
    
    # Syntactic features (0.0 = statement-based, 1.0 = parenthesized)
    syntactic_topology: float = 0.5
    
    # Semantic model (0.0 = state updating, 1.0 = field shaping)
    semantic_model: float = 0.5
    
    @property
    def neural_score(self) -> float:
        """Overall neural alignment score (0.0 = symbolic, 1.0 = neural)"""
        return (
            self.structure_preservation +
            self.meaning_distribution +
            self.evaluation_deferral +
            self.control_model +
            self.syntactic_topology +
            self.semantic_model
        ) / 6.0
    
    @property
    def position(self) -> SpectrumPosition:
        """Determine spectrum position from features"""
        score = self.neural_score
        
        if score >= 0.9:
            return SpectrumPosition.NEURAL_EXTREME
        elif score >= 0.7:
            return SpectrumPosition.NEURAL_LEANING
        elif score >= 0.55:
            return SpectrumPosition.BALANCED_NEURAL
        elif score >= 0.45:
            return SpectrumPosition.EQUILIBRIUM
        elif score >= 0.3:
            return SpectrumPosition.BALANCED_SYMBOLIC
        elif score >= 0.1:
            return SpectrumPosition.SYMBOLIC_LEANING
        else:
            return SpectrumPosition.SYMBOLIC_EXTREME
    
    def to_dict(self) -> Dict[str, Any]:
        """Export to dictionary"""
        return {
            "structure_preservation": self.structure_preservation,
            "meaning_distribution": self.meaning_distribution,
            "evaluation_deferral": self.evaluation_deferral,
            "control_model": self.control_model,
            "syntactic_topology": self.syntactic_topology,
            "semantic_model": self.semantic_model,
            "neural_score": self.neural_score,
            "position": self.position.value
        }


@dataclass
class LanguageSpectrumProfile:
    """Complete neuro-symbolic spectrum profile for a language"""
    language: str
    primary_paradigm: str
    features: SpectrumFeatures
    position: SpectrumPosition
    rationale: str
    
    # Key characteristics
    preserves_possibility: bool = False
    has_explicit_topology: bool = False
    supports_metaprogramming: bool = False
    homoiconic: bool = False  # Code = data
    
    def to_dict(self) -> Dict[str, Any]:
        """Export to dictionary"""
        return {
            "language": self.language,
            "primary_paradigm": self.primary_paradigm,
            "position": self.position.value,
            "neural_score": self.features.neural_score,
            "features": self.features.to_dict(),
            "characteristics": {
                "preserves_possibility": self.preserves_possibility,
                "has_explicit_topology": self.has_explicit_topology,
                "supports_metaprogramming": self.supports_metaprogramming,
                "homoiconic": self.homoiconic
            },
            "rationale": self.rationale
        }


class NeuroSymbolicAnalyzer:
    """Analyzes languages for their neuro-symbolic spectrum position"""
    
    # Language profiles based on neuro-symbo.md analysis
    LANGUAGE_PROFILES = {
        # Functional / Lambda-based - closest to equilibrium
        "Lisp": SpectrumFeatures(
            structure_preservation=1.0,  # Explicit tree structure
            meaning_distribution=0.9,    # Code as data
            evaluation_deferral=1.0,     # Lazy evaluation possible
            control_model=0.8,           # Expression-based
            syntactic_topology=1.0,      # Parentheses preserve topology
            semantic_model=0.9           # Field reshaping via macros
        ),
        "Scheme": SpectrumFeatures(
            structure_preservation=1.0,
            meaning_distribution=0.9,
            evaluation_deferral=1.0,
            control_model=0.8,
            syntactic_topology=1.0,
            semantic_model=0.9
        ),
        "Racket": SpectrumFeatures(
            structure_preservation=1.0,
            meaning_distribution=0.9,
            evaluation_deferral=1.0,
            control_model=0.8,
            syntactic_topology=1.0,
            semantic_model=0.9
        ),
        "Haskell": SpectrumFeatures(
            structure_preservation=0.8,  # Strong type structure
            meaning_distribution=0.7,    # Lazy evaluation
            evaluation_deferral=0.9,     # Non-strict by default
            control_model=0.7,           # Expression-based
            syntactic_topology=0.6,      # Some structure preserved
            semantic_model=0.7           # Pure functional
        ),
        "ML": SpectrumFeatures(
            structure_preservation=0.7,
            meaning_distribution=0.6,
            evaluation_deferral=0.3,     # Strict evaluation
            control_model=0.6,
            syntactic_topology=0.5,
            semantic_model=0.6
        ),
        
        # Logic programming - symbolic surface, neural interior
        "Prolog": SpectrumFeatures(
            structure_preservation=0.7,  # Tree-based
            meaning_distribution=0.8,    # Unification
            evaluation_deferral=0.8,     # Backtracking preserves possibilities
            control_model=0.9,           # Constraint-based
            syntactic_topology=0.6,      # Horn clauses
            semantic_model=0.8           # Search over possibilities
        ),
        
        # Array / dataflow - neural-leaning
        "APL": SpectrumFeatures(
            structure_preservation=0.8,  # Array structure
            meaning_distribution=0.8,    # Bulk operations
            evaluation_deferral=0.6,
            control_model=0.7,
            syntactic_topology=0.7,      # Special symbols
            semantic_model=0.8           # Transformations
        ),
        "J": SpectrumFeatures(
            structure_preservation=0.8,
            meaning_distribution=0.8,
            evaluation_deferral=0.6,
            control_model=0.7,
            syntactic_topology=0.7,
            semantic_model=0.8
        ),
        "Julia": SpectrumFeatures(
            structure_preservation=0.7,
            meaning_distribution=0.7,
            evaluation_deferral=0.5,
            control_model=0.6,
            syntactic_topology=0.5,
            semantic_model=0.7
        ),
        
        # Imperative - strongly symbolic
        "C": SpectrumFeatures(
            structure_preservation=0.2,  # Linear, sequential
            meaning_distribution=0.2,    # Localized state
            evaluation_deferral=0.1,     # Immediate execution
            control_model=0.1,           # Command-based
            syntactic_topology=0.2,      # Statement-based
            semantic_model=0.1           # State mutation
        ),
        "Rust": SpectrumFeatures(
            structure_preservation=0.3,  # Some abstraction
            meaning_distribution=0.3,
            evaluation_deferral=0.2,
            control_model=0.2,
            syntactic_topology=0.3,
            semantic_model=0.2
        ),
        "Java": SpectrumFeatures(
            structure_preservation=0.3,
            meaning_distribution=0.3,
            evaluation_deferral=0.2,
            control_model=0.2,
            syntactic_topology=0.2,
            semantic_model=0.2
        ),
        "Python": SpectrumFeatures(
            structure_preservation=0.4,  # More dynamic
            meaning_distribution=0.5,    # First-class functions
            evaluation_deferral=0.4,     # Generators, lazy evaluation
            control_model=0.4,
            syntactic_topology=0.4,
            semantic_model=0.4
        ),
        
        # Assembly - maximal collapse
        "Assembly": SpectrumFeatures(
            structure_preservation=0.0,  # No abstraction
            meaning_distribution=0.0,    # Direct hardware
            evaluation_deferral=0.0,     # Immediate execution
            control_model=0.0,           # Direct commands
            syntactic_topology=0.0,      # Linear instructions
            semantic_model=0.0           # Register manipulation
        ),
    }
    
    def __init__(self, repo_path: str = "."):
        """Initialize analyzer"""
        self.repo_path = repo_path
        self.profiles_cache: Dict[str, LanguageSpectrumProfile] = {}
    
    def analyze_language(self, language: str, paradigm: Optional[str] = None) -> LanguageSpectrumProfile:
        """Analyze a specific language's spectrum position"""
        
        # Check cache
        if language in self.profiles_cache:
            return self.profiles_cache[language]
        
        # Get features (use predefined or estimate from paradigm)
        features = self._get_language_features(language, paradigm)
        position = features.position
        
        # Determine characteristics
        characteristics = self._determine_characteristics(language, features, paradigm)
        
        # Generate rationale
        rationale = self._generate_rationale(language, features, position, paradigm)
        
        # Create profile
        profile = LanguageSpectrumProfile(
            language=language,
            primary_paradigm=paradigm or "unknown",
            features=features,
            position=position,
            rationale=rationale,
            **characteristics
        )
        
        # Cache and return
        self.profiles_cache[language] = profile
        return profile
    
    def _get_language_features(self, language: str, paradigm: Optional[str]) -> SpectrumFeatures:
        """Get or estimate language features"""
        
        # Check if we have a predefined profile
        if language in self.LANGUAGE_PROFILES:
            return self.LANGUAGE_PROFILES[language]
        
        # Estimate based on paradigm
        if paradigm:
            return self._estimate_from_paradigm(paradigm)
        
        # Default to balanced
        return SpectrumFeatures()
    
    def _estimate_from_paradigm(self, paradigm: str) -> SpectrumFeatures:
        """Estimate features based on paradigm"""
        
        paradigm_lower = paradigm.lower()
        
        if "functional" in paradigm_lower and "lambda" in paradigm_lower:
            return SpectrumFeatures(0.9, 0.9, 0.9, 0.8, 0.9, 0.9)
        elif "functional" in paradigm_lower:
            return SpectrumFeatures(0.7, 0.7, 0.6, 0.7, 0.6, 0.7)
        elif "logic" in paradigm_lower:
            return SpectrumFeatures(0.7, 0.8, 0.8, 0.9, 0.6, 0.8)
        elif "array" in paradigm_lower or "dataflow" in paradigm_lower:
            return SpectrumFeatures(0.8, 0.8, 0.6, 0.7, 0.7, 0.8)
        elif "imperative" in paradigm_lower:
            return SpectrumFeatures(0.2, 0.2, 0.1, 0.1, 0.2, 0.1)
        elif "object" in paradigm_lower:
            return SpectrumFeatures(0.3, 0.3, 0.2, 0.2, 0.2, 0.2)
        elif "multi" in paradigm_lower:
            return SpectrumFeatures(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
        else:
            return SpectrumFeatures()  # Default balanced
    
    def _determine_characteristics(
        self, 
        language: str, 
        features: SpectrumFeatures,
        paradigm: Optional[str]
    ) -> Dict[str, bool]:
        """Determine key characteristics"""
        
        # Homoiconic languages (code = data)
        homoiconic = language in ["Lisp", "Scheme", "Racket", "Clojure", "Rebol"]
        
        # Explicit topology (parenthesized syntax)
        has_topology = features.syntactic_topology > 0.7
        
        # Preserves possibility (deferred evaluation)
        preserves = features.evaluation_deferral > 0.6
        
        # Metaprogramming support
        supports_meta = homoiconic or features.syntactic_topology > 0.7
        
        return {
            "preserves_possibility": preserves,
            "has_explicit_topology": has_topology,
            "supports_metaprogramming": supports_meta,
            "homoiconic": homoiconic
        }
    
    def _generate_rationale(
        self,
        language: str,
        features: SpectrumFeatures,
        position: SpectrumPosition,
        paradigm: Optional[str]
    ) -> str:
        """Generate human-readable rationale for spectrum position"""
        
        score = features.neural_score
        
        if position == SpectrumPosition.EQUILIBRIUM:
            return (
                f"{language} achieves neuro-symbolic equilibrium. "
                f"It preserves possibility space through deferred evaluation, "
                f"maintains explicit topological structure, and allows field reshaping "
                f"before collapse. This balance makes it excellent for language generation "
                f"and meta-programming."
            )
        elif position in [SpectrumPosition.NEURAL_EXTREME, SpectrumPosition.NEURAL_LEANING]:
            return (
                f"{language} is neural-aligned with score {score:.2f}. "
                f"It emphasizes bulk transformations, distributed meaning, and field operations. "
                f"Computation operates on continuous structures rather than discrete steps."
            )
        elif position in [SpectrumPosition.SYMBOLIC_EXTREME, SpectrumPosition.SYMBOLIC_LEANING]:
            return (
                f"{language} is symbolic-aligned with score {score:.2f}. "
                f"It collapses possibilities early through immediate state updates, "
                f"privileging linear execution and localized meaning. "
                f"Excellent for deterministic, committed computation."
            )
        else:  # Balanced positions
            return (
                f"{language} balances neural and symbolic features with score {score:.2f}. "
                f"It combines elements of both paradigms, allowing flexibility in "
                f"computational expression."
            )
    
    def analyze_all(self, languages: List[str]) -> Dict[str, LanguageSpectrumProfile]:
        """Analyze multiple languages"""
        results = {}
        for lang in languages:
            try:
                results[lang] = self.analyze_language(lang)
            except Exception as e:
                logger.error(f"Error analyzing {lang}: {e}")
        return results
    
    def get_spectrum_distribution(self, profiles: List[LanguageSpectrumProfile]) -> Dict[str, int]:
        """Get distribution of languages across spectrum positions"""
        distribution = {pos.value: 0 for pos in SpectrumPosition}
        for profile in profiles:
            distribution[profile.position.value] += 1
        return distribution
    
    def export_to_dict(self, profiles: List[LanguageSpectrumProfile]) -> Dict[str, Any]:
        """Export analysis results to dictionary"""
        return {
            "total_languages": len(profiles),
            "distribution": self.get_spectrum_distribution(profiles),
            "profiles": {p.language: p.to_dict() for p in profiles}
        }
