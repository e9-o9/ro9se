#!/usr/bin/env python3
"""
OpenCog Atom Type System

Defines formalized atom type expressions for cognitive-domain and language-paradigm atoms.
Provides a type hierarchy and algebraic expressions for reasoning about language capabilities.

Inspired by OpenCog's Atomese but tailored for RosettaCog's language evaluation framework.
"""

from typing import Dict, List, Set, Tuple, Any
from dataclasses import dataclass, field
from enum import Enum
import json


class AtomTypeCategory(Enum):
    """Major atom type categories"""
    COGNITIVE_DOMAIN = "cognitive_domain"
    LANGUAGE_PARADIGM = "language_paradigm"


@dataclass
class AtomType:
    """
    Base atom type representing a fundamental concept in the RosettaCog system.
    
    An atom type is a formalized expression representing either:
    - A cognitive domain (symbolic reasoning, pattern recognition, etc.)
    - A language paradigm (imperative, functional, object-oriented, etc.)
    """
    name: str
    description: str
    category: AtomTypeCategory = None
    properties: Dict[str, Any] = field(default_factory=dict)
    relationships: Dict[str, List[str]] = field(default_factory=dict)
    
    def get_expression(self) -> str:
        """Generate formal expression for this atom type"""
        raise NotImplementedError("Subclasses must implement get_expression()")
    
    def to_dict(self) -> Dict:
        """Convert to dictionary representation"""
        return {
            'name': self.name,
            'category': self.category.value,
            'description': self.description,
            'properties': self.properties,
            'relationships': self.relationships,
            'expression': self.get_expression()
        }


@dataclass
class CognitiveDomainAtom(AtomType):
    """
    Cognitive domain atom type.
    
    Formalized Expression:
    CD(d) = ⟨Ω_d, Σ_d, Ψ_d, Φ_d⟩
    
    Where:
    - Ω_d (Omega): Task universe for domain d
    - Σ_d (Sigma): Subcategory partitioning of Ω_d  
    - Ψ_d (Psi): Cognitive processes/operations
    - Φ_d (Phi): Performance metrics and evaluation functions
    """
    
    task_universe: Set[str] = field(default_factory=set)  # Ω_d
    subcategories: Dict[str, Dict] = field(default_factory=dict)  # Σ_d
    cognitive_processes: List[str] = field(default_factory=list)  # Ψ_d
    metrics: Dict[str, float] = field(default_factory=dict)  # Φ_d
    
    def __post_init__(self):
        self.category = AtomTypeCategory.COGNITIVE_DOMAIN
    
    def get_expression(self) -> str:
        """Generate formal expression for cognitive domain atom"""
        omega = f"Ω_{self.name}"  # Task universe
        sigma = f"Σ_{self.name}"  # Subcategory partitioning
        psi = f"Ψ_{self.name}"    # Cognitive processes
        phi = f"Φ_{self.name}"    # Performance metrics
        
        return f"CD({self.name}) = ⟨{omega}, {sigma}, {psi}, {phi}⟩"
    
    def get_detailed_expression(self) -> Dict[str, str]:
        """Get detailed breakdown of the expression"""
        return {
            'canonical': self.get_expression(),
            'omega': f"Ω_{self.name} = {{ {', '.join(sorted(list(self.task_universe)[:5]))}{'...' if len(self.task_universe) > 5 else ''} }} (n={len(self.task_universe)})",
            'sigma': f"Σ_{self.name} = {{ {', '.join(self.subcategories.keys())} }} (k={len(self.subcategories)})",
            'psi': f"Ψ_{self.name} = {{ {', '.join(self.cognitive_processes)} }}",
            'phi': f"Φ_{self.name} = {{ {', '.join(f'{k}={v:.2f}' for k, v in list(self.metrics.items())[:3])} }}"
        }
    
    def compute_complexity(self) -> float:
        """
        Compute domain complexity measure:
        C(d) = |Ω_d| × |Σ_d| × log(|Ψ_d|)
        """
        import math
        omega_size = len(self.task_universe)
        sigma_size = len(self.subcategories)
        psi_size = len(self.cognitive_processes) or 1
        
        return omega_size * sigma_size * math.log(psi_size + 1)


@dataclass  
class LanguageParadigmAtom(AtomType):
    """
    Language paradigm atom type.
    
    Formalized Expression:
    LP(p) = ⟨Λ_p, Π_p, Θ_p, Ξ_p⟩
    
    Where:
    - Λ_p (Lambda): Language set belonging to paradigm p
    - Π_p (Pi): Paradigmatic features/characteristics
    - Θ_p (Theta): Computational model and execution semantics
    - Ξ_p (Xi): Cross-domain applicability matrix
    """
    
    language_set: Set[str] = field(default_factory=set)  # Λ_p
    features: List[str] = field(default_factory=list)  # Π_p
    computational_model: str = ""  # Θ_p
    domain_applicability: Dict[str, float] = field(default_factory=dict)  # Ξ_p
    
    def __post_init__(self):
        self.category = AtomTypeCategory.LANGUAGE_PARADIGM
    
    def get_expression(self) -> str:
        """Generate formal expression for language paradigm atom"""
        lambda_p = f"Λ_{self.name}"  # Language set
        pi_p = f"Π_{self.name}"      # Features
        theta_p = f"Θ_{self.name}"   # Computational model
        xi_p = f"Ξ_{self.name}"      # Domain applicability
        
        return f"LP({self.name}) = ⟨{lambda_p}, {pi_p}, {theta_p}, {xi_p}⟩"
    
    def get_detailed_expression(self) -> Dict[str, str]:
        """Get detailed breakdown of the expression"""
        return {
            'canonical': self.get_expression(),
            'lambda': f"Λ_{self.name} = {{ {', '.join(sorted(list(self.language_set)[:5]))}{'...' if len(self.language_set) > 5 else ''} }} (n={len(self.language_set)})",
            'pi': f"Π_{self.name} = {{ {', '.join(self.features)} }}",
            'theta': f"Θ_{self.name} = {self.computational_model}",
            'xi': f"Ξ_{self.name} = {{ {', '.join(f'{k}→{v:.2f}' for k, v in list(self.domain_applicability.items())[:3])} }}"
        }
    
    def compute_versatility(self) -> float:
        """
        Compute paradigm versatility measure:
        V(p) = |Λ_p| × Σ(Ξ_p) / |Features|
        """
        lambda_size = len(self.language_set)
        xi_sum = sum(self.domain_applicability.values())
        feature_count = len(self.features) or 1
        
        return (lambda_size * xi_sum) / feature_count


class AtomTypeSystem:
    """
    Complete atom type system for RosettaCog.
    
    Manages the hierarchy and relationships between cognitive domain and language paradigm atoms.
    """
    
    def __init__(self):
        self.cognitive_domains: Dict[str, CognitiveDomainAtom] = {}
        self.language_paradigms: Dict[str, LanguageParadigmAtom] = {}
        
    def register_cognitive_domain(self, atom: CognitiveDomainAtom):
        """Register a cognitive domain atom"""
        self.cognitive_domains[atom.name] = atom
    
    def register_language_paradigm(self, atom: LanguageParadigmAtom):
        """Register a language paradigm atom"""
        self.language_paradigms[atom.name] = atom
    
    def get_generalized_expressions(self) -> Dict[str, str]:
        """
        Get the two major generalized expressions:
        1. Cognitive Domain Atoms (CD)
        2. Language Paradigm Atoms (LP)
        """
        return {
            'cognitive_domain': {
                'name': 'Cognitive Domain Atom (CD)',
                'expression': 'CD(d) = ⟨Ω_d, Σ_d, Ψ_d, Φ_d⟩',
                'components': {
                    'Ω_d': 'Task universe for domain d - set of all tasks in the domain',
                    'Σ_d': 'Subcategory partitioning - refined specialization of Ω_d',
                    'Ψ_d': 'Cognitive processes - mental operations required',
                    'Φ_d': 'Performance metrics - evaluation functions'
                },
                'semantics': 'A cognitive domain atom represents a fundamental area of cognitive capability, characterized by its task universe, internal structure, required processes, and performance measures.'
            },
            'language_paradigm': {
                'name': 'Language Paradigm Atom (LP)',
                'expression': 'LP(p) = ⟨Λ_p, Π_p, Θ_p, Ξ_p⟩',
                'components': {
                    'Λ_p': 'Language set - programming languages in paradigm p',
                    'Π_p': 'Paradigmatic features - defining characteristics',
                    'Θ_p': 'Computational model - execution semantics',
                    'Ξ_p': 'Domain applicability - fitness for cognitive domains'
                },
                'semantics': 'A language paradigm atom represents a fundamental approach to programming, characterized by its language membership, defining features, computational model, and effectiveness across cognitive domains.'
            }
        }
    
    def get_all_expressions(self) -> Dict[str, Any]:
        """Get all atom type expressions"""
        return {
            'generalized': self.get_generalized_expressions(),
            'cognitive_domains': {
                name: atom.to_dict() 
                for name, atom in self.cognitive_domains.items()
            },
            'language_paradigms': {
                name: atom.to_dict()
                for name, atom in self.language_paradigms.items()
            }
        }
    
    def compute_paradigm_domain_affinity(self, paradigm: str, domain: str) -> float:
        """
        Compute affinity between a paradigm and a domain:
        A(p, d) = Ξ_p(d) × |Λ_p ∩ L_d| / |Λ_p|
        
        Where L_d is the set of languages implementing domain d
        """
        if paradigm not in self.language_paradigms:
            return 0.0
        if domain not in self.cognitive_domains:
            return 0.0
        
        paradigm_atom = self.language_paradigms[paradigm]
        domain_atom = self.cognitive_domains[domain]
        
        # Get applicability score
        xi_score = paradigm_atom.domain_applicability.get(domain, 0.0)
        
        # This would need actual implementation data
        # For now return the applicability score
        return xi_score
    
    def generate_comparison_insights(self) -> Dict[str, Any]:
        """
        Generate insights from comparing cognitive domain and language paradigm atoms.
        """
        insights = {
            'structural_comparison': self._compare_structures(),
            'complexity_analysis': self._analyze_complexity(),
            'coverage_analysis': self._analyze_coverage(),
            'affinity_patterns': self._analyze_affinity_patterns()
        }
        return insights
    
    def _compare_structures(self) -> Dict[str, Any]:
        """Compare structural properties of the two atom categories"""
        cd_count = len(self.cognitive_domains)
        lp_count = len(self.language_paradigms)
        
        # Compute average subcategories per domain
        avg_subcats = sum(
            len(atom.subcategories) 
            for atom in self.cognitive_domains.values()
        ) / cd_count if cd_count > 0 else 0
        
        # Compute average languages per paradigm
        avg_langs = sum(
            len(atom.language_set)
            for atom in self.language_paradigms.values()
        ) / lp_count if lp_count > 0 else 0
        
        return {
            'cognitive_domain_count': cd_count,
            'language_paradigm_count': lp_count,
            'ratio': cd_count / lp_count if lp_count > 0 else 0,
            'avg_subcategories_per_domain': avg_subcats,
            'avg_languages_per_paradigm': avg_langs,
            'structural_similarity': 'Both use 4-tuple representation but with different semantics'
        }
    
    def _analyze_complexity(self) -> Dict[str, Any]:
        """Analyze complexity measures"""
        domain_complexities = {
            name: atom.compute_complexity()
            for name, atom in self.cognitive_domains.items()
        }
        
        paradigm_versatilities = {
            name: atom.compute_versatility()
            for name, atom in self.language_paradigms.items()
        }
        
        return {
            'domain_complexities': domain_complexities,
            'paradigm_versatilities': paradigm_versatilities,
            'most_complex_domain': max(domain_complexities.items(), key=lambda x: x[1])[0] if domain_complexities else None,
            'most_versatile_paradigm': max(paradigm_versatilities.items(), key=lambda x: x[1])[0] if paradigm_versatilities else None
        }
    
    def _analyze_coverage(self) -> Dict[str, Any]:
        """Analyze coverage patterns"""
        total_tasks = sum(
            len(atom.task_universe)
            for atom in self.cognitive_domains.values()
        )
        
        total_languages = len(set().union(*[
            atom.language_set
            for atom in self.language_paradigms.values()
        ]))
        
        return {
            'total_cognitive_tasks': total_tasks,
            'total_paradigm_languages': total_languages,
            'tasks_per_domain': {
                name: len(atom.task_universe)
                for name, atom in self.cognitive_domains.items()
            },
            'languages_per_paradigm': {
                name: len(atom.language_set)
                for name, atom in self.language_paradigms.items()
            }
        }
    
    def _analyze_affinity_patterns(self) -> Dict[str, Any]:
        """Analyze paradigm-domain affinity patterns"""
        affinities = {}
        
        for paradigm_name, paradigm_atom in self.language_paradigms.items():
            affinities[paradigm_name] = {
                'strongest_domains': sorted(
                    paradigm_atom.domain_applicability.items(),
                    key=lambda x: x[1],
                    reverse=True
                )[:3],
                'weakest_domains': sorted(
                    paradigm_atom.domain_applicability.items(),
                    key=lambda x: x[1]
                )[:3]
            }
        
        return affinities
    
    def export_to_json(self, filepath: str):
        """Export the complete atom type system to JSON"""
        data = self.get_all_expressions()
        with open(filepath, 'w') as f:
            json.dump(data, f, indent=2)
    
    def print_summary(self):
        """Print a summary of the atom type system"""
        print("=" * 80)
        print("ROSETTACOG ATOM TYPE SYSTEM")
        print("=" * 80)
        print()
        
        # Generalized expressions
        gen_expr = self.get_generalized_expressions()
        
        print("GENERALIZED EXPRESSIONS")
        print("-" * 80)
        print()
        
        print("1. COGNITIVE DOMAIN ATOMS")
        cd_expr = gen_expr['cognitive_domain']
        print(f"   Expression: {cd_expr['expression']}")
        print(f"   Semantics: {cd_expr['semantics']}")
        print()
        
        print("2. LANGUAGE PARADIGM ATOMS")
        lp_expr = gen_expr['language_paradigm']
        print(f"   Expression: {lp_expr['expression']}")
        print(f"   Semantics: {lp_expr['semantics']}")
        print()
        
        # Specific expressions
        print("=" * 80)
        print("COGNITIVE DOMAIN ATOM INSTANCES")
        print("=" * 80)
        for name, atom in sorted(self.cognitive_domains.items()):
            print(f"\n{name}:")
            print(f"  {atom.get_expression()}")
            details = atom.get_detailed_expression()
            for key, val in details.items():
                if key != 'canonical':
                    print(f"    {val}")
        
        print()
        print("=" * 80)
        print("LANGUAGE PARADIGM ATOM INSTANCES")
        print("=" * 80)
        for name, atom in sorted(self.language_paradigms.items()):
            print(f"\n{name}:")
            print(f"  {atom.get_expression()}")
            details = atom.get_detailed_expression()
            for key, val in details.items():
                if key != 'canonical':
                    print(f"    {val}")
        
        print()
        print("=" * 80)
        print("COMPARISON INSIGHTS")
        print("=" * 80)
        insights = self.generate_comparison_insights()
        
        print("\nStructural Comparison:")
        for key, val in insights['structural_comparison'].items():
            print(f"  {key}: {val}")
        
        print("\nComplexity Analysis:")
        complexity = insights['complexity_analysis']
        print(f"  Most complex domain: {complexity['most_complex_domain']}")
        print(f"  Most versatile paradigm: {complexity['most_versatile_paradigm']}")
        
        print("\nCoverage Analysis:")
        coverage = insights['coverage_analysis']
        print(f"  Total cognitive tasks: {coverage['total_cognitive_tasks']}")
        print(f"  Total paradigm languages: {coverage['total_paradigm_languages']}")
        
        print()
