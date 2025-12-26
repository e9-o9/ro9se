#!/usr/bin/env python3
"""
OpenCog Atom Type Builder

Populates the atom type system from RosettaCog data.
Creates concrete instances of cognitive domain and language paradigm atoms.
"""

import yaml
from pathlib import Path
from typing import Dict, List
from .atom_types import (
    AtomTypeSystem, 
    CognitiveDomainAtom, 
    LanguageParadigmAtom
)
from .opencog_analyzer import OpenCogAnalyzer


class AtomTypeBuilder:
    """
    Builds the atom type system from RosettaCog data.
    """
    
    def __init__(self, root_dir: str):
        self.root_dir = Path(root_dir)
        self.analyzer = OpenCogAnalyzer(root_dir)
        self.atom_system = AtomTypeSystem()
        
        # Load categories
        categories_file = self.root_dir / 'opencog' / 'data' / 'ai-task-categories.yaml'
        with open(categories_file) as f:
            self.categories_data = yaml.safe_load(f)
    
    def build_cognitive_domain_atoms(self):
        """
        Build all 10 cognitive domain atoms with their specific expressions.
        """
        categories = self.categories_data.get('categories', {})
        
        for domain_name, domain_data in categories.items():
            # Extract data
            description = domain_data.get('description', '')
            tasks = set(domain_data.get('tasks', []))
            subcategories = domain_data.get('subcategories', {})
            
            # Define cognitive processes for each domain
            cognitive_processes = self._get_cognitive_processes(domain_name)
            
            # Create atom
            atom = CognitiveDomainAtom(
                name=domain_name,
                description=description,
                task_universe=tasks,
                subcategories=subcategories,
                cognitive_processes=cognitive_processes,
                metrics=self._compute_domain_metrics(domain_name, tasks)
            )
            
            # Add relationships
            atom.relationships = self._get_domain_relationships(domain_name)
            
            # Register
            self.atom_system.register_cognitive_domain(atom)
    
    def build_language_paradigm_atoms(self):
        """
        Build all 9 language paradigm atoms with their specific expressions.
        """
        paradigms = self.categories_data.get('paradigms', {})
        
        for paradigm_name, paradigm_data in paradigms.items():
            # Extract data
            description = paradigm_data.get('description', '')
            languages = set(paradigm_data.get('languages', []))
            
            # Define features for each paradigm
            features = self._get_paradigm_features(paradigm_name)
            
            # Define computational model
            computational_model = self._get_computational_model(paradigm_name)
            
            # Compute domain applicability
            domain_applicability = self._compute_domain_applicability(
                paradigm_name, languages
            )
            
            # Create atom
            atom = LanguageParadigmAtom(
                name=paradigm_name,
                description=description,
                language_set=languages,
                features=features,
                computational_model=computational_model,
                domain_applicability=domain_applicability
            )
            
            # Add relationships
            atom.relationships = self._get_paradigm_relationships(paradigm_name)
            
            # Register
            self.atom_system.register_language_paradigm(atom)
    
    def _get_cognitive_processes(self, domain: str) -> List[str]:
        """Get cognitive processes for each domain"""
        processes = {
            'symbolic_reasoning': [
                'deduction', 'inference', 'constraint_propagation', 
                'logical_evaluation', 'proof_construction'
            ],
            'pattern_recognition': [
                'matching', 'classification', 'similarity_computation',
                'search', 'feature_extraction'
            ],
            'knowledge_representation': [
                'encoding', 'retrieval', 'traversal', 
                'association', 'serialization'
            ],
            'machine_learning': [
                'optimization', 'regression', 'classification',
                'clustering', 'gradient_descent'
            ],
            'natural_language': [
                'tokenization', 'parsing', 'generation',
                'analysis', 'transformation'
            ],
            'planning_problem_solving': [
                'search', 'heuristic_evaluation', 'goal_decomposition',
                'path_finding', 'strategy_selection'
            ],
            'uncertainty_reasoning': [
                'sampling', 'estimation', 'inference',
                'simulation', 'distribution_fitting'
            ],
            'cognitive_architecture': [
                'synchronization', 'communication', 'scheduling',
                'resource_allocation', 'coordination'
            ],
            'perception_motor': [
                'transformation', 'filtering', 'recognition',
                'rendering', 'temporal_processing'
            ],
            'meta_learning': [
                'reflection', 'code_generation', 'evaluation',
                'introspection', 'self_modification'
            ]
        }
        return processes.get(domain, [])
    
    def _get_paradigm_features(self, paradigm: str) -> List[str]:
        """Get defining features for each paradigm"""
        features = {
            'imperative': [
                'explicit_state', 'sequential_execution', 'mutation',
                'control_flow', 'procedural_abstraction'
            ],
            'object_oriented': [
                'encapsulation', 'inheritance', 'polymorphism',
                'message_passing', 'dynamic_dispatch'
            ],
            'functional': [
                'immutability', 'higher_order_functions', 'recursion',
                'lazy_evaluation', 'referential_transparency'
            ],
            'logic': [
                'declarative_rules', 'unification', 'backtracking',
                'pattern_matching', 'non_determinism'
            ],
            'concurrent': [
                'parallelism', 'message_passing', 'actor_model',
                'lightweight_processes', 'fault_tolerance'
            ],
            'scripting': [
                'dynamic_typing', 'rapid_prototyping', 'interpretive_execution',
                'flexible_syntax', 'runtime_flexibility'
            ],
            'system': [
                'low_level_control', 'manual_memory_management', 'performance',
                'hardware_access', 'minimal_runtime'
            ],
            'scientific': [
                'numerical_optimization', 'array_operations', 'vectorization',
                'mathematical_notation', 'domain_libraries'
            ],
            'multi_paradigm': [
                'paradigm_flexibility', 'multiple_models', 'expressive_power',
                'gradual_typing', 'metaprogramming'
            ]
        }
        return features.get(paradigm, [])
    
    def _get_computational_model(self, paradigm: str) -> str:
        """Get computational model for each paradigm"""
        models = {
            'imperative': 'Von Neumann - sequential state transformation',
            'object_oriented': 'Message passing between objects with encapsulated state',
            'functional': 'Lambda calculus - function composition and evaluation',
            'logic': 'Resolution and unification in Horn clause logic',
            'concurrent': 'Actor model - asynchronous message passing',
            'scripting': 'Interpreted execution with dynamic evaluation',
            'system': 'Direct hardware manipulation with minimal abstraction',
            'scientific': 'Array-oriented computation with vectorized operations',
            'multi_paradigm': 'Hybrid model supporting multiple computational approaches'
        }
        return models.get(paradigm, 'Unspecified')
    
    def _compute_domain_metrics(self, domain: str, tasks: set) -> Dict[str, float]:
        """Compute performance metrics for a domain"""
        return {
            'task_count': float(len(tasks)),
            'complexity': len(tasks) * 1.5,  # Simple heuristic
            'coverage': 1.0  # Would need actual coverage data
        }
    
    def _compute_domain_applicability(
        self, paradigm: str, languages: set
    ) -> Dict[str, float]:
        """
        Compute how well a paradigm applies to each cognitive domain.
        
        Uses heuristics based on paradigm characteristics and domain requirements.
        """
        # Get all domains
        categories = self.categories_data.get('categories', {})
        
        applicability = {}
        
        for domain_name in categories.keys():
            # Compute applicability score based on paradigm-domain fit
            score = self._compute_fit_score(paradigm, domain_name, languages)
            applicability[domain_name] = score
        
        return applicability
    
    def _compute_fit_score(
        self, paradigm: str, domain: str, languages: set
    ) -> float:
        """
        Compute fitness score between paradigm and domain.
        
        Based on empirical observations and theoretical considerations.
        """
        # Paradigm-domain affinity matrix (heuristic-based)
        affinities = {
            'imperative': {
                'symbolic_reasoning': 0.6,
                'pattern_recognition': 0.7,
                'knowledge_representation': 0.7,
                'machine_learning': 0.5,
                'natural_language': 0.6,
                'planning_problem_solving': 0.7,
                'uncertainty_reasoning': 0.6,
                'cognitive_architecture': 0.7,
                'perception_motor': 0.8,
                'meta_learning': 0.5
            },
            'object_oriented': {
                'symbolic_reasoning': 0.7,
                'pattern_recognition': 0.8,
                'knowledge_representation': 0.9,
                'machine_learning': 0.7,
                'natural_language': 0.8,
                'planning_problem_solving': 0.8,
                'uncertainty_reasoning': 0.7,
                'cognitive_architecture': 0.8,
                'perception_motor': 0.7,
                'meta_learning': 0.6
            },
            'functional': {
                'symbolic_reasoning': 0.9,
                'pattern_recognition': 0.8,
                'knowledge_representation': 0.7,
                'machine_learning': 0.8,
                'natural_language': 0.9,
                'planning_problem_solving': 0.7,
                'uncertainty_reasoning': 0.8,
                'cognitive_architecture': 0.6,
                'perception_motor': 0.6,
                'meta_learning': 0.9
            },
            'logic': {
                'symbolic_reasoning': 1.0,
                'pattern_recognition': 0.7,
                'knowledge_representation': 0.8,
                'machine_learning': 0.5,
                'natural_language': 0.7,
                'planning_problem_solving': 0.8,
                'uncertainty_reasoning': 0.6,
                'cognitive_architecture': 0.5,
                'perception_motor': 0.4,
                'meta_learning': 0.7
            },
            'concurrent': {
                'symbolic_reasoning': 0.5,
                'pattern_recognition': 0.6,
                'knowledge_representation': 0.6,
                'machine_learning': 0.7,
                'natural_language': 0.6,
                'planning_problem_solving': 0.7,
                'uncertainty_reasoning': 0.8,
                'cognitive_architecture': 1.0,
                'perception_motor': 0.7,
                'meta_learning': 0.6
            },
            'scripting': {
                'symbolic_reasoning': 0.6,
                'pattern_recognition': 0.8,
                'knowledge_representation': 0.7,
                'machine_learning': 0.7,
                'natural_language': 0.9,
                'planning_problem_solving': 0.7,
                'uncertainty_reasoning': 0.7,
                'cognitive_architecture': 0.6,
                'perception_motor': 0.7,
                'meta_learning': 0.9
            },
            'system': {
                'symbolic_reasoning': 0.6,
                'pattern_recognition': 0.7,
                'knowledge_representation': 0.7,
                'machine_learning': 0.8,
                'natural_language': 0.6,
                'planning_problem_solving': 0.8,
                'uncertainty_reasoning': 0.8,
                'cognitive_architecture': 0.9,
                'perception_motor': 0.9,
                'meta_learning': 0.6
            },
            'scientific': {
                'symbolic_reasoning': 0.7,
                'pattern_recognition': 0.7,
                'knowledge_representation': 0.6,
                'machine_learning': 0.9,
                'natural_language': 0.5,
                'planning_problem_solving': 0.7,
                'uncertainty_reasoning': 0.9,
                'cognitive_architecture': 0.6,
                'perception_motor': 0.8,
                'meta_learning': 0.6
            },
            'multi_paradigm': {
                'symbolic_reasoning': 0.8,
                'pattern_recognition': 0.8,
                'knowledge_representation': 0.8,
                'machine_learning': 0.8,
                'natural_language': 0.8,
                'planning_problem_solving': 0.8,
                'uncertainty_reasoning': 0.8,
                'cognitive_architecture': 0.8,
                'perception_motor': 0.8,
                'meta_learning': 0.8
            }
        }
        
        paradigm_affinities = affinities.get(paradigm, {})
        return paradigm_affinities.get(domain, 0.5)
    
    def _get_domain_relationships(self, domain: str) -> Dict[str, List[str]]:
        """Get relationships between cognitive domains"""
        # Define hierarchical and complementary relationships
        relationships = {
            'symbolic_reasoning': {
                'complements': ['knowledge_representation', 'planning_problem_solving'],
                'requires': ['pattern_recognition'],
                'enables': ['uncertainty_reasoning']
            },
            'pattern_recognition': {
                'complements': ['machine_learning', 'perception_motor'],
                'requires': [],
                'enables': ['symbolic_reasoning', 'natural_language']
            },
            'knowledge_representation': {
                'complements': ['symbolic_reasoning', 'natural_language'],
                'requires': [],
                'enables': ['planning_problem_solving']
            },
            'machine_learning': {
                'complements': ['uncertainty_reasoning', 'pattern_recognition'],
                'requires': ['knowledge_representation'],
                'enables': ['perception_motor']
            },
            'natural_language': {
                'complements': ['knowledge_representation', 'symbolic_reasoning'],
                'requires': ['pattern_recognition'],
                'enables': ['meta_learning']
            },
            'planning_problem_solving': {
                'complements': ['symbolic_reasoning', 'uncertainty_reasoning'],
                'requires': ['knowledge_representation'],
                'enables': ['cognitive_architecture']
            },
            'uncertainty_reasoning': {
                'complements': ['machine_learning', 'planning_problem_solving'],
                'requires': ['symbolic_reasoning'],
                'enables': []
            },
            'cognitive_architecture': {
                'complements': ['planning_problem_solving'],
                'requires': ['knowledge_representation'],
                'enables': ['meta_learning']
            },
            'perception_motor': {
                'complements': ['machine_learning', 'pattern_recognition'],
                'requires': [],
                'enables': []
            },
            'meta_learning': {
                'complements': ['natural_language', 'symbolic_reasoning'],
                'requires': ['cognitive_architecture'],
                'enables': []
            }
        }
        return relationships.get(domain, {'complements': [], 'requires': [], 'enables': []})
    
    def _get_paradigm_relationships(self, paradigm: str) -> Dict[str, List[str]]:
        """Get relationships between language paradigms"""
        relationships = {
            'imperative': {
                'contrasts': ['functional', 'logic'],
                'extends_to': ['object_oriented', 'scripting'],
                'composes_with': ['concurrent']
            },
            'object_oriented': {
                'contrasts': ['functional'],
                'extends_to': ['multi_paradigm'],
                'composes_with': ['imperative', 'scripting']
            },
            'functional': {
                'contrasts': ['imperative', 'object_oriented'],
                'extends_to': ['multi_paradigm'],
                'composes_with': ['logic', 'concurrent']
            },
            'logic': {
                'contrasts': ['imperative'],
                'extends_to': ['multi_paradigm'],
                'composes_with': ['functional']
            },
            'concurrent': {
                'contrasts': [],
                'extends_to': ['multi_paradigm'],
                'composes_with': ['functional', 'imperative']
            },
            'scripting': {
                'contrasts': ['system'],
                'extends_to': ['multi_paradigm'],
                'composes_with': ['object_oriented', 'functional']
            },
            'system': {
                'contrasts': ['scripting'],
                'extends_to': [],
                'composes_with': ['imperative']
            },
            'scientific': {
                'contrasts': [],
                'extends_to': ['multi_paradigm'],
                'composes_with': ['functional', 'imperative']
            },
            'multi_paradigm': {
                'contrasts': [],
                'extends_to': [],
                'composes_with': ['imperative', 'functional', 'object_oriented']
            }
        }
        return relationships.get(paradigm, {
            'contrasts': [], 'extends_to': [], 'composes_with': []
        })
    
    def build(self) -> AtomTypeSystem:
        """Build the complete atom type system"""
        self.build_cognitive_domain_atoms()
        self.build_language_paradigm_atoms()
        return self.atom_system


def main():
    """Test the builder"""
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: atom_type_builder.py <root_dir>")
        sys.exit(1)
    
    root_dir = sys.argv[1]
    builder = AtomTypeBuilder(root_dir)
    atom_system = builder.build()
    
    atom_system.print_summary()


if __name__ == '__main__':
    main()
