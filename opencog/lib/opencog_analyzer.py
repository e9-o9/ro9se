#!/usr/bin/env python3
"""
OpenCog Language Capability Analyzer

Analyzes all programming languages in RosettaCog to evaluate their
AI/AGI capabilities across different cognitive domains.
"""

import os
import sys
import yaml
import json
from pathlib import Path
from collections import defaultdict
from typing import Dict, List, Set


class OpenCogAnalyzer:
    """Analyzes language capabilities for AI/cognitive tasks."""
    
    def __init__(self, root_dir: str):
        self.root_dir = Path(root_dir)
        self.lang_dir = self.root_dir / "Lang"
        self.task_dir = self.root_dir / "Task"
        self.opencog_dir = self.root_dir / "opencog"
        self.data_dir = self.opencog_dir / "data"
        
        # Load AI task categories
        self.categories = self._load_categories()
        
    def _load_categories(self) -> Dict:
        """Load AI task categorization from YAML."""
        category_file = self.data_dir / "ai-task-categories.yaml"
        if category_file.exists():
            with open(category_file, 'r') as f:
                return yaml.safe_load(f)
        return {}
    
    def get_all_languages(self) -> List[str]:
        """Get list of all programming languages."""
        languages = []
        for lang_path in sorted(self.lang_dir.iterdir()):
            if lang_path.is_dir():
                languages.append(lang_path.name)
        return languages
    
    def get_all_tasks(self) -> List[str]:
        """Get list of all tasks."""
        tasks = []
        for task_path in sorted(self.task_dir.iterdir()):
            if task_path.is_dir():
                tasks.append(task_path.name)
        return tasks
    
    def get_language_tasks(self, language: str) -> Set[str]:
        """Get all tasks implemented in a specific language."""
        lang_path = self.lang_dir / language
        tasks = set()
        
        if not lang_path.exists():
            return tasks
            
        for item in lang_path.iterdir():
            if item.is_dir() or (item.is_symlink() and item.resolve().is_dir()):
                tasks.add(item.name)
            elif item.is_file() and not item.name.startswith('00-'):
                # Some tasks are files, not directories
                pass
                
        return tasks
    
    def count_task_implementations(self, task: str) -> int:
        """Count how many languages implement a task."""
        task_path = self.task_dir / task
        if not task_path.exists():
            return 0
            
        count = 0
        for item in task_path.iterdir():
            if item.is_dir() and not item.name.startswith('00-'):
                count += 1
        return count
    
    def categorize_task(self, task: str) -> List[str]:
        """Determine which AI categories a task belongs to."""
        categories_list = []
        
        if 'categories' not in self.categories:
            return categories_list
            
        for category_name, category_data in self.categories['categories'].items():
            if 'tasks' not in category_data:
                continue
                
            # Check if task matches any pattern in category
            for pattern in category_data['tasks']:
                if pattern.lower() in task.lower() or task.lower() in pattern.lower():
                    categories_list.append(category_name)
                    break
                    
        return categories_list
    
    def analyze_language(self, language: str) -> Dict:
        """Analyze a single language's AI capabilities."""
        tasks = self.get_language_tasks(language)
        
        # Categorize tasks by AI domain
        category_tasks = defaultdict(list)
        for task in tasks:
            categories = self.categorize_task(task)
            for category in categories:
                category_tasks[category].append(task)
        
        # Calculate metrics
        total_tasks = len(tasks)
        ai_tasks = sum(len(task_list) for task_list in category_tasks.values())
        
        return {
            'language': language,
            'total_tasks': total_tasks,
            'ai_categorized_tasks': ai_tasks,
            'category_breakdown': dict(category_tasks),
            'coverage_by_category': {
                cat: len(task_list) 
                for cat, task_list in category_tasks.items()
            }
        }
    
    def generate_language_profile(self, language: str) -> Dict:
        """Generate a comprehensive capability profile for a language."""
        analysis = self.analyze_language(language)
        
        # Calculate AI capability scores
        categories = self.categories.get('categories', {})
        total_categories = len(categories)
        covered_categories = len(analysis['coverage_by_category'])
        
        coverage_score = (covered_categories / total_categories * 100) if total_categories > 0 else 0
        
        return {
            'language': language,
            'total_tasks_implemented': analysis['total_tasks'],
            'ai_tasks_implemented': analysis['ai_categorized_tasks'],
            'category_coverage': coverage_score,
            'categories_covered': covered_categories,
            'total_categories': total_categories,
            'detailed_coverage': analysis['coverage_by_category']
        }
    
    def analyze_all_languages(self) -> Dict:
        """Analyze all languages and rank by AI capabilities."""
        languages = self.get_all_languages()
        profiles = []
        
        for lang in languages:
            profile = self.generate_language_profile(lang)
            profiles.append(profile)
        
        # Sort by total AI tasks implemented
        profiles.sort(key=lambda x: x['ai_tasks_implemented'], reverse=True)
        
        return {
            'total_languages': len(profiles),
            'language_profiles': profiles
        }
    
    def generate_frankencog_manifest(self) -> Dict:
        """
        Generate the FrankenCog Integration Manifest.
        
        For each AI category, identify the best language(s) based on:
        - Number of task implementations
        - Coverage breadth
        - Category specialization
        """
        manifest = {
            'description': 'FrankenCog Patchwork Inference Fabric - Optimal language selection per AI function',
            'categories': {}
        }
        
        categories_data = self.categories.get('categories', {})
        languages = self.get_all_languages()
        
        for category_name, category_info in categories_data.items():
            # Find languages with best coverage for this category
            lang_scores = []
            
            for lang in languages:
                analysis = self.analyze_language(lang)
                coverage = analysis['coverage_by_category'].get(category_name, 0)
                
                if coverage > 0:
                    lang_scores.append({
                        'language': lang,
                        'task_count': coverage,
                        'tasks': analysis['category_breakdown'].get(category_name, [])
                    })
            
            # Sort by task count
            lang_scores.sort(key=lambda x: x['task_count'], reverse=True)
            
            manifest['categories'][category_name] = {
                'description': category_info.get('description', ''),
                'recommended_languages': lang_scores[:5],  # Top 5 languages
                'best_language': lang_scores[0]['language'] if lang_scores else None,
                'total_implementations': sum(ls['task_count'] for ls in lang_scores)
            }
        
        return manifest


def main():
    """Main entry point."""
    # Get repository root
    script_dir = Path(__file__).parent.parent.parent
    
    analyzer = OpenCogAnalyzer(str(script_dir))
    
    print("=" * 80)
    print("OpenCog: Post-Polyglot Transcendent AI Evaluation Framework")
    print("=" * 80)
    print()
    
    # Analyze all languages
    print("Analyzing language capabilities across AI domains...")
    print()
    
    all_analysis = analyzer.analyze_all_languages()
    
    print(f"Total Languages Analyzed: {all_analysis['total_languages']}")
    print()
    print("Top 20 Languages by AI Task Implementation:")
    print("-" * 80)
    print(f"{'Rank':<6} {'Language':<30} {'AI Tasks':<12} {'Coverage':<12}")
    print("-" * 80)
    
    for idx, profile in enumerate(all_analysis['language_profiles'][:20], 1):
        print(f"{idx:<6} {profile['language']:<30} "
              f"{profile['ai_tasks_implemented']:<12} "
              f"{profile['category_coverage']:.1f}%")
    
    print()
    print("=" * 80)
    print()
    
    # Generate FrankenCog manifest
    print("Generating FrankenCog Integration Manifest...")
    manifest = analyzer.generate_frankencog_manifest()
    
    print()
    print("FrankenCog Patchwork - Optimal Language per AI Category:")
    print("-" * 80)
    print(f"{'Category':<30} {'Best Language':<20} {'Implementations':<15}")
    print("-" * 80)
    
    for category, info in manifest['categories'].items():
        best_lang = info['best_language'] or 'None'
        total = info['total_implementations']
        print(f"{category:<30} {best_lang:<20} {total:<15}")
    
    print()
    print("=" * 80)
    
    # Save full analysis to JSON
    output_dir = script_dir / "opencog" / "output"
    output_dir.mkdir(exist_ok=True)
    
    with open(output_dir / "language-analysis.json", 'w') as f:
        json.dump(all_analysis, f, indent=2)
    
    with open(output_dir / "frankencog-manifest.json", 'w') as f:
        json.dump(manifest, f, indent=2)
    
    print()
    print(f"Full analysis saved to: {output_dir}")
    print(f"  - language-analysis.json")
    print(f"  - frankencog-manifest.json")
    print()
    print("OpenCog evaluation complete.")
    print()


if __name__ == '__main__':
    main()
