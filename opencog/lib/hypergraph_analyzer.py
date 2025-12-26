#!/usr/bin/env python3
"""
OpenCog Hypergraph Analyzer

Extends the base analyzer with subcategory analysis and hypergraph
generation to reveal patterns of peak performance by language and paradigm.
"""

import json
from pathlib import Path
from collections import defaultdict
from typing import Dict, List, Set, Tuple
from .opencog_analyzer import OpenCogAnalyzer


class HypergraphAnalyzer(OpenCogAnalyzer):
    """
    Analyzes language-task-paradigm relationships as a hypergraph.
    
    A hypergraph node can represent:
    - A language
    - A task subcategory
    - A programming paradigm
    
    Hyperedges connect languages to subcategories they implement,
    and languages to their paradigms.
    """
    
    def __init__(self, root_dir: str):
        super().__init__(root_dir)
        self.paradigms = self.categories.get('paradigms', {})
        
    def get_subcategories(self, category: str) -> Dict:
        """Get all subcategories for a given category."""
        categories_data = self.categories.get('categories', {})
        if category not in categories_data:
            return {}
        
        return categories_data[category].get('subcategories', {})
    
    def categorize_task_with_subcategory(self, task: str) -> List[Tuple[str, str]]:
        """
        Determine which category and subcategory a task belongs to.
        
        Returns:
            List of tuples (category, subcategory)
        """
        results = []
        categories_data = self.categories.get('categories', {})
        
        for category_name, category_data in categories_data.items():
            subcategories = category_data.get('subcategories', {})
            
            for subcat_name, subcat_data in subcategories.items():
                tasks = subcat_data.get('tasks', [])
                
                # Check if task matches
                for pattern in tasks:
                    if pattern.lower() == task.lower() or \
                       pattern.lower() in task.lower() or \
                       task.lower() in pattern.lower():
                        results.append((category_name, subcat_name))
                        break
        
        return results
    
    def analyze_language_subcategories(self, language: str) -> Dict:
        """
        Analyze a language's performance across subcategories.
        
        Returns:
            Dict mapping (category, subcategory) to task list
        """
        tasks = self.get_language_tasks(language)
        subcategory_tasks = defaultdict(list)
        
        for task in tasks:
            categorizations = self.categorize_task_with_subcategory(task)
            for category, subcategory in categorizations:
                subcategory_tasks[(category, subcategory)].append(task)
        
        return dict(subcategory_tasks)
    
    def get_language_paradigms(self, language: str) -> List[str]:
        """
        Determine which paradigms a language belongs to.
        
        Returns:
            List of paradigm names
        """
        language_paradigms = []
        
        for paradigm_name, paradigm_data in self.paradigms.items():
            paradigm_langs = paradigm_data.get('languages', [])
            
            # Normalize language names for comparison
            normalized_langs = [lang.lower().replace('-', '').replace('_', '') 
                              for lang in paradigm_langs]
            normalized_target = language.lower().replace('-', '').replace('_', '')
            
            if normalized_target in normalized_langs or language in paradigm_langs:
                language_paradigms.append(paradigm_name)
        
        return language_paradigms
    
    def generate_hypergraph(self) -> Dict:
        """
        Generate a hypergraph structure representing:
        - Languages (nodes)
        - Task subcategories (nodes)
        - Paradigms (nodes)
        - Language-Subcategory edges (which languages implement which subcategories)
        - Language-Paradigm edges (which languages use which paradigms)
        
        Returns:
            Dict with nodes and hyperedges
        """
        languages = self.get_all_languages()
        
        # Initialize hypergraph structure
        hypergraph = {
            'nodes': {
                'languages': [],
                'subcategories': [],
                'paradigms': list(self.paradigms.keys())
            },
            'edges': {
                'language_to_subcategory': [],
                'language_to_paradigm': [],
                'subcategory_performance': {}
            },
            'statistics': {}
        }
        
        # Collect all subcategories
        all_subcategories = set()
        categories_data = self.categories.get('categories', {})
        for category_name, category_data in categories_data.items():
            subcategories = category_data.get('subcategories', {})
            for subcat_name in subcategories.keys():
                all_subcategories.add((category_name, subcat_name))
        
        hypergraph['nodes']['subcategories'] = [
            f"{cat}/{subcat}" for cat, subcat in sorted(all_subcategories)
        ]
        
        # Analyze each language
        language_data = {}
        
        for lang in languages:
            hypergraph['nodes']['languages'].append(lang)
            
            # Get subcategory analysis
            subcat_analysis = self.analyze_language_subcategories(lang)
            
            # Add language-to-subcategory edges
            for (category, subcategory), tasks in subcat_analysis.items():
                edge = {
                    'language': lang,
                    'subcategory': f"{category}/{subcategory}",
                    'task_count': len(tasks),
                    'tasks': tasks
                }
                hypergraph['edges']['language_to_subcategory'].append(edge)
            
            # Get paradigms
            paradigms = self.get_language_paradigms(lang)
            
            # Add language-to-paradigm edges
            for paradigm in paradigms:
                edge = {
                    'language': lang,
                    'paradigm': paradigm
                }
                hypergraph['edges']['language_to_paradigm'].append(edge)
            
            language_data[lang] = {
                'subcategories': len(subcat_analysis),
                'total_tasks': sum(len(tasks) for tasks in subcat_analysis.values()),
                'paradigms': paradigms
            }
        
        # Calculate subcategory performance rankings
        for subcat in hypergraph['nodes']['subcategories']:
            # Find all languages implementing this subcategory
            impl_langs = []
            for edge in hypergraph['edges']['language_to_subcategory']:
                if edge['subcategory'] == subcat:
                    impl_langs.append({
                        'language': edge['language'],
                        'task_count': edge['task_count']
                    })
            
            # Sort by task count
            impl_langs.sort(key=lambda x: x['task_count'], reverse=True)
            
            hypergraph['edges']['subcategory_performance'][subcat] = {
                'top_languages': impl_langs[:10],  # Top 10 languages
                'total_implementations': len(impl_langs)
            }
        
        # Add statistics
        hypergraph['statistics'] = {
            'total_languages': len(languages),
            'total_subcategories': len(all_subcategories),
            'total_paradigms': len(self.paradigms),
            'total_edges': (
                len(hypergraph['edges']['language_to_subcategory']) +
                len(hypergraph['edges']['language_to_paradigm'])
            )
        }
        
        return hypergraph
    
    def generate_paradigm_performance_matrix(self) -> Dict:
        """
        Generate a matrix showing which paradigms perform best for each subcategory.
        
        Returns:
            Dict mapping subcategories to paradigm performance
        """
        hypergraph = self.generate_hypergraph()
        matrix = {}
        
        for subcat in hypergraph['nodes']['subcategories']:
            paradigm_scores = defaultdict(lambda: {'count': 0, 'languages': []})
            
            # Find all languages implementing this subcategory
            for edge in hypergraph['edges']['language_to_subcategory']:
                if edge['subcategory'] == subcat:
                    lang = edge['language']
                    task_count = edge['task_count']
                    
                    # Find paradigms for this language
                    for para_edge in hypergraph['edges']['language_to_paradigm']:
                        if para_edge['language'] == lang:
                            paradigm = para_edge['paradigm']
                            paradigm_scores[paradigm]['count'] += task_count
                            paradigm_scores[paradigm]['languages'].append({
                                'language': lang,
                                'task_count': task_count
                            })
            
            # Sort paradigms by score
            sorted_paradigms = sorted(
                paradigm_scores.items(),
                key=lambda x: x[1]['count'],
                reverse=True
            )
            
            matrix[subcat] = {
                'paradigm_rankings': [
                    {
                        'paradigm': p,
                        'total_tasks': data['count'],
                        'num_languages': len(data['languages']),
                        'top_language': max(data['languages'], 
                                          key=lambda x: x['task_count'])
                                       if data['languages'] else None
                    }
                    for p, data in sorted_paradigms[:5]  # Top 5 paradigms
                ]
            }
        
        return matrix
    
    def export_hypergraph(self, output_file: str):
        """Export hypergraph to JSON file."""
        hypergraph = self.generate_hypergraph()
        
        with open(output_file, 'w') as f:
            json.dump(hypergraph, f, indent=2)
        
        print(f"Hypergraph exported to {output_file}")
    
    def export_paradigm_matrix(self, output_file: str):
        """Export paradigm performance matrix to JSON file."""
        matrix = self.generate_paradigm_performance_matrix()
        
        with open(output_file, 'w') as f:
            json.dump(matrix, f, indent=2)
        
        print(f"Paradigm performance matrix exported to {output_file}")
    
    def print_subcategory_report(self):
        """Print a detailed report of subcategory analysis."""
        print("=" * 80)
        print("OpenCog Hypergraph Analysis: Task Specialization by Subcategory")
        print("=" * 80)
        print()
        
        categories_data = self.categories.get('categories', {})
        
        for category_name, category_data in sorted(categories_data.items()):
            subcategories = category_data.get('subcategories', {})
            
            if not subcategories:
                continue
            
            print(f"\n{category_name.upper().replace('_', ' ')}")
            print(f"{category_data.get('description', '')}")
            print("-" * 80)
            
            for subcat_name, subcat_data in sorted(subcategories.items()):
                print(f"\n  Subcategory: {subcat_name.replace('_', ' ').title()}")
                print(f"  Description: {subcat_data.get('description', '')}")
                
                # Analyze which languages perform best in this subcategory
                subcat_key = f"{category_name}/{subcat_name}"
                lang_performance = []
                
                for lang in self.get_all_languages():
                    subcat_analysis = self.analyze_language_subcategories(lang)
                    if (category_name, subcat_name) in subcat_analysis:
                        task_count = len(subcat_analysis[(category_name, subcat_name)])
                        lang_performance.append((lang, task_count))
                
                # Sort by task count
                lang_performance.sort(key=lambda x: x[1], reverse=True)
                
                print(f"  Top Languages:")
                for idx, (lang, count) in enumerate(lang_performance[:5], 1):
                    paradigms = self.get_language_paradigms(lang)
                    paradigm_str = ', '.join(paradigms) if paradigms else 'unknown'
                    print(f"    {idx}. {lang}: {count} tasks ({paradigm_str})")
                
                if not lang_performance:
                    print("    No implementations found")
        
        print("\n" + "=" * 80)
    
    def print_paradigm_matrix(self):
        """Print the paradigm performance matrix."""
        print("=" * 80)
        print("Paradigm Performance Matrix: Best Paradigms per Subcategory")
        print("=" * 80)
        print()
        
        matrix = self.generate_paradigm_performance_matrix()
        
        for subcat, data in sorted(matrix.items()):
            print(f"\n{subcat}")
            print("-" * 80)
            
            rankings = data['paradigm_rankings']
            if rankings:
                for idx, ranking in enumerate(rankings, 1):
                    top_lang = ranking['top_language']
                    top_lang_str = f"{top_lang['language']} ({top_lang['task_count']} tasks)" \
                                  if top_lang else "N/A"
                    
                    print(f"  {idx}. {ranking['paradigm'].replace('_', ' ').title()}")
                    print(f"     Total: {ranking['total_tasks']} tasks "
                          f"across {ranking['num_languages']} languages")
                    print(f"     Best: {top_lang_str}")
            else:
                print("  No paradigm data available")
        
        print("\n" + "=" * 80)


def main():
    """Main entry point for hypergraph analysis."""
    script_dir = Path(__file__).parent.parent.parent
    
    analyzer = HypergraphAnalyzer(str(script_dir))
    
    # Print subcategory report
    analyzer.print_subcategory_report()
    
    # Print paradigm matrix
    print("\n")
    analyzer.print_paradigm_matrix()
    
    # Export data
    output_dir = script_dir / "opencog" / "data"
    output_dir.mkdir(exist_ok=True)
    
    analyzer.export_hypergraph(str(output_dir / "hypergraph.json"))
    analyzer.export_paradigm_matrix(str(output_dir / "paradigm-matrix.json"))
    
    print("\n" + "=" * 80)
    print("Analysis complete. Data exported to opencog/data/")
    print("=" * 80)


if __name__ == "__main__":
    main()
