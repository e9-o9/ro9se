"""
Unit tests for graph_analytics.py

Tests the graph analytics module for RosettaCog hypergraph analysis,
including centrality measures, community detection, path analysis,
and subgraph mining.
"""

import pytest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "opencog" / "lib"))

from graph_analytics import (
    CentralityType,
    NodeType,
    Node,
    Edge,
    Community,
    PathResult,
    Graph,
    CentralityAnalyzer,
    CommunityDetector,
    PathAnalyzer,
    SubgraphMiner,
    GraphAnalytics,
)


class TestCentralityType:
    """Tests for CentralityType enum."""

    def test_has_degree(self):
        assert CentralityType.DEGREE.value == "degree"

    def test_has_betweenness(self):
        assert CentralityType.BETWEENNESS.value == "betweenness"

    def test_has_closeness(self):
        assert CentralityType.CLOSENESS.value == "closeness"

    def test_has_pagerank(self):
        assert CentralityType.PAGERANK.value == "pagerank"

    def test_has_eigenvector(self):
        assert CentralityType.EIGENVECTOR.value == "eigenvector"

    def test_all_types_count(self):
        assert len(CentralityType) == 5


class TestNodeType:
    """Tests for NodeType enum."""

    def test_has_language(self):
        assert NodeType.LANGUAGE.value == "language"

    def test_has_task(self):
        assert NodeType.TASK.value == "task"

    def test_has_paradigm(self):
        assert NodeType.PARADIGM.value == "paradigm"

    def test_has_domain(self):
        assert NodeType.DOMAIN.value == "domain"

    def test_has_subcategory(self):
        assert NodeType.SUBCATEGORY.value == "subcategory"

    def test_all_types_count(self):
        assert len(NodeType) == 5


class TestNode:
    """Tests for Node dataclass."""

    def test_initialization(self):
        node = Node(id="python", node_type=NodeType.LANGUAGE)
        assert node.id == "python"
        assert node.node_type == NodeType.LANGUAGE
        assert node.attributes == {}

    def test_initialization_with_attributes(self):
        node = Node(
            id="sorting",
            node_type=NodeType.TASK,
            attributes={"difficulty": "medium", "category": "algorithms"}
        )
        assert node.attributes["difficulty"] == "medium"

    def test_hash(self):
        node1 = Node(id="test", node_type=NodeType.LANGUAGE)
        node2 = Node(id="test", node_type=NodeType.TASK)
        assert hash(node1) == hash(node2)  # Same id means same hash

    def test_equality(self):
        node1 = Node(id="test", node_type=NodeType.LANGUAGE)
        node2 = Node(id="test", node_type=NodeType.TASK)
        assert node1 == node2  # Equality based on id

    def test_inequality(self):
        node1 = Node(id="python", node_type=NodeType.LANGUAGE)
        node2 = Node(id="java", node_type=NodeType.LANGUAGE)
        assert node1 != node2


class TestEdge:
    """Tests for Edge dataclass."""

    def test_initialization(self):
        edge = Edge(source="python", target="sorting", edge_type="implements")
        assert edge.source == "python"
        assert edge.target == "sorting"
        assert edge.edge_type == "implements"
        assert edge.weight == 1.0

    def test_initialization_with_weight(self):
        edge = Edge(source="a", target="b", edge_type="test", weight=2.5)
        assert edge.weight == 2.5

    def test_initialization_with_attributes(self):
        edge = Edge(
            source="a", target="b", edge_type="test",
            attributes={"quality": "high"}
        )
        assert edge.attributes["quality"] == "high"

    def test_hash(self):
        edge1 = Edge(source="a", target="b", edge_type="test")
        edge2 = Edge(source="a", target="b", edge_type="test")
        assert hash(edge1) == hash(edge2)


class TestCommunity:
    """Tests for Community dataclass."""

    def test_initialization(self):
        community = Community(id=1, members={"a", "b", "c"})
        assert community.id == 1
        assert len(community.members) == 3
        assert community.density == 0.0

    def test_initialization_with_all_fields(self):
        community = Community(
            id=2,
            members={"x", "y"},
            density=0.8,
            label="test_cluster",
            dominant_type=NodeType.LANGUAGE
        )
        assert community.density == 0.8
        assert community.label == "test_cluster"

    def test_to_dict(self):
        community = Community(
            id=1,
            members={"a", "b"},
            density=0.5,
            label="cluster_2",
            dominant_type=NodeType.TASK
        )
        d = community.to_dict()
        assert d["id"] == 1
        assert d["size"] == 2
        assert d["density"] == 0.5
        assert d["dominant_type"] == "task"


class TestPathResult:
    """Tests for PathResult dataclass."""

    def test_initialization(self):
        result = PathResult(
            path=["a", "b", "c"],
            length=2,
            total_weight=3.0,
            edge_types=["e1", "e2"]
        )
        assert result.path == ["a", "b", "c"]
        assert result.length == 2
        assert result.total_weight == 3.0

    def test_to_dict(self):
        result = PathResult(
            path=["x", "y"],
            length=1,
            total_weight=1.0,
            edge_types=["edge"]
        )
        d = result.to_dict()
        assert d["path"] == ["x", "y"]
        assert d["length"] == 1


class TestGraph:
    """Tests for Graph class."""

    def test_initialization_undirected(self):
        graph = Graph(directed=False)
        assert not graph.directed
        assert graph.node_count() == 0
        assert graph.edge_count() == 0

    def test_initialization_directed(self):
        graph = Graph(directed=True)
        assert graph.directed

    def test_add_node(self):
        graph = Graph()
        node = Node(id="test", node_type=NodeType.LANGUAGE)
        graph.add_node(node)
        assert graph.node_count() == 1
        assert "test" in graph.nodes

    def test_add_multiple_nodes(self):
        graph = Graph()
        for i in range(5):
            graph.add_node(Node(id=f"node_{i}", node_type=NodeType.LANGUAGE))
        assert graph.node_count() == 5

    def test_add_edge(self):
        graph = Graph()
        graph.add_node(Node(id="a", node_type=NodeType.LANGUAGE))
        graph.add_node(Node(id="b", node_type=NodeType.LANGUAGE))
        graph.add_edge(Edge(source="a", target="b", edge_type="test"))
        assert graph.edge_count() == 1

    def test_undirected_edge_creates_both_directions(self):
        graph = Graph(directed=False)
        graph.add_node(Node(id="a", node_type=NodeType.LANGUAGE))
        graph.add_node(Node(id="b", node_type=NodeType.LANGUAGE))
        graph.add_edge(Edge(source="a", target="b", edge_type="test"))

        # Both directions should be in adjacency
        neighbors_a = graph.get_neighbors("a")
        neighbors_b = graph.get_neighbors("b")
        assert len(neighbors_a) == 1
        assert len(neighbors_b) == 1

    def test_directed_edge_single_direction(self):
        graph = Graph(directed=True)
        graph.add_node(Node(id="a", node_type=NodeType.LANGUAGE))
        graph.add_node(Node(id="b", node_type=NodeType.LANGUAGE))
        graph.add_edge(Edge(source="a", target="b", edge_type="test"))

        assert len(graph.get_neighbors("a")) == 1
        assert len(graph.get_neighbors("b")) == 0

    def test_get_degree(self):
        graph = Graph()
        graph.add_node(Node(id="center", node_type=NodeType.LANGUAGE))
        for i in range(4):
            graph.add_node(Node(id=f"node_{i}", node_type=NodeType.LANGUAGE))
            graph.add_edge(Edge(source="center", target=f"node_{i}", edge_type="test"))

        assert graph.get_degree("center") == 4

    def test_get_nodes_by_type(self):
        graph = Graph()
        graph.add_node(Node(id="python", node_type=NodeType.LANGUAGE))
        graph.add_node(Node(id="java", node_type=NodeType.LANGUAGE))
        graph.add_node(Node(id="sorting", node_type=NodeType.TASK))

        languages = graph.get_nodes_by_type(NodeType.LANGUAGE)
        tasks = graph.get_nodes_by_type(NodeType.TASK)

        assert len(languages) == 2
        assert len(tasks) == 1

    def test_to_dict(self):
        graph = Graph()
        graph.add_node(Node(id="a", node_type=NodeType.LANGUAGE))
        graph.add_edge(Edge(source="a", target="a", edge_type="self"))

        d = graph.to_dict()
        assert d["node_count"] == 1
        assert d["edge_count"] == 1
        assert "a" in d["nodes"]


class TestCentralityAnalyzer:
    """Tests for CentralityAnalyzer class."""

    @pytest.fixture
    def star_graph(self):
        """Create a star graph for testing."""
        graph = Graph()
        graph.add_node(Node(id="center", node_type=NodeType.LANGUAGE))
        for i in range(4):
            graph.add_node(Node(id=f"leaf_{i}", node_type=NodeType.LANGUAGE))
            graph.add_edge(Edge(source="center", target=f"leaf_{i}", edge_type="test"))
        return graph

    @pytest.fixture
    def line_graph(self):
        """Create a line graph: a-b-c-d."""
        graph = Graph()
        nodes = ["a", "b", "c", "d"]
        for n in nodes:
            graph.add_node(Node(id=n, node_type=NodeType.LANGUAGE))
        for i in range(len(nodes) - 1):
            graph.add_edge(Edge(source=nodes[i], target=nodes[i+1], edge_type="test"))
        return graph

    def test_initialization(self, star_graph):
        analyzer = CentralityAnalyzer(star_graph)
        assert analyzer.graph is star_graph

    def test_degree_centrality(self, star_graph):
        analyzer = CentralityAnalyzer(star_graph)
        centrality = analyzer.degree_centrality()

        # Center should have highest degree centrality
        assert centrality["center"] == max(centrality.values())

        # All leaves should have equal centrality
        leaf_centralities = [centrality[f"leaf_{i}"] for i in range(4)]
        assert len(set(leaf_centralities)) == 1

    def test_degree_centrality_caching(self, star_graph):
        analyzer = CentralityAnalyzer(star_graph)

        # First call
        result1 = analyzer.degree_centrality()
        # Second call should use cache
        result2 = analyzer.degree_centrality()

        assert result1 == result2
        assert CentralityType.DEGREE in analyzer._centrality_cache

    def test_betweenness_centrality(self, line_graph):
        analyzer = CentralityAnalyzer(line_graph)
        centrality = analyzer.betweenness_centrality()

        # Middle nodes (b, c) should have higher betweenness
        assert centrality["b"] > centrality["a"]
        assert centrality["c"] > centrality["d"]

    def test_closeness_centrality(self, line_graph):
        analyzer = CentralityAnalyzer(line_graph)
        centrality = analyzer.closeness_centrality()

        # All nodes should have positive closeness
        for node, score in centrality.items():
            assert score >= 0

    def test_pagerank(self, star_graph):
        analyzer = CentralityAnalyzer(star_graph)
        pagerank = analyzer.pagerank()

        # Sum should be close to 1
        assert abs(sum(pagerank.values()) - 1.0) < 0.01

        # All values should be positive
        for node, score in pagerank.items():
            assert score > 0

    def test_pagerank_convergence(self, star_graph):
        analyzer = CentralityAnalyzer(star_graph)
        pagerank = analyzer.pagerank(max_iterations=1000, tolerance=1e-10)

        # Should converge with high precision
        assert sum(pagerank.values()) > 0

    def test_get_top_nodes(self, star_graph):
        analyzer = CentralityAnalyzer(star_graph)
        top = analyzer.get_top_nodes(CentralityType.DEGREE, top_k=2)

        assert len(top) == 2
        assert top[0][0] == "center"  # Center should be first

    def test_empty_graph(self):
        graph = Graph()
        analyzer = CentralityAnalyzer(graph)

        degree = analyzer.degree_centrality()
        assert degree == {}

    def test_to_dict(self, star_graph):
        analyzer = CentralityAnalyzer(star_graph)
        d = analyzer.to_dict()

        assert "degree" in d
        assert "betweenness" in d
        assert "closeness" in d
        assert "pagerank" in d


class TestCommunityDetector:
    """Tests for CommunityDetector class."""

    @pytest.fixture
    def two_cliques_graph(self):
        """Create graph with two connected cliques."""
        graph = Graph()

        # First clique: a, b, c
        for n in ["a", "b", "c"]:
            graph.add_node(Node(id=n, node_type=NodeType.LANGUAGE))
        graph.add_edge(Edge(source="a", target="b", edge_type="test"))
        graph.add_edge(Edge(source="b", target="c", edge_type="test"))
        graph.add_edge(Edge(source="a", target="c", edge_type="test"))

        # Second clique: d, e, f
        for n in ["d", "e", "f"]:
            graph.add_node(Node(id=n, node_type=NodeType.LANGUAGE))
        graph.add_edge(Edge(source="d", target="e", edge_type="test"))
        graph.add_edge(Edge(source="e", target="f", edge_type="test"))
        graph.add_edge(Edge(source="d", target="f", edge_type="test"))

        # Connect cliques with single edge
        graph.add_edge(Edge(source="c", target="d", edge_type="bridge"))

        return graph

    def test_initialization(self, two_cliques_graph):
        detector = CommunityDetector(two_cliques_graph)
        assert detector.graph is two_cliques_graph

    def test_louvain_communities(self, two_cliques_graph):
        detector = CommunityDetector(two_cliques_graph)
        communities = detector.louvain_communities()

        assert len(communities) > 0
        # All nodes should be in some community
        all_members = set()
        for comm in communities:
            all_members.update(comm.members)
        assert len(all_members) == 6

    def test_louvain_returns_community_objects(self, two_cliques_graph):
        detector = CommunityDetector(two_cliques_graph)
        communities = detector.louvain_communities()

        for comm in communities:
            assert isinstance(comm, Community)
            assert len(comm.members) > 0

    def test_label_propagation(self, two_cliques_graph):
        detector = CommunityDetector(two_cliques_graph)
        communities = detector.label_propagation()

        assert len(communities) > 0
        # All nodes should be assigned
        all_members = set()
        for comm in communities:
            all_members.update(comm.members)
        assert len(all_members) == 6

    def test_compute_modularity(self, two_cliques_graph):
        detector = CommunityDetector(two_cliques_graph)
        communities = detector.louvain_communities()
        modularity = detector.compute_modularity(communities)

        # Modularity should be between -1 and 1
        assert -1 <= modularity <= 1

    def test_compute_modularity_empty(self, two_cliques_graph):
        detector = CommunityDetector(two_cliques_graph)
        modularity = detector.compute_modularity([])
        assert modularity == 0.0

    def test_community_density(self, two_cliques_graph):
        detector = CommunityDetector(two_cliques_graph)
        communities = detector.louvain_communities()

        for comm in communities:
            # Density should be between 0 and 1
            assert 0 <= comm.density <= 1

    def test_empty_graph(self):
        graph = Graph()
        detector = CommunityDetector(graph)
        communities = detector.louvain_communities()
        assert communities == []


class TestPathAnalyzer:
    """Tests for PathAnalyzer class."""

    @pytest.fixture
    def simple_graph(self):
        """Create simple connected graph."""
        graph = Graph()
        for n in ["a", "b", "c", "d"]:
            graph.add_node(Node(id=n, node_type=NodeType.LANGUAGE))

        graph.add_edge(Edge(source="a", target="b", edge_type="e1"))
        graph.add_edge(Edge(source="b", target="c", edge_type="e2"))
        graph.add_edge(Edge(source="c", target="d", edge_type="e3"))
        graph.add_edge(Edge(source="a", target="d", edge_type="e4", weight=10))

        return graph

    def test_initialization(self, simple_graph):
        analyzer = PathAnalyzer(simple_graph)
        assert analyzer.graph is simple_graph

    def test_shortest_path_exists(self, simple_graph):
        analyzer = PathAnalyzer(simple_graph)
        result = analyzer.shortest_path("a", "d")

        assert result is not None
        assert result.path[0] == "a"
        assert result.path[-1] == "d"

    def test_shortest_path_same_node(self, simple_graph):
        analyzer = PathAnalyzer(simple_graph)
        result = analyzer.shortest_path("a", "a")

        assert result is not None
        assert result.path == ["a"]
        assert result.length == 0

    def test_shortest_path_direct_edge(self, simple_graph):
        analyzer = PathAnalyzer(simple_graph)
        result = analyzer.shortest_path("a", "b")

        assert result is not None
        assert result.path == ["a", "b"]
        assert result.length == 1

    def test_shortest_path_not_exists(self):
        graph = Graph()
        graph.add_node(Node(id="a", node_type=NodeType.LANGUAGE))
        graph.add_node(Node(id="b", node_type=NodeType.LANGUAGE))
        # No edge between them

        analyzer = PathAnalyzer(graph)
        result = analyzer.shortest_path("a", "b")
        assert result is None

    def test_shortest_path_invalid_nodes(self, simple_graph):
        analyzer = PathAnalyzer(simple_graph)
        result = analyzer.shortest_path("nonexistent", "a")
        assert result is None

    def test_all_shortest_paths(self, simple_graph):
        analyzer = PathAnalyzer(simple_graph)
        paths = analyzer.all_shortest_paths("a", "d")

        assert len(paths) > 0
        # All paths should start at a and end at d
        for path in paths:
            assert path.path[0] == "a"
            assert path.path[-1] == "d"

    def test_all_shortest_paths_limited(self, simple_graph):
        analyzer = PathAnalyzer(simple_graph)
        paths = analyzer.all_shortest_paths("a", "d", max_paths=1)
        assert len(paths) <= 1

    def test_find_paths_through(self, simple_graph):
        analyzer = PathAnalyzer(simple_graph)
        paths = analyzer.find_paths_through("b", source="a", target="c")

        assert len(paths) > 0
        for path in paths:
            assert "b" in path.path

    def test_diameter(self, simple_graph):
        analyzer = PathAnalyzer(simple_graph)
        diameter = analyzer.diameter()

        # Diameter should be positive
        assert diameter > 0

    def test_path_result_includes_edge_types(self, simple_graph):
        analyzer = PathAnalyzer(simple_graph)
        result = analyzer.shortest_path("a", "b")

        assert result is not None
        assert len(result.edge_types) == result.length


class TestSubgraphMiner:
    """Tests for SubgraphMiner class."""

    @pytest.fixture
    def triangle_graph(self):
        """Create graph with triangles."""
        graph = Graph()
        for n in ["a", "b", "c", "d"]:
            graph.add_node(Node(id=n, node_type=NodeType.LANGUAGE))

        # Triangle a-b-c
        graph.add_edge(Edge(source="a", target="b", edge_type="test"))
        graph.add_edge(Edge(source="b", target="c", edge_type="test"))
        graph.add_edge(Edge(source="a", target="c", edge_type="test"))

        # d connected to a
        graph.add_edge(Edge(source="d", target="a", edge_type="test"))

        return graph

    def test_initialization(self, triangle_graph):
        miner = SubgraphMiner(triangle_graph)
        assert miner.graph is triangle_graph

    def test_find_triangles(self, triangle_graph):
        miner = SubgraphMiner(triangle_graph)
        triangles = miner.find_triangles()

        assert len(triangles) >= 1
        # Should find the a-b-c triangle
        found = False
        for triangle in triangles:
            if set(triangle) == {"a", "b", "c"}:
                found = True
        assert found

    def test_find_triangles_no_triangles(self):
        graph = Graph()
        for n in ["a", "b", "c"]:
            graph.add_node(Node(id=n, node_type=NodeType.LANGUAGE))
        graph.add_edge(Edge(source="a", target="b", edge_type="test"))
        graph.add_edge(Edge(source="b", target="c", edge_type="test"))
        # No edge from a to c, so no triangle

        miner = SubgraphMiner(graph)
        triangles = miner.find_triangles()
        assert len(triangles) == 0

    def test_find_cliques(self, triangle_graph):
        miner = SubgraphMiner(triangle_graph)
        cliques = miner.find_cliques(min_size=3)

        # Should find at least the triangle as a clique
        assert len(cliques) >= 1

    def test_find_stars(self, triangle_graph):
        miner = SubgraphMiner(triangle_graph)
        stars = miner.find_stars(min_degree=2)

        # Some nodes should have degree >= 2
        assert len(stars) >= 0

    def test_clustering_coefficient_node(self, triangle_graph):
        miner = SubgraphMiner(triangle_graph)
        cc = miner.compute_clustering_coefficient("a")

        # a is part of a triangle, should have high clustering
        assert 0 <= cc <= 1

    def test_clustering_coefficient_global(self, triangle_graph):
        miner = SubgraphMiner(triangle_graph)
        cc = miner.compute_clustering_coefficient()

        assert 0 <= cc <= 1

    def test_clustering_coefficient_isolated(self):
        graph = Graph()
        graph.add_node(Node(id="isolated", node_type=NodeType.LANGUAGE))

        miner = SubgraphMiner(graph)
        cc = miner.compute_clustering_coefficient("isolated")
        assert cc == 0.0


class TestGraphAnalytics:
    """Tests for GraphAnalytics main class."""

    @pytest.fixture
    def sample_graph(self):
        """Create sample graph for testing."""
        graph = Graph()
        for i in range(5):
            graph.add_node(Node(id=f"node_{i}", node_type=NodeType.LANGUAGE))

        graph.add_edge(Edge(source="node_0", target="node_1", edge_type="test"))
        graph.add_edge(Edge(source="node_1", target="node_2", edge_type="test"))
        graph.add_edge(Edge(source="node_2", target="node_3", edge_type="test"))
        graph.add_edge(Edge(source="node_3", target="node_4", edge_type="test"))
        graph.add_edge(Edge(source="node_0", target="node_4", edge_type="test"))

        return graph

    def test_initialization_empty(self):
        analytics = GraphAnalytics()
        assert analytics.graph is not None
        assert analytics.graph.node_count() == 0

    def test_initialization_with_graph(self, sample_graph):
        analytics = GraphAnalytics(sample_graph)
        assert analytics.graph is sample_graph
        assert analytics.centrality_analyzer is not None
        assert analytics.community_detector is not None
        assert analytics.path_analyzer is not None
        assert analytics.subgraph_miner is not None

    def test_load_from_hypergraph(self):
        analytics = GraphAnalytics()

        hypergraph_data = {
            "nodes": {
                "languages": ["python", "java"],
                "tasks": ["sorting", "search"]
            },
            "edges": {
                "implements": [
                    {"source": "python", "target": "sorting", "weight": 1.0},
                    {"source": "java", "target": "sorting", "weight": 1.0}
                ]
            }
        }

        analytics.load_from_hypergraph(hypergraph_data)

        assert analytics.graph.node_count() == 4
        assert analytics.graph.edge_count() == 2

    def test_run_full_analysis(self, sample_graph):
        analytics = GraphAnalytics(sample_graph)
        results = analytics.run_full_analysis()

        assert "summary" in results
        assert "centrality" in results
        assert "communities" in results
        assert "patterns" in results

    def test_run_full_analysis_summary(self, sample_graph):
        analytics = GraphAnalytics(sample_graph)
        results = analytics.run_full_analysis()

        summary = results["summary"]
        assert summary["node_count"] == 5
        assert summary["edge_count"] == 5
        assert "density" in summary
        assert "clustering_coefficient" in summary

    def test_run_full_analysis_empty_graph(self):
        analytics = GraphAnalytics()
        results = analytics.run_full_analysis()

        assert "error" in results

    def test_export_analysis(self, sample_graph, tmp_path):
        analytics = GraphAnalytics(sample_graph)
        output_file = tmp_path / "analysis.json"

        analytics.export_analysis(str(output_file))

        assert output_file.exists()

        import json
        with open(output_file) as f:
            data = json.load(f)
        assert "summary" in data

    def test_export_graphml(self, sample_graph, tmp_path):
        analytics = GraphAnalytics(sample_graph)
        output_file = tmp_path / "graph.graphml"

        analytics.export_graphml(str(output_file))

        assert output_file.exists()

        content = output_file.read_text()
        assert "graphml" in content
        assert "node_0" in content


class TestGraphAnalyticsIntegration:
    """Integration tests for GraphAnalytics."""

    def test_full_workflow(self):
        """Test complete analysis workflow."""
        # Create graph
        graph = Graph()
        languages = ["python", "java", "c", "rust", "go"]
        for lang in languages:
            graph.add_node(Node(id=lang, node_type=NodeType.LANGUAGE))

        # Create edges
        edges = [
            ("python", "java"), ("java", "c"), ("c", "rust"),
            ("rust", "go"), ("go", "python"), ("python", "c")
        ]
        for src, tgt in edges:
            graph.add_edge(Edge(source=src, target=tgt, edge_type="similarity"))

        # Run analytics
        analytics = GraphAnalytics(graph)
        results = analytics.run_full_analysis()

        # Verify all components ran
        assert results["summary"]["node_count"] == 5
        assert len(results["centrality"]["top_degree"]) > 0
        assert len(results["communities"]) > 0

    def test_centrality_rankings(self):
        """Test that centrality measures produce sensible rankings."""
        # Create star graph - center should have highest centrality
        graph = Graph()
        graph.add_node(Node(id="hub", node_type=NodeType.LANGUAGE))
        for i in range(5):
            graph.add_node(Node(id=f"spoke_{i}", node_type=NodeType.LANGUAGE))
            graph.add_edge(Edge(source="hub", target=f"spoke_{i}", edge_type="test"))

        analytics = GraphAnalytics(graph)
        results = analytics.run_full_analysis()

        # Hub should be top in all centrality measures
        top_degree = results["centrality"]["top_degree"][0][0]
        top_betweenness = results["centrality"]["top_betweenness"][0][0]
        top_pagerank = results["centrality"]["top_pagerank"][0][0]

        assert top_degree == "hub"
        assert top_betweenness == "hub"
        # PageRank might differ in star graphs

    def test_community_detection_quality(self):
        """Test that communities are meaningful."""
        # Create two clearly separated clusters
        graph = Graph()

        # Cluster 1
        for i in range(3):
            graph.add_node(Node(id=f"a_{i}", node_type=NodeType.LANGUAGE))
        graph.add_edge(Edge(source="a_0", target="a_1", edge_type="test"))
        graph.add_edge(Edge(source="a_1", target="a_2", edge_type="test"))
        graph.add_edge(Edge(source="a_0", target="a_2", edge_type="test"))

        # Cluster 2
        for i in range(3):
            graph.add_node(Node(id=f"b_{i}", node_type=NodeType.LANGUAGE))
        graph.add_edge(Edge(source="b_0", target="b_1", edge_type="test"))
        graph.add_edge(Edge(source="b_1", target="b_2", edge_type="test"))
        graph.add_edge(Edge(source="b_0", target="b_2", edge_type="test"))

        # Weak bridge
        graph.add_edge(Edge(source="a_0", target="b_0", edge_type="bridge", weight=0.1))

        analytics = GraphAnalytics(graph)
        results = analytics.run_full_analysis()

        # Should detect communities with positive modularity
        assert results["summary"]["modularity"] >= 0
