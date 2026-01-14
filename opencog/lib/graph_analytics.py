"""
Graph Analytics Module for RosettaCog Hypergraph

Implements advanced graph analysis algorithms for the language-task-paradigm hypergraph:
- Community detection: Identify language clusters
- Centrality analysis: Find influential languages/paradigms
- Path analysis: Discover optimal paradigm transitions
- Subgraph mining: Extract frequent patterns

Phase 2 of RosettaCog roadmap: Intelligence Amplification
"""

import logging
import json
import math
from typing import Dict, List, Any, Optional, Set, Tuple
from dataclasses import dataclass, field
from collections import defaultdict
from enum import Enum


logger = logging.getLogger(__name__)


class CentralityType(Enum):
    """Types of centrality measures"""
    DEGREE = "degree"
    BETWEENNESS = "betweenness"
    CLOSENESS = "closeness"
    PAGERANK = "pagerank"
    EIGENVECTOR = "eigenvector"


class NodeType(Enum):
    """Types of nodes in the hypergraph"""
    LANGUAGE = "language"
    TASK = "task"
    PARADIGM = "paradigm"
    DOMAIN = "domain"
    SUBCATEGORY = "subcategory"


@dataclass
class Node:
    """Represents a node in the graph"""
    id: str
    node_type: NodeType
    attributes: Dict[str, Any] = field(default_factory=dict)

    def __hash__(self):
        return hash(self.id)

    def __eq__(self, other):
        if isinstance(other, Node):
            return self.id == other.id
        return False


@dataclass
class Edge:
    """Represents an edge in the graph"""
    source: str
    target: str
    edge_type: str
    weight: float = 1.0
    attributes: Dict[str, Any] = field(default_factory=dict)

    def __hash__(self):
        return hash((self.source, self.target, self.edge_type))


@dataclass
class Community:
    """Represents a detected community"""
    id: int
    members: Set[str]
    density: float = 0.0
    label: str = ""
    dominant_type: Optional[NodeType] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "members": list(self.members),
            "density": self.density,
            "label": self.label,
            "dominant_type": self.dominant_type.value if self.dominant_type else None,
            "size": len(self.members)
        }


@dataclass
class PathResult:
    """Result of a path analysis"""
    path: List[str]
    length: int
    total_weight: float
    edge_types: List[str]

    def to_dict(self) -> Dict[str, Any]:
        return {
            "path": self.path,
            "length": self.length,
            "total_weight": self.total_weight,
            "edge_types": self.edge_types
        }


class Graph:
    """
    Graph data structure for analytics.

    Provides efficient adjacency list representation with
    support for directed/undirected edges and weights.
    """

    def __init__(self, directed: bool = False):
        self.directed = directed
        self.nodes: Dict[str, Node] = {}
        self.edges: List[Edge] = []
        self.adjacency: Dict[str, List[Tuple[str, float, str]]] = defaultdict(list)
        self.reverse_adjacency: Dict[str, List[Tuple[str, float, str]]] = defaultdict(list)

    def add_node(self, node: Node):
        """Add a node to the graph"""
        self.nodes[node.id] = node

    def add_edge(self, edge: Edge):
        """Add an edge to the graph"""
        self.edges.append(edge)
        self.adjacency[edge.source].append((edge.target, edge.weight, edge.edge_type))
        self.reverse_adjacency[edge.target].append((edge.source, edge.weight, edge.edge_type))

        if not self.directed:
            self.adjacency[edge.target].append((edge.source, edge.weight, edge.edge_type))
            self.reverse_adjacency[edge.source].append((edge.target, edge.weight, edge.edge_type))

    def get_neighbors(self, node_id: str) -> List[Tuple[str, float, str]]:
        """Get neighbors of a node"""
        return self.adjacency.get(node_id, [])

    def get_degree(self, node_id: str) -> int:
        """Get degree of a node"""
        return len(self.adjacency.get(node_id, []))

    def get_nodes_by_type(self, node_type: NodeType) -> List[Node]:
        """Get all nodes of a specific type"""
        return [n for n in self.nodes.values() if n.node_type == node_type]

    def node_count(self) -> int:
        """Get total number of nodes"""
        return len(self.nodes)

    def edge_count(self) -> int:
        """Get total number of edges"""
        return len(self.edges)

    def to_dict(self) -> Dict[str, Any]:
        """Convert graph to dictionary"""
        return {
            "directed": self.directed,
            "node_count": self.node_count(),
            "edge_count": self.edge_count(),
            "nodes": {nid: {"type": n.node_type.value, "attributes": n.attributes}
                     for nid, n in self.nodes.items()},
            "edges": [{"source": e.source, "target": e.target,
                      "type": e.edge_type, "weight": e.weight}
                     for e in self.edges]
        }


class CentralityAnalyzer:
    """
    Computes various centrality measures for graph nodes.

    Centrality helps identify:
    - Most influential languages (high degree/PageRank)
    - Bridge languages connecting paradigms (high betweenness)
    - Core languages in clusters (high closeness)
    """

    def __init__(self, graph: Graph):
        self.graph = graph
        self._centrality_cache: Dict[CentralityType, Dict[str, float]] = {}

    def degree_centrality(self) -> Dict[str, float]:
        """
        Compute degree centrality for all nodes.

        DC(v) = degree(v) / (n - 1)
        """
        if CentralityType.DEGREE in self._centrality_cache:
            return self._centrality_cache[CentralityType.DEGREE]

        n = self.graph.node_count()
        if n <= 1:
            return {nid: 0.0 for nid in self.graph.nodes}

        centrality = {}
        max_degree = n - 1

        for node_id in self.graph.nodes:
            degree = self.graph.get_degree(node_id)
            centrality[node_id] = degree / max_degree

        self._centrality_cache[CentralityType.DEGREE] = centrality
        return centrality

    def betweenness_centrality(self) -> Dict[str, float]:
        """
        Compute betweenness centrality using Brandes algorithm.

        BC(v) = sum over all s,t of (sigma_st(v) / sigma_st)
        where sigma_st is the number of shortest paths from s to t
        and sigma_st(v) is the number passing through v.
        """
        if CentralityType.BETWEENNESS in self._centrality_cache:
            return self._centrality_cache[CentralityType.BETWEENNESS]

        centrality = {nid: 0.0 for nid in self.graph.nodes}
        nodes = list(self.graph.nodes.keys())

        for s in nodes:
            # BFS from s
            stack = []
            predecessors = {nid: [] for nid in nodes}
            sigma = {nid: 0.0 for nid in nodes}
            sigma[s] = 1.0
            distance = {nid: -1 for nid in nodes}
            distance[s] = 0

            queue = [s]
            while queue:
                v = queue.pop(0)
                stack.append(v)

                for neighbor, weight, etype in self.graph.get_neighbors(v):
                    if neighbor not in distance:
                        continue
                    if distance[neighbor] < 0:
                        distance[neighbor] = distance[v] + 1
                        queue.append(neighbor)
                    if distance[neighbor] == distance[v] + 1:
                        sigma[neighbor] += sigma[v]
                        predecessors[neighbor].append(v)

            # Accumulation
            delta = {nid: 0.0 for nid in nodes}
            while stack:
                w = stack.pop()
                for v in predecessors[w]:
                    if sigma[w] > 0:
                        delta[v] += (sigma[v] / sigma[w]) * (1 + delta[w])
                if w != s:
                    centrality[w] += delta[w]

        # Normalize
        n = len(nodes)
        if n > 2:
            norm = 2.0 / ((n - 1) * (n - 2))
            for nid in centrality:
                centrality[nid] *= norm

        self._centrality_cache[CentralityType.BETWEENNESS] = centrality
        return centrality

    def closeness_centrality(self) -> Dict[str, float]:
        """
        Compute closeness centrality.

        CC(v) = (n - 1) / sum of shortest path distances from v
        """
        if CentralityType.CLOSENESS in self._centrality_cache:
            return self._centrality_cache[CentralityType.CLOSENESS]

        centrality = {}
        nodes = list(self.graph.nodes.keys())
        n = len(nodes)

        for node in nodes:
            # BFS to find distances
            distances = self._bfs_distances(node)

            total_dist = sum(d for d in distances.values() if d > 0)
            reachable = sum(1 for d in distances.values() if d > 0)

            if total_dist > 0 and reachable > 0:
                centrality[node] = reachable / total_dist
            else:
                centrality[node] = 0.0

        self._centrality_cache[CentralityType.CLOSENESS] = centrality
        return centrality

    def pagerank(self, damping: float = 0.85, max_iterations: int = 100,
                 tolerance: float = 1e-6) -> Dict[str, float]:
        """
        Compute PageRank centrality.

        PR(v) = (1-d)/n + d * sum over u linking to v of (PR(u) / outdegree(u))
        """
        if CentralityType.PAGERANK in self._centrality_cache:
            return self._centrality_cache[CentralityType.PAGERANK]

        nodes = list(self.graph.nodes.keys())
        n = len(nodes)

        if n == 0:
            return {}

        # Initialize
        pagerank = {nid: 1.0 / n for nid in nodes}

        for iteration in range(max_iterations):
            new_pagerank = {}

            for node in nodes:
                # Get incoming edges
                incoming = self.graph.reverse_adjacency.get(node, [])

                rank_sum = 0.0
                for source, weight, etype in incoming:
                    out_degree = self.graph.get_degree(source)
                    if out_degree > 0:
                        rank_sum += pagerank[source] / out_degree

                new_pagerank[node] = (1 - damping) / n + damping * rank_sum

            # Check convergence
            diff = sum(abs(new_pagerank[n] - pagerank[n]) for n in nodes)
            pagerank = new_pagerank

            if diff < tolerance:
                break

        self._centrality_cache[CentralityType.PAGERANK] = pagerank
        return pagerank

    def _bfs_distances(self, start: str) -> Dict[str, int]:
        """BFS to compute distances from start node"""
        distances = {nid: -1 for nid in self.graph.nodes}
        distances[start] = 0

        queue = [start]
        while queue:
            current = queue.pop(0)
            for neighbor, weight, etype in self.graph.get_neighbors(current):
                if neighbor in distances and distances[neighbor] < 0:
                    distances[neighbor] = distances[current] + 1
                    queue.append(neighbor)

        return distances

    def get_top_nodes(self, centrality_type: CentralityType,
                      top_k: int = 10) -> List[Tuple[str, float]]:
        """Get top-k nodes by centrality measure"""
        if centrality_type == CentralityType.DEGREE:
            centrality = self.degree_centrality()
        elif centrality_type == CentralityType.BETWEENNESS:
            centrality = self.betweenness_centrality()
        elif centrality_type == CentralityType.CLOSENESS:
            centrality = self.closeness_centrality()
        elif centrality_type == CentralityType.PAGERANK:
            centrality = self.pagerank()
        else:
            centrality = self.degree_centrality()

        sorted_nodes = sorted(centrality.items(), key=lambda x: x[1], reverse=True)
        return sorted_nodes[:top_k]

    def to_dict(self) -> Dict[str, Any]:
        """Export all centrality measures"""
        return {
            "degree": self.degree_centrality(),
            "betweenness": self.betweenness_centrality(),
            "closeness": self.closeness_centrality(),
            "pagerank": self.pagerank()
        }


class CommunityDetector:
    """
    Detects communities in the graph using various algorithms.

    Helps identify:
    - Language families/clusters
    - Paradigm groupings
    - Task category clusters
    """

    def __init__(self, graph: Graph):
        self.graph = graph

    def louvain_communities(self, resolution: float = 1.0) -> List[Community]:
        """
        Detect communities using Louvain algorithm (simplified version).

        Optimizes modularity:
        Q = 1/(2m) * sum over all pairs (A_ij - k_i*k_j/(2m)) * delta(c_i, c_j)
        """
        nodes = list(self.graph.nodes.keys())
        if not nodes:
            return []

        # Initialize: each node in its own community
        node_to_community = {node: i for i, node in enumerate(nodes)}

        # Compute total edge weight
        total_weight = sum(e.weight for e in self.graph.edges)
        if total_weight == 0:
            total_weight = 1.0

        # Compute node weights (sum of incident edge weights)
        node_weights = {node: 0.0 for node in nodes}
        for edge in self.graph.edges:
            node_weights[edge.source] += edge.weight
            node_weights[edge.target] += edge.weight

        # Iteratively move nodes to improve modularity
        improved = True
        iterations = 0
        max_iterations = 100

        while improved and iterations < max_iterations:
            improved = False
            iterations += 1

            for node in nodes:
                current_community = node_to_community[node]

                # Find neighboring communities
                neighbor_communities = set()
                for neighbor, weight, etype in self.graph.get_neighbors(node):
                    neighbor_communities.add(node_to_community[neighbor])

                # Try moving to each neighboring community
                best_community = current_community
                best_delta_q = 0.0

                for target_community in neighbor_communities:
                    if target_community == current_community:
                        continue

                    delta_q = self._compute_modularity_delta(
                        node, current_community, target_community,
                        node_to_community, node_weights, total_weight, resolution
                    )

                    if delta_q > best_delta_q:
                        best_delta_q = delta_q
                        best_community = target_community

                if best_community != current_community:
                    node_to_community[node] = best_community
                    improved = True

        # Build community objects
        community_members = defaultdict(set)
        for node, comm_id in node_to_community.items():
            community_members[comm_id].add(node)

        communities = []
        for comm_id, members in community_members.items():
            if members:
                community = Community(
                    id=comm_id,
                    members=members,
                    density=self._compute_density(members),
                    label=self._generate_label(members),
                    dominant_type=self._get_dominant_type(members)
                )
                communities.append(community)

        return sorted(communities, key=lambda c: len(c.members), reverse=True)

    def _compute_modularity_delta(
        self, node: str, from_comm: int, to_comm: int,
        node_to_community: Dict[str, int], node_weights: Dict[str, float],
        total_weight: float, resolution: float
    ) -> float:
        """Compute change in modularity from moving node"""
        # Simplified computation
        edges_to_from = 0.0
        edges_to_to = 0.0

        for neighbor, weight, etype in self.graph.get_neighbors(node):
            if node_to_community[neighbor] == from_comm:
                edges_to_from += weight
            elif node_to_community[neighbor] == to_comm:
                edges_to_to += weight

        # Delta Q approximation
        delta_q = (edges_to_to - edges_to_from) / total_weight

        return delta_q

    def _compute_density(self, members: Set[str]) -> float:
        """Compute edge density within community"""
        if len(members) < 2:
            return 1.0

        internal_edges = 0
        for node in members:
            for neighbor, weight, etype in self.graph.get_neighbors(node):
                if neighbor in members:
                    internal_edges += 1

        max_edges = len(members) * (len(members) - 1)
        if not self.graph.directed:
            internal_edges //= 2
            max_edges //= 2

        return internal_edges / max_edges if max_edges > 0 else 0.0

    def _generate_label(self, members: Set[str]) -> str:
        """Generate a label for the community"""
        # Use the most common node type
        type_counts = defaultdict(int)
        for member in members:
            if member in self.graph.nodes:
                type_counts[self.graph.nodes[member].node_type] += 1

        if type_counts:
            dominant_type = max(type_counts.items(), key=lambda x: x[1])[0]
            return f"{dominant_type.value}_cluster_{len(members)}"
        return f"cluster_{len(members)}"

    def _get_dominant_type(self, members: Set[str]) -> Optional[NodeType]:
        """Get the most common node type in community"""
        type_counts = defaultdict(int)
        for member in members:
            if member in self.graph.nodes:
                type_counts[self.graph.nodes[member].node_type] += 1

        if type_counts:
            return max(type_counts.items(), key=lambda x: x[1])[0]
        return None

    def label_propagation(self, max_iterations: int = 100) -> List[Community]:
        """
        Detect communities using label propagation.

        Simple and fast algorithm:
        1. Initialize each node with unique label
        2. Iteratively update labels to most common neighbor label
        3. Stop when no labels change
        """
        nodes = list(self.graph.nodes.keys())
        if not nodes:
            return []

        # Initialize labels
        labels = {node: i for i, node in enumerate(nodes)}

        for iteration in range(max_iterations):
            changed = False

            # Shuffle nodes for randomization
            import random
            random.shuffle(nodes)

            for node in nodes:
                neighbors = self.graph.get_neighbors(node)
                if not neighbors:
                    continue

                # Count neighbor labels
                label_counts = defaultdict(float)
                for neighbor, weight, etype in neighbors:
                    label_counts[labels[neighbor]] += weight

                # Update to most common label
                if label_counts:
                    best_label = max(label_counts.items(), key=lambda x: x[1])[0]
                    if labels[node] != best_label:
                        labels[node] = best_label
                        changed = True

            if not changed:
                break

        # Build communities
        community_members = defaultdict(set)
        for node, label in labels.items():
            community_members[label].add(node)

        communities = []
        for label, members in community_members.items():
            community = Community(
                id=label,
                members=members,
                density=self._compute_density(members),
                label=self._generate_label(members),
                dominant_type=self._get_dominant_type(members)
            )
            communities.append(community)

        return sorted(communities, key=lambda c: len(c.members), reverse=True)

    def compute_modularity(self, communities: List[Community]) -> float:
        """
        Compute modularity score for a community assignment.

        Q = sum over communities (e_c - a_c^2)
        where e_c is fraction of edges within community c
        and a_c is fraction of edge ends in community c
        """
        if not communities:
            return 0.0

        # Build node to community mapping
        node_to_comm = {}
        for comm in communities:
            for member in comm.members:
                node_to_comm[member] = comm.id

        total_edges = len(self.graph.edges)
        if total_edges == 0:
            return 0.0

        modularity = 0.0

        for comm in communities:
            # Count edges within community
            internal = 0
            for node in comm.members:
                for neighbor, weight, etype in self.graph.get_neighbors(node):
                    if neighbor in comm.members:
                        internal += 1

            if not self.graph.directed:
                internal //= 2

            e_c = internal / total_edges

            # Count total edges incident to community
            total = 0
            for node in comm.members:
                total += self.graph.get_degree(node)

            if not self.graph.directed:
                total //= 2

            a_c = total / (2 * total_edges) if total_edges > 0 else 0

            modularity += e_c - a_c * a_c

        return modularity


class PathAnalyzer:
    """
    Analyzes paths in the graph.

    Helps discover:
    - Optimal paradigm transitions
    - Language migration paths
    - Task similarity routes
    """

    def __init__(self, graph: Graph):
        self.graph = graph

    def shortest_path(self, source: str, target: str) -> Optional[PathResult]:
        """
        Find shortest path between two nodes using BFS.
        """
        if source not in self.graph.nodes or target not in self.graph.nodes:
            return None

        if source == target:
            return PathResult(path=[source], length=0, total_weight=0, edge_types=[])

        # BFS
        visited = {source}
        queue = [(source, [source], 0.0, [])]

        while queue:
            current, path, weight, edges = queue.pop(0)

            for neighbor, edge_weight, edge_type in self.graph.get_neighbors(current):
                if neighbor == target:
                    return PathResult(
                        path=path + [neighbor],
                        length=len(path),
                        total_weight=weight + edge_weight,
                        edge_types=edges + [edge_type]
                    )

                if neighbor not in visited:
                    visited.add(neighbor)
                    queue.append((
                        neighbor,
                        path + [neighbor],
                        weight + edge_weight,
                        edges + [edge_type]
                    ))

        return None

    def all_shortest_paths(self, source: str, target: str,
                          max_paths: int = 10) -> List[PathResult]:
        """Find all shortest paths between two nodes."""
        if source not in self.graph.nodes or target not in self.graph.nodes:
            return []

        # Modified BFS to find all shortest paths
        paths = []
        min_length = float('inf')

        queue = [(source, [source], 0.0, [])]
        visited_at_depth = defaultdict(set)
        visited_at_depth[0].add(source)

        while queue and len(paths) < max_paths:
            current, path, weight, edges = queue.pop(0)

            if len(path) > min_length:
                break

            for neighbor, edge_weight, edge_type in self.graph.get_neighbors(current):
                new_path = path + [neighbor]

                if neighbor == target:
                    if len(new_path) <= min_length:
                        min_length = len(new_path)
                        paths.append(PathResult(
                            path=new_path,
                            length=len(path),
                            total_weight=weight + edge_weight,
                            edge_types=edges + [edge_type]
                        ))
                elif neighbor not in visited_at_depth[len(new_path) - 1]:
                    visited_at_depth[len(new_path) - 1].add(neighbor)
                    queue.append((
                        neighbor,
                        new_path,
                        weight + edge_weight,
                        edges + [edge_type]
                    ))

        return paths[:max_paths]

    def find_paths_through(self, via_node: str, source: str = None,
                          target: str = None) -> List[PathResult]:
        """Find paths that pass through a specific node."""
        paths = []

        if source and target:
            # Find path source -> via -> target
            path1 = self.shortest_path(source, via_node)
            path2 = self.shortest_path(via_node, target)

            if path1 and path2:
                combined_path = path1.path + path2.path[1:]  # Avoid duplicating via_node
                paths.append(PathResult(
                    path=combined_path,
                    length=path1.length + path2.length,
                    total_weight=path1.total_weight + path2.total_weight,
                    edge_types=path1.edge_types + path2.edge_types
                ))
        else:
            # Find all paths through via_node
            for s in list(self.graph.nodes.keys())[:20]:  # Limit for performance
                for t in list(self.graph.nodes.keys())[:20]:
                    if s != via_node and t != via_node and s != t:
                        path1 = self.shortest_path(s, via_node)
                        path2 = self.shortest_path(via_node, t)

                        if path1 and path2:
                            combined_path = path1.path + path2.path[1:]
                            paths.append(PathResult(
                                path=combined_path,
                                length=path1.length + path2.length,
                                total_weight=path1.total_weight + path2.total_weight,
                                edge_types=path1.edge_types + path2.edge_types
                            ))

        return paths[:100]  # Limit results

    def diameter(self) -> int:
        """Compute graph diameter (longest shortest path)."""
        max_dist = 0
        nodes = list(self.graph.nodes.keys())

        for node in nodes[:50]:  # Limit for performance
            distances = self._bfs_distances(node)
            max_from_node = max((d for d in distances.values() if d >= 0), default=0)
            max_dist = max(max_dist, max_from_node)

        return max_dist

    def _bfs_distances(self, start: str) -> Dict[str, int]:
        """BFS to compute distances."""
        distances = {nid: -1 for nid in self.graph.nodes}
        distances[start] = 0

        queue = [start]
        while queue:
            current = queue.pop(0)
            for neighbor, weight, etype in self.graph.get_neighbors(current):
                if neighbor in distances and distances[neighbor] < 0:
                    distances[neighbor] = distances[current] + 1
                    queue.append(neighbor)

        return distances


class SubgraphMiner:
    """
    Mines frequent subgraph patterns.

    Identifies common structural patterns in the hypergraph.
    """

    def __init__(self, graph: Graph):
        self.graph = graph

    def find_triangles(self) -> List[Tuple[str, str, str]]:
        """Find all triangles in the graph."""
        triangles = []
        nodes = list(self.graph.nodes.keys())

        for i, u in enumerate(nodes):
            neighbors_u = {n for n, w, e in self.graph.get_neighbors(u)}

            for v in neighbors_u:
                if v <= u:
                    continue
                neighbors_v = {n for n, w, e in self.graph.get_neighbors(v)}

                common = neighbors_u & neighbors_v
                for w in common:
                    if w > v:
                        triangles.append((u, v, w))

        return triangles

    def find_cliques(self, min_size: int = 3, max_size: int = 5) -> List[Set[str]]:
        """Find cliques (fully connected subgraphs)."""
        cliques = []
        nodes = list(self.graph.nodes.keys())

        # Use Bron-Kerbosch algorithm (simplified)
        def bron_kerbosch(r: Set[str], p: Set[str], x: Set[str]):
            if not p and not x:
                if len(r) >= min_size:
                    cliques.append(r.copy())
                return

            if len(r) >= max_size:
                cliques.append(r.copy())
                return

            for v in list(p):
                neighbors = {n for n, w, e in self.graph.get_neighbors(v)}
                bron_kerbosch(
                    r | {v},
                    p & neighbors,
                    x & neighbors
                )
                p.remove(v)
                x.add(v)

        bron_kerbosch(set(), set(nodes), set())
        return cliques

    def find_stars(self, min_degree: int = 5) -> List[Tuple[str, List[str]]]:
        """Find star patterns (high-degree nodes with their neighbors)."""
        stars = []

        for node_id, node in self.graph.nodes.items():
            neighbors = [n for n, w, e in self.graph.get_neighbors(node_id)]
            if len(neighbors) >= min_degree:
                stars.append((node_id, neighbors))

        return sorted(stars, key=lambda x: len(x[1]), reverse=True)

    def compute_clustering_coefficient(self, node_id: str = None) -> float:
        """
        Compute clustering coefficient.

        CC(v) = 2 * triangles(v) / (degree(v) * (degree(v) - 1))
        """
        if node_id:
            return self._node_clustering(node_id)

        # Global average
        total = 0.0
        count = 0

        for nid in self.graph.nodes:
            cc = self._node_clustering(nid)
            if cc >= 0:
                total += cc
                count += 1

        return total / count if count > 0 else 0.0

    def _node_clustering(self, node_id: str) -> float:
        """Compute clustering coefficient for a single node."""
        neighbors = [n for n, w, e in self.graph.get_neighbors(node_id)]
        k = len(neighbors)

        if k < 2:
            return 0.0

        # Count edges between neighbors
        neighbor_edges = 0
        neighbor_set = set(neighbors)

        for neighbor in neighbors:
            for n2, w, e in self.graph.get_neighbors(neighbor):
                if n2 in neighbor_set and n2 != neighbor:
                    neighbor_edges += 1

        if not self.graph.directed:
            neighbor_edges //= 2

        max_edges = k * (k - 1) / 2
        return neighbor_edges / max_edges if max_edges > 0 else 0.0


class GraphAnalytics:
    """
    Main graph analytics engine combining all analysis capabilities.
    """

    def __init__(self, graph: Graph = None):
        self.graph = graph or Graph()
        self.centrality_analyzer = None
        self.community_detector = None
        self.path_analyzer = None
        self.subgraph_miner = None

        if graph:
            self._initialize_analyzers()

    def _initialize_analyzers(self):
        """Initialize all analyzer components."""
        self.centrality_analyzer = CentralityAnalyzer(self.graph)
        self.community_detector = CommunityDetector(self.graph)
        self.path_analyzer = PathAnalyzer(self.graph)
        self.subgraph_miner = SubgraphMiner(self.graph)

    def load_from_hypergraph(self, hypergraph_data: Dict[str, Any]):
        """Load graph from hypergraph JSON data."""
        self.graph = Graph(directed=False)

        # Load nodes
        if "nodes" in hypergraph_data:
            for category, nodes in hypergraph_data["nodes"].items():
                node_type = self._map_category_to_type(category)
                for node_id in nodes:
                    self.graph.add_node(Node(
                        id=node_id,
                        node_type=node_type,
                        attributes={"category": category}
                    ))

        # Load edges
        if "edges" in hypergraph_data:
            for category, edges in hypergraph_data["edges"].items():
                for edge_data in edges:
                    if isinstance(edge_data, dict):
                        self.graph.add_edge(Edge(
                            source=edge_data.get("source", ""),
                            target=edge_data.get("target", ""),
                            edge_type=category,
                            weight=edge_data.get("weight", 1.0)
                        ))

        self._initialize_analyzers()
        logger.info(f"Loaded graph: {self.graph.node_count()} nodes, {self.graph.edge_count()} edges")

    def _map_category_to_type(self, category: str) -> NodeType:
        """Map hypergraph category to node type."""
        category_lower = category.lower()
        if "language" in category_lower:
            return NodeType.LANGUAGE
        elif "task" in category_lower:
            return NodeType.TASK
        elif "paradigm" in category_lower:
            return NodeType.PARADIGM
        elif "domain" in category_lower:
            return NodeType.DOMAIN
        elif "subcategory" in category_lower or "subcat" in category_lower:
            return NodeType.SUBCATEGORY
        return NodeType.LANGUAGE

    def run_full_analysis(self) -> Dict[str, Any]:
        """Run complete graph analysis."""
        if not self.graph or self.graph.node_count() == 0:
            return {"error": "No graph loaded"}

        results = {
            "summary": {
                "node_count": self.graph.node_count(),
                "edge_count": self.graph.edge_count(),
                "density": self._compute_density(),
                "diameter": self.path_analyzer.diameter() if self.path_analyzer else 0,
                "clustering_coefficient": self.subgraph_miner.compute_clustering_coefficient() if self.subgraph_miner else 0
            },
            "centrality": {},
            "communities": [],
            "patterns": {}
        }

        # Centrality analysis
        if self.centrality_analyzer:
            results["centrality"] = {
                "top_degree": self.centrality_analyzer.get_top_nodes(CentralityType.DEGREE, 10),
                "top_betweenness": self.centrality_analyzer.get_top_nodes(CentralityType.BETWEENNESS, 10),
                "top_pagerank": self.centrality_analyzer.get_top_nodes(CentralityType.PAGERANK, 10)
            }

        # Community detection
        if self.community_detector:
            communities = self.community_detector.louvain_communities()
            results["communities"] = [c.to_dict() for c in communities[:20]]
            results["summary"]["community_count"] = len(communities)
            results["summary"]["modularity"] = self.community_detector.compute_modularity(communities)

        # Pattern mining
        if self.subgraph_miner:
            triangles = self.subgraph_miner.find_triangles()
            stars = self.subgraph_miner.find_stars(min_degree=3)
            results["patterns"] = {
                "triangle_count": len(triangles),
                "star_count": len(stars),
                "top_stars": [(s[0], len(s[1])) for s in stars[:10]]
            }

        return results

    def _compute_density(self) -> float:
        """Compute graph density."""
        n = self.graph.node_count()
        e = self.graph.edge_count()

        if n < 2:
            return 0.0

        max_edges = n * (n - 1)
        if not self.graph.directed:
            max_edges //= 2

        return e / max_edges if max_edges > 0 else 0.0

    def export_analysis(self, path: str):
        """Export analysis results to JSON."""
        results = self.run_full_analysis()

        with open(path, 'w') as f:
            json.dump(results, f, indent=2, default=str)

        logger.info(f"Exported analysis to {path}")

    def export_graphml(self, path: str):
        """Export graph to GraphML format for visualization tools."""
        lines = ['<?xml version="1.0" encoding="UTF-8"?>']
        lines.append('<graphml xmlns="http://graphml.graphdrawing.org/xmlns">')
        lines.append('  <key id="type" for="node" attr.name="type" attr.type="string"/>')
        lines.append('  <key id="weight" for="edge" attr.name="weight" attr.type="double"/>')
        lines.append('  <graph id="G" edgedefault="undirected">')

        for node_id, node in self.graph.nodes.items():
            lines.append(f'    <node id="{node_id}">')
            lines.append(f'      <data key="type">{node.node_type.value}</data>')
            lines.append('    </node>')

        for i, edge in enumerate(self.graph.edges):
            lines.append(f'    <edge id="e{i}" source="{edge.source}" target="{edge.target}">')
            lines.append(f'      <data key="weight">{edge.weight}</data>')
            lines.append('    </edge>')

        lines.append('  </graph>')
        lines.append('</graphml>')

        with open(path, 'w') as f:
            f.write('\n'.join(lines))

        logger.info(f"Exported GraphML to {path}")
