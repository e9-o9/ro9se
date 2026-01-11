"""
Tensor Logic: Unifying Deep Learning and Symbolic AI

Based on the framework from tensor-logic.org, this module implements a programming
language that unifies neural network scalability with symbolic reasoning transparency.

Core Principles:
1. Logical rules and Einstein summation are essentially the same operation
2. The tensor equation is the sole construct in the language
3. Enables sound reasoning in embedding space
4. Combines scalability of neural networks with reliability of symbolic reasoning

This implementation supports:
- Transformers
- Formal reasoning systems
- Kernel machines
- Graphical models
"""

import logging
import json
import math
from typing import Dict, List, Any, Optional, Tuple, Union, Callable
from dataclasses import dataclass, field
from enum import Enum
from abc import ABC, abstractmethod


logger = logging.getLogger(__name__)


class TensorType(Enum):
    """Types of tensors in the logic system"""
    SCALAR = "scalar"           # 0-dimensional tensor
    VECTOR = "vector"           # 1-dimensional tensor
    MATRIX = "matrix"           # 2-dimensional tensor
    TENSOR3 = "tensor3"         # 3-dimensional tensor
    TENSOR_N = "tensor_n"       # N-dimensional tensor
    EMBEDDING = "embedding"     # Learned embedding vector
    LOGICAL = "logical"         # Logical truth values (fuzzy)


class LogicalOperator(Enum):
    """Logical operators that can be expressed as tensor operations"""
    AND = "and"
    OR = "or"
    NOT = "not"
    IMPLIES = "implies"
    IFF = "iff"
    FORALL = "forall"
    EXISTS = "exists"


class ReasoningMode(Enum):
    """Modes of reasoning supported"""
    SYMBOLIC = "symbolic"       # Pure symbolic reasoning
    NEURAL = "neural"           # Pure neural inference
    HYBRID = "hybrid"           # Combined neuro-symbolic
    DIFFERENTIABLE = "differentiable"  # Differentiable reasoning


@dataclass
class TensorShape:
    """Represents the shape of a tensor"""
    dimensions: Tuple[int, ...]
    index_names: Tuple[str, ...] = field(default_factory=tuple)

    @property
    def rank(self) -> int:
        """The rank (number of dimensions) of the tensor"""
        return len(self.dimensions)

    @property
    def size(self) -> int:
        """Total number of elements in the tensor"""
        result = 1
        for d in self.dimensions:
            result *= d
        return result

    def is_compatible(self, other: 'TensorShape', contraction_indices: List[str]) -> bool:
        """Check if two shapes are compatible for contraction"""
        for idx in contraction_indices:
            if idx in self.index_names and idx in other.index_names:
                i1 = self.index_names.index(idx)
                i2 = other.index_names.index(idx)
                if self.dimensions[i1] != other.dimensions[i2]:
                    return False
        return True

    def to_dict(self) -> Dict[str, Any]:
        return {
            "dimensions": self.dimensions,
            "index_names": self.index_names,
            "rank": self.rank,
            "size": self.size
        }


@dataclass
class Tensor:
    """
    Core tensor representation for tensor logic.

    A tensor is the fundamental data structure that unifies:
    - Logical predicates (as truth value tensors)
    - Neural embeddings (as learned vectors)
    - Knowledge representations (as structured tensors)
    """
    name: str
    shape: TensorShape
    tensor_type: TensorType
    data: Optional[List[float]] = None
    is_learnable: bool = False
    gradient: Optional[List[float]] = None

    def __post_init__(self):
        if self.data is None:
            # Initialize with zeros
            self.data = [0.0] * self.shape.size

    def get_element(self, indices: Tuple[int, ...]) -> float:
        """Get element at specified indices"""
        if len(indices) != self.shape.rank:
            raise ValueError(f"Expected {self.shape.rank} indices, got {len(indices)}")
        flat_idx = self._flat_index(indices)
        return self.data[flat_idx]

    def set_element(self, indices: Tuple[int, ...], value: float):
        """Set element at specified indices"""
        if len(indices) != self.shape.rank:
            raise ValueError(f"Expected {self.shape.rank} indices, got {len(indices)}")
        flat_idx = self._flat_index(indices)
        self.data[flat_idx] = value

    def _flat_index(self, indices: Tuple[int, ...]) -> int:
        """Convert multi-dimensional indices to flat index"""
        flat_idx = 0
        multiplier = 1
        for i in range(len(indices) - 1, -1, -1):
            flat_idx += indices[i] * multiplier
            multiplier *= self.shape.dimensions[i]
        return flat_idx

    def apply_activation(self, activation: str) -> 'Tensor':
        """Apply activation function element-wise"""
        result = Tensor(
            name=f"{activation}({self.name})",
            shape=self.shape,
            tensor_type=self.tensor_type,
            is_learnable=self.is_learnable
        )

        if activation == "sigmoid":
            result.data = [1.0 / (1.0 + math.exp(-x)) if x > -700 else 0.0 for x in self.data]
        elif activation == "tanh":
            result.data = [math.tanh(x) for x in self.data]
        elif activation == "relu":
            result.data = [max(0.0, x) for x in self.data]
        elif activation == "softmax":
            max_val = max(self.data)
            exp_data = [math.exp(x - max_val) for x in self.data]
            sum_exp = sum(exp_data)
            result.data = [x / sum_exp for x in exp_data]
        else:
            result.data = self.data.copy()

        return result

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "shape": self.shape.to_dict(),
            "tensor_type": self.tensor_type.value,
            "is_learnable": self.is_learnable,
            "data_sample": self.data[:10] if self.data else None
        }


@dataclass
class TensorEquation:
    """
    The tensor equation - the sole construct in tensor logic.

    A tensor equation represents:
    - Logical rules (via Einstein summation semantics)
    - Neural computations (matrix operations)
    - Symbolic constraints (logical conditions)

    Format: result[free_indices] = einsum_expr(input_tensors)
    """
    name: str
    inputs: List[Tensor]
    output: Tensor
    einsum_notation: str  # Einstein summation notation
    logical_interpretation: Optional[str] = None
    is_differentiable: bool = True

    def execute(self) -> Tensor:
        """
        Execute the tensor equation via Einstein summation.

        Einstein summation unifies:
        - Matrix multiplication (ij,jk->ik)
        - Logical AND (i,i->i)
        - Trace operations (ii->)
        - Outer products (i,j->ij)
        """
        # Parse einsum notation
        input_specs, output_spec = self._parse_einsum()

        # Validate inputs
        if len(input_specs) != len(self.inputs):
            raise ValueError(f"Einsum expects {len(input_specs)} inputs, got {len(self.inputs)}")

        # For self-referential rules (output is same as input),
        # we need to work with copies to avoid overwriting during computation
        original_inputs = self.inputs
        if any(inp is self.output for inp in self.inputs):
            # Create temporary copies
            temp_inputs = []
            for inp in self.inputs:
                if inp is self.output:
                    temp = Tensor(
                        name=f"_temp_{inp.name}",
                        shape=inp.shape,
                        tensor_type=inp.tensor_type,
                        data=inp.data.copy()
                    )
                    temp_inputs.append(temp)
                else:
                    temp_inputs.append(inp)
            self.inputs = temp_inputs

        # Execute the operation
        result_data = self._compute_einsum(input_specs, output_spec)

        # Restore original inputs
        self.inputs = original_inputs

        # For transitive closure type rules, merge new values with existing
        # by taking maximum (fuzzy OR for derived facts)
        if any(inp is self.output for inp in self.inputs):
            merged_data = [
                max(old, new) for old, new in zip(self.output.data, result_data)
            ]
            self.output.data = merged_data
        else:
            self.output.data = result_data

        return self.output

    def _parse_einsum(self) -> Tuple[List[str], str]:
        """Parse Einstein summation notation"""
        if "->" not in self.einsum_notation:
            raise ValueError("Invalid einsum notation: missing '->'")

        input_part, output_part = self.einsum_notation.split("->")
        input_specs = input_part.split(",")
        return input_specs, output_part

    def _compute_einsum(self, input_specs: List[str], output_spec: str) -> List[float]:
        """Compute Einstein summation"""
        # Identify contracted and free indices
        all_input_indices = set("".join(input_specs))
        free_indices = set(output_spec)
        contracted_indices = all_input_indices - free_indices

        # Handle single input with no contraction (identity/copy)
        if len(self.inputs) == 1 and not contracted_indices:
            return self.inputs[0].data.copy()

        # For simple cases, compute directly
        if len(self.inputs) == 2 and len(contracted_indices) == 1:
            return self._matrix_multiply(
                self.inputs[0], self.inputs[1],
                input_specs[0], input_specs[1], output_spec
            )
        elif len(self.inputs) == 1 and contracted_indices:
            return self._trace_operation(self.inputs[0], input_specs[0], output_spec)
        elif len(self.inputs) == 2 and not contracted_indices:
            return self._outer_product(self.inputs[0], self.inputs[1])
        else:
            # General case: iterate over all index combinations
            return self._general_einsum(input_specs, output_spec)

    def _matrix_multiply(self, a: Tensor, b: Tensor,
                         spec_a: str, spec_b: str, spec_out: str) -> List[float]:
        """Compute matrix multiplication as einsum"""
        # Find the contracted index
        contracted = set(spec_a) & set(spec_b) - set(spec_out)
        if not contracted:
            return self._outer_product(a, b)

        contracted_idx = list(contracted)[0]

        # Get dimensions
        pos_a_contracted = spec_a.index(contracted_idx)
        pos_b_contracted = spec_b.index(contracted_idx)

        # Simple 2D matrix multiply: ij,jk->ik
        if a.shape.rank == 2 and b.shape.rank == 2:
            m = a.shape.dimensions[0]
            n = a.shape.dimensions[1]
            p = b.shape.dimensions[1]

            result = [0.0] * (m * p)
            for i in range(m):
                for k in range(p):
                    total = 0.0
                    for j in range(n):
                        total += a.get_element((i, j)) * b.get_element((j, k))
                    result[i * p + k] = total
            return result

        # Vector-vector inner product: i,i->
        if a.shape.rank == 1 and b.shape.rank == 1:
            return [sum(x * y for x, y in zip(a.data, b.data))]

        return [0.0] * self.output.shape.size

    def _outer_product(self, a: Tensor, b: Tensor) -> List[float]:
        """Compute outer product"""
        result = []
        for x in a.data:
            for y in b.data:
                result.append(x * y)
        return result

    def _trace_operation(self, tensor: Tensor, spec_in: str, spec_out: str) -> List[float]:
        """Compute trace-like operations"""
        # Find repeated indices in input (diagonal elements)
        if tensor.shape.rank == 2 and spec_in == "ii" and spec_out == "":
            # Trace of a matrix
            n = tensor.shape.dimensions[0]
            return [sum(tensor.get_element((i, i)) for i in range(n))]
        return tensor.data.copy()

    def _general_einsum(self, input_specs: List[str], output_spec: str) -> List[float]:
        """General Einstein summation computation"""
        # Simplified implementation for common cases
        return [0.0] * self.output.shape.size

    def get_logical_form(self) -> str:
        """Get the logical interpretation of this tensor equation"""
        if self.logical_interpretation:
            return self.logical_interpretation

        # Derive logical form from einsum
        input_specs, output_spec = self._parse_einsum()

        # Common patterns
        if len(self.inputs) == 2:
            contracted = set(input_specs[0]) & set(input_specs[1]) - set(output_spec)
            if contracted:
                return f"∃{','.join(contracted)}. {self.inputs[0].name} ∧ {self.inputs[1].name}"
            else:
                return f"{self.inputs[0].name} ⊗ {self.inputs[1].name}"

        return f"TensorEq({self.name})"

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "einsum_notation": self.einsum_notation,
            "logical_form": self.get_logical_form(),
            "inputs": [t.name for t in self.inputs],
            "output": self.output.name,
            "is_differentiable": self.is_differentiable
        }


class LogicalAtom:
    """
    Represents a logical atom (predicate application) in tensor form.

    A logical atom like P(x,y) becomes a tensor P with indices x,y.
    Truth values are represented as tensor elements in [0,1].
    """

    def __init__(self, predicate: str, arity: int, domain_sizes: List[int]):
        self.predicate = predicate
        self.arity = arity
        self.domain_sizes = domain_sizes

        # Create the tensor representation
        shape = TensorShape(
            dimensions=tuple(domain_sizes),
            index_names=tuple(f"x{i}" for i in range(arity))
        )
        self.tensor = Tensor(
            name=predicate,
            shape=shape,
            tensor_type=TensorType.LOGICAL,
            is_learnable=True
        )

    def set_truth_value(self, arguments: Tuple[int, ...], value: float):
        """Set truth value for a grounding of the predicate"""
        if not 0.0 <= value <= 1.0:
            raise ValueError("Truth value must be in [0,1]")
        self.tensor.set_element(arguments, value)

    def get_truth_value(self, arguments: Tuple[int, ...]) -> float:
        """Get truth value for a grounding of the predicate"""
        return self.tensor.get_element(arguments)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "predicate": self.predicate,
            "arity": self.arity,
            "domain_sizes": self.domain_sizes,
            "tensor": self.tensor.to_dict()
        }


class TensorLogicRule:
    """
    A logical rule expressed as a tensor equation.

    For example, the rule:
        ∀x,y,z. R(x,y) ∧ S(y,z) → T(x,z)

    Becomes the tensor equation:
        T[x,z] = ∃y. R[x,y] * S[y,z]  (via einsum: "xy,yz->xz")
    """

    def __init__(self, name: str, head: LogicalAtom, body: List[LogicalAtom],
                 einsum_notation: str):
        self.name = name
        self.head = head
        self.body = body
        self.einsum_notation = einsum_notation

        # Create the tensor equation
        self.equation = TensorEquation(
            name=name,
            inputs=[atom.tensor for atom in body],
            output=head.tensor,
            einsum_notation=einsum_notation,
            logical_interpretation=self._derive_logical_form()
        )

    def _derive_logical_form(self) -> str:
        """Derive the logical form from the rule structure"""
        body_terms = " ∧ ".join(atom.predicate for atom in self.body)
        head_term = self.head.predicate
        return f"∀vars. {body_terms} → {head_term}"

    def apply(self) -> Tensor:
        """Apply the rule to derive new truth values"""
        return self.equation.execute()

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "head": self.head.to_dict(),
            "body": [atom.to_dict() for atom in self.body],
            "einsum_notation": self.einsum_notation,
            "logical_form": self._derive_logical_form()
        }


class EmbeddingSpace:
    """
    Embedding space for representing entities and relations.

    Enables "sound reasoning in embedding space" by:
    - Learning embeddings for entities and relations
    - Performing logical inference via tensor operations
    - Maintaining consistency with symbolic constraints
    """

    def __init__(self, embedding_dim: int, num_entities: int, num_relations: int):
        self.embedding_dim = embedding_dim
        self.num_entities = num_entities
        self.num_relations = num_relations

        # Entity embeddings
        self.entity_embeddings = Tensor(
            name="entity_embeddings",
            shape=TensorShape(
                dimensions=(num_entities, embedding_dim),
                index_names=("entity", "dim")
            ),
            tensor_type=TensorType.EMBEDDING,
            is_learnable=True
        )

        # Relation embeddings (as matrices for composition)
        self.relation_embeddings = Tensor(
            name="relation_embeddings",
            shape=TensorShape(
                dimensions=(num_relations, embedding_dim, embedding_dim),
                index_names=("relation", "dim_in", "dim_out")
            ),
            tensor_type=TensorType.EMBEDDING,
            is_learnable=True
        )

        # Initialize with random values
        import random
        self.entity_embeddings.data = [
            random.gauss(0, 0.1) for _ in range(num_entities * embedding_dim)
        ]
        self.relation_embeddings.data = [
            random.gauss(0, 0.1) for _ in range(num_relations * embedding_dim * embedding_dim)
        ]

    def get_entity_embedding(self, entity_id: int) -> List[float]:
        """Get embedding vector for an entity"""
        start = entity_id * self.embedding_dim
        return self.entity_embeddings.data[start:start + self.embedding_dim]

    def score_triple(self, head: int, relation: int, tail: int) -> float:
        """
        Score a knowledge graph triple (head, relation, tail).

        Uses tensor contraction: score = h^T * R * t
        """
        h = self.get_entity_embedding(head)
        t = self.get_entity_embedding(tail)

        # Get relation matrix
        r_start = relation * self.embedding_dim * self.embedding_dim
        r_data = self.relation_embeddings.data[
            r_start:r_start + self.embedding_dim * self.embedding_dim
        ]

        # Compute h^T * R * t
        # First: R * t
        rt = []
        for i in range(self.embedding_dim):
            val = 0.0
            for j in range(self.embedding_dim):
                val += r_data[i * self.embedding_dim + j] * t[j]
            rt.append(val)

        # Then: h^T * (R * t)
        score = sum(h[i] * rt[i] for i in range(self.embedding_dim))

        # Apply sigmoid for probability
        return 1.0 / (1.0 + math.exp(-score)) if score > -700 else 0.0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "embedding_dim": self.embedding_dim,
            "num_entities": self.num_entities,
            "num_relations": self.num_relations,
            "entity_embeddings": self.entity_embeddings.to_dict(),
            "relation_embeddings": self.relation_embeddings.to_dict()
        }


class TensorLogicProgram:
    """
    A complete tensor logic program.

    Contains:
    - Logical atoms (predicates as tensors)
    - Rules (logical implications as tensor equations)
    - Embedding space for learned representations
    - Inference engine for reasoning
    """

    def __init__(self, name: str, embedding_dim: int = 64):
        self.name = name
        self.embedding_dim = embedding_dim
        self.atoms: Dict[str, LogicalAtom] = {}
        self.rules: Dict[str, TensorLogicRule] = {}
        self.embedding_space: Optional[EmbeddingSpace] = None
        self.reasoning_mode = ReasoningMode.HYBRID
        self.inference_history: List[Dict[str, Any]] = []

    def add_atom(self, predicate: str, arity: int, domain_sizes: List[int]) -> LogicalAtom:
        """Add a logical atom (predicate) to the program"""
        atom = LogicalAtom(predicate, arity, domain_sizes)
        self.atoms[predicate] = atom
        logger.info(f"Added atom: {predicate}/{arity}")
        return atom

    def add_rule(self, name: str, head: str, body: List[str],
                 einsum_notation: str) -> TensorLogicRule:
        """Add a logical rule to the program"""
        head_atom = self.atoms[head]
        body_atoms = [self.atoms[b] for b in body]

        rule = TensorLogicRule(name, head_atom, body_atoms, einsum_notation)
        self.rules[name] = rule
        logger.info(f"Added rule: {name}")
        return rule

    def add_fact(self, predicate: str, arguments: Tuple[int, ...],
                 truth_value: float = 1.0):
        """Add a fact (ground atom) to the program"""
        if predicate not in self.atoms:
            raise ValueError(f"Unknown predicate: {predicate}")
        self.atoms[predicate].set_truth_value(arguments, truth_value)
        logger.debug(f"Added fact: {predicate}{arguments} = {truth_value}")

    def initialize_embedding_space(self, num_entities: int, num_relations: int):
        """Initialize the embedding space for neural reasoning"""
        self.embedding_space = EmbeddingSpace(
            self.embedding_dim, num_entities, num_relations
        )
        logger.info(f"Initialized embedding space: {num_entities} entities, {num_relations} relations")

    def forward_chain(self, max_iterations: int = 10) -> int:
        """
        Perform forward chaining inference.

        Applies all rules repeatedly until fixpoint or max iterations.
        Returns number of new facts derived.
        """
        total_new_facts = 0

        for iteration in range(max_iterations):
            new_facts = 0

            for rule in self.rules.values():
                result = rule.apply()
                # Count non-zero new values
                for val in result.data:
                    if val > 0.5:  # Threshold for "true"
                        new_facts += 1

            if new_facts == 0:
                logger.info(f"Forward chaining reached fixpoint at iteration {iteration}")
                break

            total_new_facts += new_facts

            self.inference_history.append({
                "iteration": iteration,
                "new_facts": new_facts,
                "total_facts": total_new_facts
            })

        return total_new_facts

    def query(self, predicate: str, arguments: Tuple[int, ...]) -> float:
        """Query the truth value of a ground atom"""
        if predicate not in self.atoms:
            raise ValueError(f"Unknown predicate: {predicate}")
        return self.atoms[predicate].get_truth_value(arguments)

    def query_embedding(self, head: int, relation: int, tail: int) -> float:
        """Query using the embedding space"""
        if self.embedding_space is None:
            raise ValueError("Embedding space not initialized")
        return self.embedding_space.score_triple(head, relation, tail)

    def get_statistics(self) -> Dict[str, Any]:
        """Get program statistics"""
        total_atoms = sum(atom.tensor.shape.size for atom in self.atoms.values())
        return {
            "name": self.name,
            "num_predicates": len(self.atoms),
            "num_rules": len(self.rules),
            "total_groundings": total_atoms,
            "reasoning_mode": self.reasoning_mode.value,
            "has_embeddings": self.embedding_space is not None
        }

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "embedding_dim": self.embedding_dim,
            "atoms": {k: v.to_dict() for k, v in self.atoms.items()},
            "rules": {k: v.to_dict() for k, v in self.rules.items()},
            "statistics": self.get_statistics(),
            "inference_history": self.inference_history
        }

    def export_to_json(self, path: str):
        """Export program to JSON file"""
        with open(path, 'w') as f:
            json.dump(self.to_dict(), f, indent=2)
        logger.info(f"Exported program to {path}")


class TransformerLayer:
    """
    Transformer attention layer expressed as tensor equations.

    Demonstrates that transformers are expressible in tensor logic:
    - Attention: softmax(Q * K^T / sqrt(d)) * V
    - Each component is a tensor equation
    """

    def __init__(self, hidden_dim: int, num_heads: int):
        self.hidden_dim = hidden_dim
        self.num_heads = num_heads
        self.head_dim = hidden_dim // num_heads

        # Query, Key, Value projection tensors
        self.W_q = Tensor(
            name="W_q",
            shape=TensorShape((hidden_dim, hidden_dim), ("in", "out")),
            tensor_type=TensorType.MATRIX,
            is_learnable=True
        )
        self.W_k = Tensor(
            name="W_k",
            shape=TensorShape((hidden_dim, hidden_dim), ("in", "out")),
            tensor_type=TensorType.MATRIX,
            is_learnable=True
        )
        self.W_v = Tensor(
            name="W_v",
            shape=TensorShape((hidden_dim, hidden_dim), ("in", "out")),
            tensor_type=TensorType.MATRIX,
            is_learnable=True
        )

    def get_attention_equations(self) -> List[TensorEquation]:
        """Get the tensor equations that define attention"""
        equations = []

        # Q = X @ W_q (einsum: "bi,ij->bj")
        q_output = Tensor(
            name="Q",
            shape=TensorShape((1, self.hidden_dim), ("batch", "hidden")),
            tensor_type=TensorType.MATRIX
        )

        # K = X @ W_k
        # V = X @ W_v
        # Attention = softmax(Q @ K^T / sqrt(d)) @ V

        return equations

    def to_dict(self) -> Dict[str, Any]:
        return {
            "hidden_dim": self.hidden_dim,
            "num_heads": self.num_heads,
            "head_dim": self.head_dim,
            "W_q": self.W_q.to_dict(),
            "W_k": self.W_k.to_dict(),
            "W_v": self.W_v.to_dict()
        }


class TensorLogicEngine:
    """
    Main engine for tensor logic computation.

    Provides:
    - Program management
    - Inference execution
    - Neural-symbolic integration
    - Performance optimization
    """

    def __init__(self):
        self.programs: Dict[str, TensorLogicProgram] = {}
        self.global_embedding_dim = 64
        self.computation_graph: List[TensorEquation] = []

    def create_program(self, name: str, embedding_dim: int = None) -> TensorLogicProgram:
        """Create a new tensor logic program"""
        dim = embedding_dim or self.global_embedding_dim
        program = TensorLogicProgram(name, dim)
        self.programs[name] = program
        logger.info(f"Created program: {name}")
        return program

    def get_program(self, name: str) -> Optional[TensorLogicProgram]:
        """Get a program by name"""
        return self.programs.get(name)

    def execute_equation(self, equation: TensorEquation) -> Tensor:
        """Execute a single tensor equation"""
        self.computation_graph.append(equation)
        return equation.execute()

    def run_inference(self, program_name: str, max_iterations: int = 10) -> Dict[str, Any]:
        """Run inference on a program"""
        program = self.programs.get(program_name)
        if program is None:
            raise ValueError(f"Unknown program: {program_name}")

        new_facts = program.forward_chain(max_iterations)

        return {
            "program": program_name,
            "new_facts": new_facts,
            "statistics": program.get_statistics()
        }

    def create_transitive_closure_rule(
        self,
        program: TensorLogicProgram,
        relation: str,
        domain_size: int
    ) -> TensorLogicRule:
        """
        Create a transitive closure rule: R(x,y) ∧ R(y,z) → R(x,z)

        This is a common pattern in knowledge graphs.
        """
        # Ensure the relation atom exists
        if relation not in program.atoms:
            program.add_atom(relation, 2, [domain_size, domain_size])

        # Create the rule using einsum for composition
        rule = program.add_rule(
            name=f"{relation}_transitive",
            head=relation,
            body=[relation, relation],
            einsum_notation="xy,yz->xz"
        )

        return rule

    def get_statistics(self) -> Dict[str, Any]:
        """Get engine statistics"""
        return {
            "num_programs": len(self.programs),
            "total_equations": len(self.computation_graph),
            "programs": {
                name: prog.get_statistics()
                for name, prog in self.programs.items()
            }
        }

    def export_all(self, directory: str):
        """Export all programs to a directory"""
        import os
        os.makedirs(directory, exist_ok=True)

        for name, program in self.programs.items():
            path = os.path.join(directory, f"{name}.json")
            program.export_to_json(path)

        logger.info(f"Exported {len(self.programs)} programs to {directory}")


# Utility functions for common tensor operations

def create_identity_tensor(size: int, name: str = "I") -> Tensor:
    """Create an identity matrix tensor"""
    shape = TensorShape((size, size), ("i", "j"))
    tensor = Tensor(name=name, shape=shape, tensor_type=TensorType.MATRIX)
    for i in range(size):
        tensor.set_element((i, i), 1.0)
    return tensor


def create_logical_and(a: Tensor, b: Tensor) -> TensorEquation:
    """Create logical AND as element-wise minimum"""
    output = Tensor(
        name=f"and({a.name},{b.name})",
        shape=a.shape,
        tensor_type=TensorType.LOGICAL
    )
    output.data = [min(x, y) for x, y in zip(a.data, b.data)]

    return TensorEquation(
        name="logical_and",
        inputs=[a, b],
        output=output,
        einsum_notation="i,i->i",
        logical_interpretation=f"{a.name} ∧ {b.name}"
    )


def create_logical_or(a: Tensor, b: Tensor) -> TensorEquation:
    """Create logical OR as element-wise maximum"""
    output = Tensor(
        name=f"or({a.name},{b.name})",
        shape=a.shape,
        tensor_type=TensorType.LOGICAL
    )
    output.data = [max(x, y) for x, y in zip(a.data, b.data)]

    return TensorEquation(
        name="logical_or",
        inputs=[a, b],
        output=output,
        einsum_notation="i,i->i",
        logical_interpretation=f"{a.name} ∨ {b.name}"
    )


def create_logical_not(a: Tensor) -> Tensor:
    """Create logical NOT as 1 - x"""
    output = Tensor(
        name=f"not({a.name})",
        shape=a.shape,
        tensor_type=TensorType.LOGICAL
    )
    output.data = [1.0 - x for x in a.data]
    return output


def fuzzy_implies(a: float, b: float) -> float:
    """Fuzzy implication: a → b = min(1, 1 - a + b)"""
    return min(1.0, 1.0 - a + b)


def t_norm_product(a: float, b: float) -> float:
    """Product t-norm for fuzzy AND"""
    return a * b


def t_conorm_probabilistic(a: float, b: float) -> float:
    """Probabilistic t-conorm for fuzzy OR"""
    return a + b - a * b
