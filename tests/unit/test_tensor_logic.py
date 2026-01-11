"""
Unit tests for tensor_logic.py

Tests the Tensor Logic framework that unifies deep learning and symbolic AI
based on the tensor-logic.org concepts.
"""

import pytest
import sys
import math
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "opencog" / "lib"))

from tensor_logic import (
    TensorType,
    LogicalOperator,
    ReasoningMode,
    TensorShape,
    Tensor,
    TensorEquation,
    LogicalAtom,
    TensorLogicRule,
    EmbeddingSpace,
    TensorLogicProgram,
    TransformerLayer,
    TensorLogicEngine,
    create_identity_tensor,
    create_logical_and,
    create_logical_or,
    create_logical_not,
    fuzzy_implies,
    t_norm_product,
    t_conorm_probabilistic,
)


class TestTensorType:
    """Tests for TensorType enum."""

    def test_has_scalar(self):
        assert TensorType.SCALAR.value == "scalar"

    def test_has_vector(self):
        assert TensorType.VECTOR.value == "vector"

    def test_has_matrix(self):
        assert TensorType.MATRIX.value == "matrix"

    def test_has_tensor3(self):
        assert TensorType.TENSOR3.value == "tensor3"

    def test_has_tensor_n(self):
        assert TensorType.TENSOR_N.value == "tensor_n"

    def test_has_embedding(self):
        assert TensorType.EMBEDDING.value == "embedding"

    def test_has_logical(self):
        assert TensorType.LOGICAL.value == "logical"

    def test_all_types_count(self):
        assert len(TensorType) == 7


class TestLogicalOperator:
    """Tests for LogicalOperator enum."""

    def test_has_and(self):
        assert LogicalOperator.AND.value == "and"

    def test_has_or(self):
        assert LogicalOperator.OR.value == "or"

    def test_has_not(self):
        assert LogicalOperator.NOT.value == "not"

    def test_has_implies(self):
        assert LogicalOperator.IMPLIES.value == "implies"

    def test_has_iff(self):
        assert LogicalOperator.IFF.value == "iff"

    def test_has_forall(self):
        assert LogicalOperator.FORALL.value == "forall"

    def test_has_exists(self):
        assert LogicalOperator.EXISTS.value == "exists"

    def test_all_operators_count(self):
        assert len(LogicalOperator) == 7


class TestReasoningMode:
    """Tests for ReasoningMode enum."""

    def test_has_symbolic(self):
        assert ReasoningMode.SYMBOLIC.value == "symbolic"

    def test_has_neural(self):
        assert ReasoningMode.NEURAL.value == "neural"

    def test_has_hybrid(self):
        assert ReasoningMode.HYBRID.value == "hybrid"

    def test_has_differentiable(self):
        assert ReasoningMode.DIFFERENTIABLE.value == "differentiable"

    def test_all_modes_count(self):
        assert len(ReasoningMode) == 4


class TestTensorShape:
    """Tests for TensorShape dataclass."""

    def test_initialization(self):
        shape = TensorShape(dimensions=(3, 4))
        assert shape.dimensions == (3, 4)
        assert shape.index_names == ()

    def test_initialization_with_names(self):
        shape = TensorShape(dimensions=(3, 4), index_names=("i", "j"))
        assert shape.index_names == ("i", "j")

    def test_rank_scalar(self):
        shape = TensorShape(dimensions=())
        assert shape.rank == 0

    def test_rank_vector(self):
        shape = TensorShape(dimensions=(5,))
        assert shape.rank == 1

    def test_rank_matrix(self):
        shape = TensorShape(dimensions=(3, 4))
        assert shape.rank == 2

    def test_rank_tensor3(self):
        shape = TensorShape(dimensions=(2, 3, 4))
        assert shape.rank == 3

    def test_size_scalar(self):
        shape = TensorShape(dimensions=())
        assert shape.size == 1

    def test_size_vector(self):
        shape = TensorShape(dimensions=(5,))
        assert shape.size == 5

    def test_size_matrix(self):
        shape = TensorShape(dimensions=(3, 4))
        assert shape.size == 12

    def test_size_tensor3(self):
        shape = TensorShape(dimensions=(2, 3, 4))
        assert shape.size == 24

    def test_is_compatible_matching_indices(self):
        shape1 = TensorShape(dimensions=(3, 4), index_names=("i", "j"))
        shape2 = TensorShape(dimensions=(4, 5), index_names=("j", "k"))
        assert shape1.is_compatible(shape2, ["j"])

    def test_is_compatible_mismatched_sizes(self):
        shape1 = TensorShape(dimensions=(3, 4), index_names=("i", "j"))
        shape2 = TensorShape(dimensions=(5, 6), index_names=("j", "k"))
        assert not shape1.is_compatible(shape2, ["j"])

    def test_to_dict(self):
        shape = TensorShape(dimensions=(3, 4), index_names=("i", "j"))
        d = shape.to_dict()
        assert d["dimensions"] == (3, 4)
        assert d["index_names"] == ("i", "j")
        assert d["rank"] == 2
        assert d["size"] == 12


class TestTensor:
    """Tests for Tensor class."""

    def test_initialization(self):
        shape = TensorShape(dimensions=(2, 3))
        tensor = Tensor(name="test", shape=shape, tensor_type=TensorType.MATRIX)
        assert tensor.name == "test"
        assert tensor.tensor_type == TensorType.MATRIX
        assert len(tensor.data) == 6

    def test_initialization_with_data(self):
        shape = TensorShape(dimensions=(2,))
        tensor = Tensor(
            name="test",
            shape=shape,
            tensor_type=TensorType.VECTOR,
            data=[1.0, 2.0]
        )
        assert tensor.data == [1.0, 2.0]

    def test_default_data_is_zeros(self):
        shape = TensorShape(dimensions=(3,))
        tensor = Tensor(name="test", shape=shape, tensor_type=TensorType.VECTOR)
        assert tensor.data == [0.0, 0.0, 0.0]

    def test_is_learnable_default_false(self):
        shape = TensorShape(dimensions=(2,))
        tensor = Tensor(name="test", shape=shape, tensor_type=TensorType.VECTOR)
        assert tensor.is_learnable is False

    def test_is_learnable_true(self):
        shape = TensorShape(dimensions=(2,))
        tensor = Tensor(
            name="test",
            shape=shape,
            tensor_type=TensorType.EMBEDDING,
            is_learnable=True
        )
        assert tensor.is_learnable is True

    def test_get_element_vector(self):
        shape = TensorShape(dimensions=(3,))
        tensor = Tensor(
            name="test",
            shape=shape,
            tensor_type=TensorType.VECTOR,
            data=[1.0, 2.0, 3.0]
        )
        assert tensor.get_element((0,)) == 1.0
        assert tensor.get_element((1,)) == 2.0
        assert tensor.get_element((2,)) == 3.0

    def test_get_element_matrix(self):
        shape = TensorShape(dimensions=(2, 3))
        tensor = Tensor(
            name="test",
            shape=shape,
            tensor_type=TensorType.MATRIX,
            data=[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
        )
        assert tensor.get_element((0, 0)) == 1.0
        assert tensor.get_element((0, 2)) == 3.0
        assert tensor.get_element((1, 0)) == 4.0
        assert tensor.get_element((1, 2)) == 6.0

    def test_get_element_wrong_rank(self):
        shape = TensorShape(dimensions=(2, 3))
        tensor = Tensor(name="test", shape=shape, tensor_type=TensorType.MATRIX)
        with pytest.raises(ValueError):
            tensor.get_element((0,))

    def test_set_element(self):
        shape = TensorShape(dimensions=(2, 2))
        tensor = Tensor(name="test", shape=shape, tensor_type=TensorType.MATRIX)
        tensor.set_element((0, 0), 5.0)
        tensor.set_element((1, 1), 10.0)
        assert tensor.get_element((0, 0)) == 5.0
        assert tensor.get_element((1, 1)) == 10.0

    def test_set_element_wrong_rank(self):
        shape = TensorShape(dimensions=(2, 2))
        tensor = Tensor(name="test", shape=shape, tensor_type=TensorType.MATRIX)
        with pytest.raises(ValueError):
            tensor.set_element((0,), 5.0)

    def test_apply_activation_sigmoid(self):
        shape = TensorShape(dimensions=(3,))
        tensor = Tensor(
            name="test",
            shape=shape,
            tensor_type=TensorType.VECTOR,
            data=[0.0, 1.0, -1.0]
        )
        result = tensor.apply_activation("sigmoid")
        assert abs(result.data[0] - 0.5) < 0.001
        assert result.data[1] > 0.7
        assert result.data[2] < 0.3

    def test_apply_activation_tanh(self):
        shape = TensorShape(dimensions=(3,))
        tensor = Tensor(
            name="test",
            shape=shape,
            tensor_type=TensorType.VECTOR,
            data=[0.0, 1.0, -1.0]
        )
        result = tensor.apply_activation("tanh")
        assert abs(result.data[0]) < 0.001
        assert result.data[1] > 0.7
        assert result.data[2] < -0.7

    def test_apply_activation_relu(self):
        shape = TensorShape(dimensions=(3,))
        tensor = Tensor(
            name="test",
            shape=shape,
            tensor_type=TensorType.VECTOR,
            data=[-1.0, 0.0, 1.0]
        )
        result = tensor.apply_activation("relu")
        assert result.data[0] == 0.0
        assert result.data[1] == 0.0
        assert result.data[2] == 1.0

    def test_apply_activation_softmax(self):
        shape = TensorShape(dimensions=(3,))
        tensor = Tensor(
            name="test",
            shape=shape,
            tensor_type=TensorType.VECTOR,
            data=[1.0, 2.0, 3.0]
        )
        result = tensor.apply_activation("softmax")
        # Sum should be 1.0
        assert abs(sum(result.data) - 1.0) < 0.001
        # Values should be in increasing order
        assert result.data[0] < result.data[1] < result.data[2]

    def test_to_dict(self):
        shape = TensorShape(dimensions=(2,))
        tensor = Tensor(
            name="test",
            shape=shape,
            tensor_type=TensorType.VECTOR,
            data=[1.0, 2.0],
            is_learnable=True
        )
        d = tensor.to_dict()
        assert d["name"] == "test"
        assert d["tensor_type"] == "vector"
        assert d["is_learnable"] is True


class TestTensorEquation:
    """Tests for TensorEquation class."""

    def test_initialization(self):
        shape_a = TensorShape(dimensions=(2, 3))
        shape_b = TensorShape(dimensions=(3, 4))
        shape_c = TensorShape(dimensions=(2, 4))

        a = Tensor(name="A", shape=shape_a, tensor_type=TensorType.MATRIX)
        b = Tensor(name="B", shape=shape_b, tensor_type=TensorType.MATRIX)
        c = Tensor(name="C", shape=shape_c, tensor_type=TensorType.MATRIX)

        eq = TensorEquation(
            name="matmul",
            inputs=[a, b],
            output=c,
            einsum_notation="ij,jk->ik"
        )

        assert eq.name == "matmul"
        assert eq.einsum_notation == "ij,jk->ik"
        assert len(eq.inputs) == 2

    def test_execute_matrix_multiply(self):
        # 2x2 identity times 2x2 matrix
        shape = TensorShape(dimensions=(2, 2))

        a = Tensor(
            name="A",
            shape=shape,
            tensor_type=TensorType.MATRIX,
            data=[1.0, 0.0, 0.0, 1.0]  # Identity
        )
        b = Tensor(
            name="B",
            shape=shape,
            tensor_type=TensorType.MATRIX,
            data=[1.0, 2.0, 3.0, 4.0]
        )
        c = Tensor(name="C", shape=shape, tensor_type=TensorType.MATRIX)

        eq = TensorEquation(
            name="matmul",
            inputs=[a, b],
            output=c,
            einsum_notation="ij,jk->ik"
        )

        result = eq.execute()
        # Identity times any matrix = that matrix
        assert result.data == [1.0, 2.0, 3.0, 4.0]

    def test_execute_vector_inner_product(self):
        shape = TensorShape(dimensions=(3,))

        a = Tensor(
            name="A",
            shape=shape,
            tensor_type=TensorType.VECTOR,
            data=[1.0, 2.0, 3.0]
        )
        b = Tensor(
            name="B",
            shape=shape,
            tensor_type=TensorType.VECTOR,
            data=[4.0, 5.0, 6.0]
        )

        output_shape = TensorShape(dimensions=(1,))
        c = Tensor(name="C", shape=output_shape, tensor_type=TensorType.SCALAR)

        eq = TensorEquation(
            name="inner",
            inputs=[a, b],
            output=c,
            einsum_notation="i,i->"
        )

        result = eq.execute()
        # 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
        assert abs(result.data[0] - 32.0) < 0.001

    def test_get_logical_form_existential(self):
        shape = TensorShape(dimensions=(2, 2))
        a = Tensor(name="R", shape=shape, tensor_type=TensorType.LOGICAL)
        b = Tensor(name="S", shape=shape, tensor_type=TensorType.LOGICAL)
        c = Tensor(name="T", shape=shape, tensor_type=TensorType.LOGICAL)

        eq = TensorEquation(
            name="compose",
            inputs=[a, b],
            output=c,
            einsum_notation="xy,yz->xz"
        )

        logical_form = eq.get_logical_form()
        assert "∃" in logical_form
        assert "R" in logical_form
        assert "S" in logical_form

    def test_to_dict(self):
        shape = TensorShape(dimensions=(2, 2))
        a = Tensor(name="A", shape=shape, tensor_type=TensorType.MATRIX)
        b = Tensor(name="B", shape=shape, tensor_type=TensorType.MATRIX)

        eq = TensorEquation(
            name="test",
            inputs=[a],
            output=b,
            einsum_notation="ij->ij"
        )

        d = eq.to_dict()
        assert d["name"] == "test"
        assert d["einsum_notation"] == "ij->ij"
        assert "logical_form" in d


class TestLogicalAtom:
    """Tests for LogicalAtom class."""

    def test_initialization(self):
        atom = LogicalAtom(predicate="likes", arity=2, domain_sizes=[10, 10])
        assert atom.predicate == "likes"
        assert atom.arity == 2
        assert atom.domain_sizes == [10, 10]

    def test_tensor_creation(self):
        atom = LogicalAtom(predicate="P", arity=2, domain_sizes=[3, 4])
        assert atom.tensor.shape.dimensions == (3, 4)
        assert atom.tensor.tensor_type == TensorType.LOGICAL

    def test_set_truth_value(self):
        atom = LogicalAtom(predicate="P", arity=2, domain_sizes=[5, 5])
        atom.set_truth_value((1, 2), 0.8)
        assert atom.get_truth_value((1, 2)) == 0.8

    def test_set_truth_value_invalid_range(self):
        atom = LogicalAtom(predicate="P", arity=1, domain_sizes=[5])
        with pytest.raises(ValueError):
            atom.set_truth_value((0,), 1.5)

    def test_get_truth_value_default(self):
        atom = LogicalAtom(predicate="P", arity=1, domain_sizes=[5])
        assert atom.get_truth_value((0,)) == 0.0

    def test_to_dict(self):
        atom = LogicalAtom(predicate="test", arity=2, domain_sizes=[3, 3])
        d = atom.to_dict()
        assert d["predicate"] == "test"
        assert d["arity"] == 2
        assert d["domain_sizes"] == [3, 3]


class TestTensorLogicRule:
    """Tests for TensorLogicRule class."""

    def test_initialization(self):
        head = LogicalAtom(predicate="T", arity=2, domain_sizes=[5, 5])
        body1 = LogicalAtom(predicate="R", arity=2, domain_sizes=[5, 5])
        body2 = LogicalAtom(predicate="S", arity=2, domain_sizes=[5, 5])

        rule = TensorLogicRule(
            name="transitive",
            head=head,
            body=[body1, body2],
            einsum_notation="xy,yz->xz"
        )

        assert rule.name == "transitive"
        assert rule.head == head
        assert len(rule.body) == 2

    def test_derive_logical_form(self):
        head = LogicalAtom(predicate="T", arity=2, domain_sizes=[5, 5])
        body1 = LogicalAtom(predicate="R", arity=2, domain_sizes=[5, 5])
        body2 = LogicalAtom(predicate="S", arity=2, domain_sizes=[5, 5])

        rule = TensorLogicRule(
            name="test",
            head=head,
            body=[body1, body2],
            einsum_notation="xy,yz->xz"
        )

        logical_form = rule._derive_logical_form()
        assert "→" in logical_form
        assert "R" in logical_form
        assert "S" in logical_form
        assert "T" in logical_form

    def test_apply(self):
        head = LogicalAtom(predicate="T", arity=2, domain_sizes=[3, 3])
        body1 = LogicalAtom(predicate="R", arity=2, domain_sizes=[3, 3])
        body2 = LogicalAtom(predicate="S", arity=2, domain_sizes=[3, 3])

        # Set some truth values
        body1.set_truth_value((0, 1), 1.0)
        body2.set_truth_value((1, 2), 1.0)

        rule = TensorLogicRule(
            name="transitive",
            head=head,
            body=[body1, body2],
            einsum_notation="xy,yz->xz"
        )

        result = rule.apply()
        # R(0,1) ∧ S(1,2) should imply T(0,2)
        assert result.get_element((0, 2)) > 0

    def test_to_dict(self):
        head = LogicalAtom(predicate="T", arity=2, domain_sizes=[3, 3])
        body = LogicalAtom(predicate="R", arity=2, domain_sizes=[3, 3])

        rule = TensorLogicRule(
            name="test",
            head=head,
            body=[body],
            einsum_notation="xy->xy"
        )

        d = rule.to_dict()
        assert d["name"] == "test"
        assert "head" in d
        assert "body" in d
        assert "logical_form" in d


class TestEmbeddingSpace:
    """Tests for EmbeddingSpace class."""

    def test_initialization(self):
        space = EmbeddingSpace(embedding_dim=32, num_entities=100, num_relations=10)
        assert space.embedding_dim == 32
        assert space.num_entities == 100
        assert space.num_relations == 10

    def test_entity_embeddings_shape(self):
        space = EmbeddingSpace(embedding_dim=16, num_entities=50, num_relations=5)
        assert space.entity_embeddings.shape.dimensions == (50, 16)

    def test_relation_embeddings_shape(self):
        space = EmbeddingSpace(embedding_dim=16, num_entities=50, num_relations=5)
        assert space.relation_embeddings.shape.dimensions == (5, 16, 16)

    def test_get_entity_embedding(self):
        space = EmbeddingSpace(embedding_dim=8, num_entities=10, num_relations=3)
        emb = space.get_entity_embedding(0)
        assert len(emb) == 8

    def test_score_triple(self):
        space = EmbeddingSpace(embedding_dim=8, num_entities=10, num_relations=3)
        score = space.score_triple(head=0, relation=0, tail=1)
        # Score should be a probability in [0, 1]
        assert 0.0 <= score <= 1.0

    def test_to_dict(self):
        space = EmbeddingSpace(embedding_dim=8, num_entities=10, num_relations=3)
        d = space.to_dict()
        assert d["embedding_dim"] == 8
        assert d["num_entities"] == 10
        assert d["num_relations"] == 3


class TestTensorLogicProgram:
    """Tests for TensorLogicProgram class."""

    def test_initialization(self):
        prog = TensorLogicProgram(name="test_program")
        assert prog.name == "test_program"
        assert prog.embedding_dim == 64
        assert prog.atoms == {}
        assert prog.rules == {}

    def test_initialization_with_embedding_dim(self):
        prog = TensorLogicProgram(name="test", embedding_dim=128)
        assert prog.embedding_dim == 128

    def test_add_atom(self):
        prog = TensorLogicProgram(name="test")
        atom = prog.add_atom("likes", arity=2, domain_sizes=[10, 10])
        assert "likes" in prog.atoms
        assert atom.predicate == "likes"

    def test_add_rule(self):
        prog = TensorLogicProgram(name="test")
        prog.add_atom("R", arity=2, domain_sizes=[5, 5])
        prog.add_atom("T", arity=2, domain_sizes=[5, 5])

        rule = prog.add_rule(
            name="copy",
            head="T",
            body=["R"],
            einsum_notation="xy->xy"
        )

        assert "copy" in prog.rules
        assert rule.name == "copy"

    def test_add_fact(self):
        prog = TensorLogicProgram(name="test")
        prog.add_atom("P", arity=2, domain_sizes=[5, 5])
        prog.add_fact("P", (1, 2), 0.9)
        assert prog.query("P", (1, 2)) == 0.9

    def test_add_fact_unknown_predicate(self):
        prog = TensorLogicProgram(name="test")
        with pytest.raises(ValueError):
            prog.add_fact("unknown", (0,), 1.0)

    def test_initialize_embedding_space(self):
        prog = TensorLogicProgram(name="test")
        prog.initialize_embedding_space(num_entities=100, num_relations=10)
        assert prog.embedding_space is not None
        assert prog.embedding_space.num_entities == 100

    def test_forward_chain(self):
        prog = TensorLogicProgram(name="test")
        prog.add_atom("R", arity=2, domain_sizes=[3, 3])
        prog.add_atom("T", arity=2, domain_sizes=[3, 3])

        # R(0,1) = 1.0
        prog.add_fact("R", (0, 1), 1.0)

        prog.add_rule(
            name="copy",
            head="T",
            body=["R"],
            einsum_notation="xy->xy"
        )

        new_facts = prog.forward_chain(max_iterations=1)
        assert new_facts > 0

    def test_query(self):
        prog = TensorLogicProgram(name="test")
        prog.add_atom("P", arity=1, domain_sizes=[5])
        prog.add_fact("P", (2,), 0.75)
        assert prog.query("P", (2,)) == 0.75

    def test_query_unknown_predicate(self):
        prog = TensorLogicProgram(name="test")
        with pytest.raises(ValueError):
            prog.query("unknown", (0,))

    def test_query_embedding(self):
        prog = TensorLogicProgram(name="test", embedding_dim=16)
        prog.initialize_embedding_space(num_entities=10, num_relations=3)
        score = prog.query_embedding(head=0, relation=0, tail=1)
        assert 0.0 <= score <= 1.0

    def test_query_embedding_not_initialized(self):
        prog = TensorLogicProgram(name="test")
        with pytest.raises(ValueError):
            prog.query_embedding(head=0, relation=0, tail=1)

    def test_get_statistics(self):
        prog = TensorLogicProgram(name="test")
        prog.add_atom("P", arity=2, domain_sizes=[5, 5])
        prog.add_atom("Q", arity=1, domain_sizes=[10])

        stats = prog.get_statistics()
        assert stats["name"] == "test"
        assert stats["num_predicates"] == 2
        assert stats["reasoning_mode"] == "hybrid"

    def test_to_dict(self):
        prog = TensorLogicProgram(name="test")
        prog.add_atom("P", arity=1, domain_sizes=[5])
        d = prog.to_dict()
        assert d["name"] == "test"
        assert "atoms" in d
        assert "P" in d["atoms"]

    def test_export_to_json(self, tmp_path):
        prog = TensorLogicProgram(name="test")
        prog.add_atom("P", arity=1, domain_sizes=[3])

        path = tmp_path / "program.json"
        prog.export_to_json(str(path))

        assert path.exists()

        import json
        with open(path) as f:
            data = json.load(f)
        assert data["name"] == "test"


class TestTransformerLayer:
    """Tests for TransformerLayer class."""

    def test_initialization(self):
        layer = TransformerLayer(hidden_dim=64, num_heads=8)
        assert layer.hidden_dim == 64
        assert layer.num_heads == 8
        assert layer.head_dim == 8

    def test_weight_tensors_created(self):
        layer = TransformerLayer(hidden_dim=32, num_heads=4)
        assert layer.W_q is not None
        assert layer.W_k is not None
        assert layer.W_v is not None

    def test_weight_shapes(self):
        layer = TransformerLayer(hidden_dim=64, num_heads=8)
        assert layer.W_q.shape.dimensions == (64, 64)
        assert layer.W_k.shape.dimensions == (64, 64)
        assert layer.W_v.shape.dimensions == (64, 64)

    def test_to_dict(self):
        layer = TransformerLayer(hidden_dim=32, num_heads=4)
        d = layer.to_dict()
        assert d["hidden_dim"] == 32
        assert d["num_heads"] == 4
        assert "W_q" in d


class TestTensorLogicEngine:
    """Tests for TensorLogicEngine class."""

    def test_initialization(self):
        engine = TensorLogicEngine()
        assert engine.programs == {}
        assert engine.global_embedding_dim == 64

    def test_create_program(self):
        engine = TensorLogicEngine()
        prog = engine.create_program("my_program")
        assert "my_program" in engine.programs
        assert prog.name == "my_program"

    def test_create_program_with_dim(self):
        engine = TensorLogicEngine()
        prog = engine.create_program("test", embedding_dim=128)
        assert prog.embedding_dim == 128

    def test_get_program(self):
        engine = TensorLogicEngine()
        engine.create_program("test")
        prog = engine.get_program("test")
        assert prog is not None
        assert prog.name == "test"

    def test_get_program_nonexistent(self):
        engine = TensorLogicEngine()
        prog = engine.get_program("nonexistent")
        assert prog is None

    def test_run_inference(self):
        engine = TensorLogicEngine()
        prog = engine.create_program("test")
        prog.add_atom("P", arity=2, domain_sizes=[3, 3])
        prog.add_atom("Q", arity=2, domain_sizes=[3, 3])
        prog.add_fact("P", (0, 1), 1.0)
        prog.add_rule("copy", head="Q", body=["P"], einsum_notation="xy->xy")

        result = engine.run_inference("test", max_iterations=1)
        assert result["program"] == "test"
        assert "new_facts" in result
        assert "statistics" in result

    def test_run_inference_unknown_program(self):
        engine = TensorLogicEngine()
        with pytest.raises(ValueError):
            engine.run_inference("unknown")

    def test_create_transitive_closure_rule(self):
        engine = TensorLogicEngine()
        prog = engine.create_program("test")

        rule = engine.create_transitive_closure_rule(prog, "connected", domain_size=5)

        assert "connected" in prog.atoms
        assert rule.name == "connected_transitive"
        assert rule.einsum_notation == "xy,yz->xz"

    def test_get_statistics(self):
        engine = TensorLogicEngine()
        engine.create_program("prog1")
        engine.create_program("prog2")

        stats = engine.get_statistics()
        assert stats["num_programs"] == 2
        assert "prog1" in stats["programs"]
        assert "prog2" in stats["programs"]

    def test_export_all(self, tmp_path):
        engine = TensorLogicEngine()
        prog = engine.create_program("test")
        prog.add_atom("P", arity=1, domain_sizes=[3])

        engine.export_all(str(tmp_path))

        exported = tmp_path / "test.json"
        assert exported.exists()


class TestUtilityFunctions:
    """Tests for utility functions."""

    def test_create_identity_tensor(self):
        ident = create_identity_tensor(3)
        assert ident.shape.dimensions == (3, 3)
        assert ident.get_element((0, 0)) == 1.0
        assert ident.get_element((1, 1)) == 1.0
        assert ident.get_element((2, 2)) == 1.0
        assert ident.get_element((0, 1)) == 0.0

    def test_create_logical_and(self):
        shape = TensorShape(dimensions=(3,))
        a = Tensor(
            name="A",
            shape=shape,
            tensor_type=TensorType.LOGICAL,
            data=[0.0, 0.5, 1.0]
        )
        b = Tensor(
            name="B",
            shape=shape,
            tensor_type=TensorType.LOGICAL,
            data=[1.0, 1.0, 0.5]
        )

        eq = create_logical_and(a, b)
        assert eq.output.data == [0.0, 0.5, 0.5]

    def test_create_logical_or(self):
        shape = TensorShape(dimensions=(3,))
        a = Tensor(
            name="A",
            shape=shape,
            tensor_type=TensorType.LOGICAL,
            data=[0.0, 0.5, 1.0]
        )
        b = Tensor(
            name="B",
            shape=shape,
            tensor_type=TensorType.LOGICAL,
            data=[0.3, 0.3, 0.5]
        )

        eq = create_logical_or(a, b)
        assert eq.output.data == [0.3, 0.5, 1.0]

    def test_create_logical_not(self):
        shape = TensorShape(dimensions=(3,))
        a = Tensor(
            name="A",
            shape=shape,
            tensor_type=TensorType.LOGICAL,
            data=[0.0, 0.5, 1.0]
        )

        result = create_logical_not(a)
        assert result.data == [1.0, 0.5, 0.0]

    def test_fuzzy_implies_true_to_true(self):
        # True implies True = True
        assert fuzzy_implies(1.0, 1.0) == 1.0

    def test_fuzzy_implies_true_to_false(self):
        # True implies False = False
        assert fuzzy_implies(1.0, 0.0) == 0.0

    def test_fuzzy_implies_false_to_anything(self):
        # False implies anything = True
        assert fuzzy_implies(0.0, 0.0) == 1.0
        assert fuzzy_implies(0.0, 1.0) == 1.0

    def test_t_norm_product(self):
        assert t_norm_product(0.5, 0.5) == 0.25
        assert t_norm_product(1.0, 0.8) == 0.8
        assert t_norm_product(0.0, 1.0) == 0.0

    def test_t_conorm_probabilistic(self):
        # P(A or B) = P(A) + P(B) - P(A and B)
        assert t_conorm_probabilistic(0.5, 0.5) == 0.75
        assert t_conorm_probabilistic(0.0, 1.0) == 1.0
        assert t_conorm_probabilistic(1.0, 1.0) == 1.0


class TestTensorLogicIntegration:
    """Integration tests for Tensor Logic system."""

    def test_full_reasoning_pipeline(self):
        """Test complete reasoning pipeline with rules and inference."""
        engine = TensorLogicEngine()
        prog = engine.create_program("family")

        # Define predicates
        prog.add_atom("parent", arity=2, domain_sizes=[5, 5])
        prog.add_atom("grandparent", arity=2, domain_sizes=[5, 5])

        # Add facts: parent(0, 1), parent(1, 2)
        prog.add_fact("parent", (0, 1), 1.0)
        prog.add_fact("parent", (1, 2), 1.0)

        # Add rule: parent(x,y) ∧ parent(y,z) → grandparent(x,z)
        prog.add_rule(
            name="grandparent_rule",
            head="grandparent",
            body=["parent", "parent"],
            einsum_notation="xy,yz->xz"
        )

        # Run inference
        result = engine.run_inference("family", max_iterations=1)

        # Check that grandparent(0, 2) is derived
        gp_value = prog.query("grandparent", (0, 2))
        assert gp_value > 0

    def test_embedding_based_reasoning(self):
        """Test reasoning using embedding space."""
        engine = TensorLogicEngine()
        prog = engine.create_program("kg", embedding_dim=16)
        prog.initialize_embedding_space(num_entities=10, num_relations=3)

        # Score some triples
        scores = [
            prog.query_embedding(head=i, relation=0, tail=(i+1) % 10)
            for i in range(5)
        ]

        # All scores should be valid probabilities
        for score in scores:
            assert 0.0 <= score <= 1.0

    def test_transitive_closure(self):
        """Test transitive closure computation."""
        engine = TensorLogicEngine()
        prog = engine.create_program("graph")

        engine.create_transitive_closure_rule(prog, "path", domain_size=4)

        # Add edges: 0->1, 1->2, 2->3
        prog.add_fact("path", (0, 1), 1.0)
        prog.add_fact("path", (1, 2), 1.0)
        prog.add_fact("path", (2, 3), 1.0)

        # Run forward chaining
        prog.forward_chain(max_iterations=3)

        # 0->2 should be derivable
        assert prog.query("path", (0, 2)) > 0

    def test_hybrid_neuro_symbolic(self):
        """Test hybrid neural-symbolic reasoning."""
        prog = TensorLogicProgram(name="hybrid", embedding_dim=32)

        # Add symbolic predicates
        prog.add_atom("known", arity=2, domain_sizes=[10, 10])
        prog.add_fact("known", (0, 1), 1.0)

        # Initialize neural embeddings
        prog.initialize_embedding_space(num_entities=10, num_relations=5)

        # Both query types should work
        symbolic_result = prog.query("known", (0, 1))
        neural_result = prog.query_embedding(head=0, relation=0, tail=1)

        assert symbolic_result == 1.0
        assert 0.0 <= neural_result <= 1.0
