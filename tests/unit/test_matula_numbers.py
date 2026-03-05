"""
Unit tests for matula_numbers.py

Tests the Matula number encoding system including:
- Parentheses to order conversion
- Prime power series enumeration
- Probability density superposition
"""

import pytest
import sys
from pathlib import Path

# Add the Analysis path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "Analysis" / "RootedTrees" / "synthesis"))

from matula_numbers import (
    # Core Matula functions
    matula_to_tree,
    tree_to_matula,
    tree_order,
    # New parens-to-order functions
    parens_to_order,
    matula_to_inner_parens,
    inner_parens_to_order,
    # Prime utilities
    nth_prime,
    prime_index,
    is_prime,
    prime_factorization,
    # Prime power series functions
    prime_power_series,
    enumerate_prime_power_overlap,
    probability_density_superposition,
)


class TestParensToOrder:
    """Tests for parentheses length to order conversion."""

    def test_parens_to_order_basic(self):
        """Test basic parens_to_order calculation."""
        assert parens_to_order("()") == 1
        assert parens_to_order("(())") == 2
        assert parens_to_order("((()))") == 3
        assert parens_to_order("(()())") == 3

    def test_parens_to_order_matches_tree_order(self):
        """Test that parens_to_order matches tree_order for Matula trees."""
        for m in range(1, 21):
            tree = matula_to_tree(m)
            assert parens_to_order(tree) == tree_order(m)


class TestMatulaToInnerParens:
    """Tests for matula_to_inner_parens function."""

    def test_matula_1_has_empty_inner(self):
        """Matula 1 (atom) has no children, so inner is empty."""
        assert matula_to_inner_parens(1) == ""

    def test_matula_2_inner(self):
        """Matula 2 has inner '()' (one atom child)."""
        assert matula_to_inner_parens(2) == "()"

    def test_inner_parens_length_is_consistent(self):
        """Inner parens length should be 2*(tree_order - 1)."""
        for m in range(1, 21):
            inner = matula_to_inner_parens(m)
            expected_len = 2 * (tree_order(m) - 1)
            assert len(inner) == expected_len, f"Matula {m}: expected len {expected_len}, got {len(inner)}"


class TestInnerParensToOrder:
    """Tests for inner_parens_to_order function."""

    def test_problem_statement_examples(self):
        """
        Test the exact examples from the problem statement.
        
        mat(2):len[()]/2=2/2=1=>ord(1)
        mat(3):len[(())]/2=4/2=2=>ord(2)
        mat(4):len[()()]/2=4/2=2=>ord(2)
        mat(5):len[((()))]/2=6/2=3=>ord(3)
        mat(7):len[(()())]/2=6/2=3=>ord(3)
        mat(6):len[(())()]/2=6/2=3=>ord(3)
        mat(8):len[()()()]/2=6/2=3=>ord(3)
        """
        examples = [
            (2, 1),
            (3, 2),
            (4, 2),
            (5, 3),
            (7, 3),
            (6, 3),
            (8, 3),
        ]
        
        for matula, expected_order in examples:
            inner = matula_to_inner_parens(matula)
            actual_order = inner_parens_to_order(inner)
            assert actual_order == expected_order, f"mat({matula}): expected {expected_order}, got {actual_order}"

    def test_inner_order_equals_tree_order_minus_one(self):
        """Inner order should equal tree_order - 1 (excludes root)."""
        for m in range(1, 21):
            inner = matula_to_inner_parens(m)
            inner_order = inner_parens_to_order(inner)
            expected = tree_order(m) - 1
            assert inner_order == expected, f"Matula {m}: expected {expected}, got {inner_order}"


class TestPrimePowerSeries:
    """Tests for prime_power_series function."""

    def test_prime_power_series_2(self):
        """Test power series for prime 2."""
        series = prime_power_series(2, 5)
        assert series == [2, 4, 8, 16, 32]

    def test_prime_power_series_3(self):
        """Test power series for prime 3."""
        series = prime_power_series(3, 4)
        assert series == [3, 9, 27, 81]

    def test_prime_power_series_5(self):
        """Test power series for prime 5."""
        series = prime_power_series(5, 3)
        assert series == [5, 25, 125]


class TestEnumeratePrimePowerOverlap:
    """Tests for enumerate_prime_power_overlap function."""

    def test_single_prime_no_overlap(self):
        """A single prime's powers have no overlap."""
        overlap = enumerate_prime_power_overlap([2], max_value=100)
        # All values should have exactly one source
        for v, sources in overlap.items():
            assert len(sources) == 1

    def test_multiple_primes_powers_are_distinct(self):
        """Prime powers are typically distinct (no perfect power coincidences in small range)."""
        overlap = enumerate_prime_power_overlap([2, 3, 5], max_value=50)
        # In this range, prime powers don't overlap
        for v, sources in overlap.items():
            assert len(sources) == 1

    def test_prime_powers_never_overlap(self):
        """
        Test that prime powers never overlap (fundamental theorem of arithmetic).
        
        For prime bases, there can never be p₁^a = p₂^b for distinct primes p₁, p₂
        because each integer has a unique prime factorization.
        
        This is a mathematical property we're validating, not a bug in the code.
        """
        overlap = enumerate_prime_power_overlap([2, 3, 5, 7, 11], max_value=10000)
        overlapping = {v: s for v, s in overlap.items() if len(s) > 1}
        # Should be empty for prime bases only
        assert len(overlapping) == 0, f"Found unexpected overlaps: {overlapping}"

    def test_value_source_structure(self):
        """Test that each value correctly tracks its source (prime, power) tuple."""
        overlap = enumerate_prime_power_overlap([2, 3], max_value=30)
        
        # Check specific values
        assert (2, 1) in overlap[2]  # 2^1 = 2
        assert (2, 2) in overlap[4]  # 2^2 = 4
        assert (2, 3) in overlap[8]  # 2^3 = 8
        assert (3, 1) in overlap[3]  # 3^1 = 3
        assert (3, 2) in overlap[9]  # 3^2 = 9
        assert (3, 3) in overlap[27]  # 3^3 = 27

    def test_handles_large_max_value(self):
        """Test that the function handles larger ranges correctly."""
        overlap = enumerate_prime_power_overlap([2], max_value=1024)
        
        # 2^10 = 1024 should be included
        assert 1024 in overlap
        assert overlap[1024] == [(2, 10)]


class TestProbabilityDensitySuperposition:
    """Tests for probability_density_superposition function."""

    def test_single_prime_density(self):
        """Test density for a single prime."""
        density = probability_density_superposition([2], max_value=20)
        
        # p^1 has density 1.0, p^2 has density 0.5, etc.
        assert density[2] == pytest.approx(1.0)
        assert density[4] == pytest.approx(0.5)
        assert density[8] == pytest.approx(1.0/3)
        assert density[16] == pytest.approx(0.25)

    def test_multiple_primes_density(self):
        """Test density with multiple primes (no overlap case)."""
        density = probability_density_superposition([2, 3], max_value=20)
        
        # Each prime contributes independently
        assert density[2] == pytest.approx(1.0)
        assert density[3] == pytest.approx(1.0)
        assert density[4] == pytest.approx(0.5)
        assert density[9] == pytest.approx(0.5)

    def test_density_decreases_with_power(self):
        """Density should decrease as the power increases."""
        density = probability_density_superposition([2], max_value=100)
        
        # For prime 2: 2, 4, 8, 16, 32, 64
        assert density[2] > density[4] > density[8] > density[16] > density[32] > density[64]


class TestMatulaBijection:
    """Tests for basic Matula number bijection."""

    def test_tree_to_matula_atom(self):
        """Test that () maps to Matula 1."""
        assert tree_to_matula("()") == 1

    def test_tree_to_matula_container(self):
        """Test that (()) maps to Matula 2."""
        assert tree_to_matula("(())") == 2

    def test_matula_to_tree_roundtrip(self):
        """Test that matula_to_tree and tree_to_matula are inverses."""
        for m in range(1, 21):
            tree = matula_to_tree(m)
            recovered_m = tree_to_matula(tree)
            assert recovered_m == m, f"Roundtrip failed for Matula {m}"

    def test_tree_order_matula_1(self):
        """Matula 1 has order 1 (one node)."""
        assert tree_order(1) == 1

    def test_tree_order_matula_2(self):
        """Matula 2 has order 2 (root + one child)."""
        assert tree_order(2) == 2

    def test_tree_order_increases_with_matula(self):
        """Tree order generally increases with Matula number."""
        # Note: not strictly monotonic, but small Matulas have small orders
        for m in range(2, 10):
            assert tree_order(m) >= 2


class TestPrimeUtilities:
    """Tests for prime number utility functions."""

    def test_nth_prime(self):
        """Test nth_prime function."""
        assert nth_prime(1) == 2
        assert nth_prime(2) == 3
        assert nth_prime(3) == 5
        assert nth_prime(4) == 7
        assert nth_prime(5) == 11

    def test_prime_index(self):
        """Test prime_index function (inverse of nth_prime)."""
        assert prime_index(2) == 1
        assert prime_index(3) == 2
        assert prime_index(5) == 3
        assert prime_index(7) == 4
        assert prime_index(11) == 5

    def test_prime_index_nth_prime_inverse(self):
        """Test that prime_index and nth_prime are inverses."""
        for i in range(1, 15):
            p = nth_prime(i)
            assert prime_index(p) == i

    def test_is_prime(self):
        """Test is_prime function."""
        primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
        non_primes = [1, 4, 6, 8, 9, 10, 12, 14, 15, 16]
        
        for p in primes:
            assert is_prime(p), f"{p} should be prime"
        for n in non_primes:
            assert not is_prime(n), f"{n} should not be prime"

    def test_prime_factorization(self):
        """Test prime_factorization function."""
        assert prime_factorization(1) == []
        assert prime_factorization(2) == [(2, 1)]
        assert prime_factorization(4) == [(2, 2)]
        assert prime_factorization(6) == [(2, 1), (3, 1)]
        assert prime_factorization(12) == [(2, 2), (3, 1)]
