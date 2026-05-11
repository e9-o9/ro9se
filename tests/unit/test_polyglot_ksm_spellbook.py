"""
Tests for the Polyglot KSM hyper-skill spellbook plugin.
"""

from pathlib import Path

import yaml


def spellbook_dir(root_dir: Path) -> Path:
    return root_dir / "opencog" / "spellbook" / "polyglot-ksm.skill"


def test_polyglot_ksm_spellbook_manifest_is_discoverable(root_dir):
    """The polyglot KSM plugin should expose a machine-readable manifest."""
    manifest_path = spellbook_dir(root_dir) / "spellbook.yaml"
    skill_path = spellbook_dir(root_dir) / "SKILL.md"

    assert manifest_path.exists()
    assert skill_path.exists()

    manifest = yaml.safe_load(manifest_path.read_text())
    assert manifest["name"] == "polyglot-ksm"
    assert manifest["type"] == "hyper-skill-spellbook"
    assert set(manifest["components"]) == {"cogutil", "atomspace", "cogserver"}


def test_polyglot_ksm_spellbook_declares_executable_validation(root_dir):
    """Every spell should have commands or success criteria that make it operational."""
    manifest = yaml.safe_load((spellbook_dir(root_dir) / "spellbook.yaml").read_text())

    spell_ids = {spell["id"] for spell in manifest["spells"]}
    assert {"coverage-parity", "language-union-propagation", "interface-crystallization", "repair-and-seal"}.issubset(spell_ids)

    for spell in manifest["spells"]:
        assert spell.get("objective")
        assert spell.get("commands")
        assert spell.get("success")
