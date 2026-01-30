"""Unit tests for path traversal validation (no emacs required)."""

import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent))

from server import validate_org_path


@pytest.fixture
def org_dir(tmp_path):
    """Provide a temporary org directory path."""
    return str(tmp_path)


class TestValidateOrgPath:
    def test_simple_filename(self, org_dir, monkeypatch):
        monkeypatch.setattr("server.ORG_DIR", org_dir)
        assert validate_org_path("tasks.org") == "tasks.org"

    def test_subdirectory(self, org_dir, monkeypatch):
        monkeypatch.setattr("server.ORG_DIR", org_dir)
        assert validate_org_path("subdir/tasks.org") == "subdir/tasks.org"

    def test_dot_slash(self, org_dir, monkeypatch):
        monkeypatch.setattr("server.ORG_DIR", org_dir)
        assert validate_org_path("./tasks.org") == "./tasks.org"

    def test_parent_traversal_rejected(self, org_dir, monkeypatch):
        monkeypatch.setattr("server.ORG_DIR", org_dir)
        with pytest.raises(ValueError, match="escape"):
            validate_org_path("../../etc/passwd")

    def test_absolute_path_outside_rejected(self, org_dir, monkeypatch):
        monkeypatch.setattr("server.ORG_DIR", org_dir)
        with pytest.raises(ValueError, match="escape"):
            validate_org_path("/tmp/evil.org")

    def test_sneaky_traversal_rejected(self, org_dir, monkeypatch):
        monkeypatch.setattr("server.ORG_DIR", org_dir)
        with pytest.raises(ValueError, match="escape"):
            validate_org_path("subdir/../../etc/passwd")

    def test_custom_param_name_in_error(self, org_dir, monkeypatch):
        monkeypatch.setattr("server.ORG_DIR", org_dir)
        with pytest.raises(ValueError, match="target_file"):
            validate_org_path("../../etc/passwd", param_name="target_file")


class TestTaskEnv:
    def test_id_only(self):
        from server import _task_env
        env = _task_env(id="abc-123", heading=None)
        assert env == {"SC_ID": "abc-123"}

    def test_heading_only(self):
        from server import _task_env
        env = _task_env(id=None, heading="My task")
        assert env == {"SC_HEADING": "My task"}

    def test_both_id_and_heading(self):
        from server import _task_env
        env = _task_env(id="abc-123", heading="My task")
        assert env["SC_ID"] == "abc-123"
        assert env["SC_HEADING"] == "My task"

    def test_neither_raises(self):
        from server import _task_env
        with pytest.raises(ValueError, match="required"):
            _task_env(id=None, heading=None)
