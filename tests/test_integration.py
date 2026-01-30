"""Integration tests for spacecadet MCP tools (requires emacs)."""

import json
import shutil
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent))

pytestmark = pytest.mark.skipif(
    shutil.which("emacs") is None,
    reason="emacs not found in PATH",
)


@pytest.fixture
def org_dir(tmp_path, monkeypatch):
    """Create a temporary org directory and patch ORG_DIR."""
    tasks_dir = tmp_path / "tasks"
    tasks_dir.mkdir()
    (tasks_dir / "tasks.org").write_text(
        "#+TITLE: Test Tasks\n#+STARTUP: overview\n\n"
    )
    monkeypatch.setattr("server.ORG_DIR", str(tasks_dir))
    return tasks_dir


class TestAddTask:
    def test_basic(self, org_dir):
        from server import add_task
        result = json.loads(add_task(heading="Test task"))
        assert result["status"] == "ok"
        assert result["heading"] == "Test task"

    def test_returns_id(self, org_dir):
        from server import add_task
        result = json.loads(add_task(heading="ID task"))
        assert result["status"] == "ok"
        assert "id" in result
        assert len(result["id"]) > 8

    def test_with_priority_and_tags(self, org_dir):
        from server import add_task
        result = json.loads(add_task(heading="Tagged", priority="A", tags="work,urgent"))
        assert result["status"] == "ok"

    def test_path_traversal_rejected(self, org_dir):
        from server import add_task
        result = json.loads(add_task(heading="Evil", file="../../etc/evil.org"))
        assert result["status"] == "error"
        assert "escape" in result["message"]


class TestGetTask:
    def test_by_heading(self, org_dir):
        from server import add_task, get_task
        add_task(heading="Findable")
        result = json.loads(get_task(heading="Findable"))
        assert result["heading"] == "Findable"
        assert "id" in result

    def test_by_id(self, org_dir):
        from server import add_task, get_task
        add_result = json.loads(add_task(heading="ID lookup"))
        task_id = add_result["id"]
        result = json.loads(get_task(id=task_id))
        assert result["heading"] == "ID lookup"

    def test_not_found(self, org_dir):
        from server import get_task
        result = json.loads(get_task(heading="Nonexistent"))
        assert result["status"] == "error"

    def test_neither_id_nor_heading(self, org_dir):
        from server import get_task
        result = json.loads(get_task())
        assert result["status"] == "error"


class TestListTasks:
    def test_includes_id(self, org_dir):
        from server import add_task, list_tasks
        add_task(heading="Listed task")
        result = json.loads(list_tasks())
        assert len(result) >= 1
        assert "id" in result[0]

    def test_filter_by_state(self, org_dir):
        from server import add_task, list_tasks
        add_task(heading="Todo item")
        result = json.loads(list_tasks(state="TODO"))
        assert len(result) >= 1


class TestUpdateTask:
    def test_by_heading(self, org_dir):
        from server import add_task, update_task
        add_task(heading="Updatable")
        result = json.loads(update_task(heading="Updatable", new_state="DONE"))
        assert result["status"] == "ok"

    def test_by_id(self, org_dir):
        from server import add_task, update_task
        add_result = json.loads(add_task(heading="Update by ID"))
        task_id = add_result["id"]
        result = json.loads(update_task(id=task_id, new_state="DONE"))
        assert result["status"] == "ok"


class TestDeleteTask:
    def test_by_id(self, org_dir):
        from server import add_task, delete_task, list_tasks
        add_result = json.loads(add_task(heading="Deletable"))
        task_id = add_result["id"]
        result = json.loads(delete_task(id=task_id))
        assert result["status"] == "ok"
        # Verify it's gone
        tasks = json.loads(list_tasks())
        headings = [t["heading"] for t in tasks]
        assert "Deletable" not in headings


class TestRefileTask:
    def test_path_traversal_rejected(self, org_dir):
        from server import add_task, refile_task
        add_task(heading="Source")
        result = json.loads(refile_task(
            heading="Source",
            target_heading="Projects",
            target_file="../../etc/evil.org",
        ))
        assert result["status"] == "error"


class TestAddNote:
    def test_by_id(self, org_dir):
        from server import add_task, add_note
        add_result = json.loads(add_task(heading="Notable"))
        task_id = add_result["id"]
        result = json.loads(add_note(id=task_id, note="A test note"))
        assert result["status"] == "ok"


class TestSetProperty:
    def test_by_id(self, org_dir):
        from server import add_task, set_property
        add_result = json.loads(add_task(heading="Propertied"))
        task_id = add_result["id"]
        result = json.loads(set_property(id=task_id, property="Effort", value="2:00"))
        assert result["status"] == "ok"
