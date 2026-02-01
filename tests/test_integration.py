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
    monkeypatch.setattr("server._ORG_DIR", str(tasks_dir))
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


class TestErrorPaths:
    """Tests for error handling and edge cases."""

    def test_update_nonexistent_by_heading(self, org_dir):
        from server import update_task
        result = json.loads(update_task(heading="Does not exist", new_state="DONE"))
        assert result["status"] == "error"
        assert "not found" in result["message"].lower()

    def test_update_nonexistent_by_id(self, org_dir):
        from server import update_task
        result = json.loads(update_task(id="00000000-0000-0000-0000-000000000000", new_state="DONE"))
        assert result["status"] == "error"

    def test_delete_nonexistent(self, org_dir):
        from server import delete_task
        result = json.loads(delete_task(heading="Ghost task"))
        assert result["status"] == "error"

    def test_get_task_no_args(self, org_dir):
        from server import get_task
        result = json.loads(get_task())
        assert result["status"] == "error"

    def test_add_note_missing_note(self, org_dir):
        from server import add_task, add_note
        add_result = json.loads(add_task(heading="No note target"))
        task_id = add_result["id"]
        result = json.loads(add_note(id=task_id, note=""))
        assert result["status"] == "error"

    def test_set_property_missing_fields(self, org_dir):
        from server import add_task, set_property
        add_result = json.loads(add_task(heading="No prop target"))
        task_id = add_result["id"]
        result = json.loads(set_property(id=task_id, property="", value=""))
        assert result["status"] == "error"

    def test_update_task_no_id_or_heading(self, org_dir):
        from server import update_task
        result = json.loads(update_task(new_state="DONE"))
        assert result["status"] == "error"

    def test_add_task_state_persists(self, org_dir):
        """Verify that toggling state actually changes the org file."""
        from server import add_task, update_task, get_task
        add_result = json.loads(add_task(heading="Toggle me"))
        task_id = add_result["id"]
        assert add_result["todo"] == "TODO"

        update_task(id=task_id, new_state="DONE")
        get_result = json.loads(get_task(id=task_id))
        assert get_result["todo"] == "DONE"

    def test_add_task_invalid_state(self, org_dir):
        from server import add_task
        result = json.loads(add_task(heading="Bad state", state="BOGUS"))
        assert result["status"] == "error"
        assert "Invalid state" in result["message"]

    def test_add_task_invalid_priority(self, org_dir):
        from server import add_task
        result = json.loads(add_task(heading="Bad pri", priority="Z"))
        assert result["status"] == "error"
        assert "Invalid priority" in result["message"]

    def test_update_task_invalid_state(self, org_dir):
        from server import add_task, update_task
        add_result = json.loads(add_task(heading="Valid task"))
        task_id = add_result["id"]
        result = json.loads(update_task(id=task_id, new_state="BOGUS"))
        assert result["status"] == "error"
        assert "Invalid state" in result["message"]

    def test_update_task_invalid_priority(self, org_dir):
        from server import add_task, update_task
        add_result = json.loads(add_task(heading="Valid task 2"))
        task_id = add_result["id"]
        result = json.loads(update_task(id=task_id, new_priority="Z"))
        assert result["status"] == "error"
        assert "Invalid priority" in result["message"]


class TestClockOut:
    def test_clock_out_no_active_clock(self, org_dir):
        from server import clock_out
        result = json.loads(clock_out())
        assert result["status"] == "error"
        assert "no open clock" in result["message"].lower()


class TestRefileSuccess:
    def test_refile_under_new_parent(self, org_dir):
        """Create a parent heading, then refile a task under it."""
        from server import add_task, refile_task, get_task
        # Create a parent task and a child task
        parent = json.loads(add_task(heading="Projects"))
        assert parent["status"] == "ok"
        child = json.loads(add_task(heading="Sub task"))
        assert child["status"] == "ok"
        child_id = child["id"]
        result = json.loads(refile_task(
            id=child_id,
            target_heading="Projects",
        ))
        assert result["status"] == "ok"
        # Task should still be retrievable
        get_result = json.loads(get_task(id=child_id))
        assert get_result["heading"] == "Sub task"


class TestGetAgenda:
    def test_agenda_specific_date(self, org_dir):
        from server import add_task, get_agenda
        add_task(heading="Dated task", deadline="2025-06-15")
        result = get_agenda(date="2025-06-15")
        assert isinstance(result, str)

    def test_agenda_date_range(self, org_dir):
        from server import get_agenda
        result = get_agenda(range_start="2025-06-01", range_end="2025-06-30")
        assert isinstance(result, str)

    def test_agenda_invalid_date(self, org_dir):
        from server import get_agenda
        result = json.loads(get_agenda(date="2025-13-99"))
        assert result["status"] == "error"
        assert "not a valid" in result["message"]

    def test_agenda_invalid_range(self, org_dir):
        from server import get_agenda
        result = json.loads(get_agenda(range_start="2025-02-30", range_end="2025-06-30"))
        assert result["status"] == "error"


class TestSearchTasks:
    def test_search_by_tag(self, org_dir):
        from server import add_task, search_tasks
        add_task(heading="Searchable", tags="findme")
        result = json.loads(search_tasks(query="+findme"))
        assert isinstance(result, list)
        assert len(result) >= 1
        assert any(t["heading"] == "Searchable" for t in result)

    def test_search_no_results(self, org_dir):
        from server import search_tasks
        result = json.loads(search_tasks(query="+nonexistenttag"))
        assert isinstance(result, list)
        assert len(result) == 0

    def test_search_by_state(self, org_dir):
        from server import add_task, search_tasks
        add_task(heading="State search")
        result = json.loads(search_tasks(query="/TODO"))
        assert isinstance(result, list)
        assert len(result) >= 1
