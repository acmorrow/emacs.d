- **`claude-code-ide-extras-projectile/task_start(task_type, command, file_path)`** - Launch compilation/test (non-blocking)
  - `task_type`: compile, test, configure, install, package, run
  - Returns buffer name immediately
  - Non-blocking: returns instantly while task runs in background
  - **Workflow**:
    1. `claude-code-ide-extras-projectile/read_dir_locals` to see baseline (e.g., `cmake --build build/local`)
    2. Understand pattern (CMake + Ninja, what flags, what directory)
    3. Craft variation (e.g., `ctest --test-dir build/local --verbose --output-on-failure -R specific_test`)
    4. Launch with `claude-code-ide-extras-projectile/task_start`
  - **Using `task_type="run"` for arbitrary shell commands**:
    - The `run` task type can execute any shell command, not just project-specific commands
    - Output goes to a persistent buffer that can be queried/searched via `task_wait`, `task_query`, `task_search`
    - Useful when you need to work with command output (search, analyze, extract information)
    - **Limitation**: Only one task buffer per project at a time - next `task_start` replaces it
    - Examples: `git log --oneline`, `find . -name "*.cpp"`, analysis scripts, data processing commands
