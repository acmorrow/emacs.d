# MCP Tool Usage Guidance

The following custom guidance has been configured for MCP tools:

## Philosophy

You have access to Emacs-powered tools via claude-code-ide.el MCP server. These provide semantic understanding of code (LSP, tree-sitter, xref) and integration with the user's actual development environment (projectile, dir-locals). **Always prefer these over Bash/Grep when the task is semantic** (understanding code structure, finding symbols, running builds/tests).

If you find yourself reaching for bash, PAUSE AND THINK: is there an emacs MCP integration that does this job better. If so, ALWAYS USE THE EMACS MCP.

---

## Common Workflows

**Finding and Understanding Code:**

1. xref_find_apropos("pattern", file) → find symbols matching pattern
2. claude-code-ide-extras-lsp/describe_thing_at_point(file, line, col) → get type info and docs
3. xref_find_references("identifier", file) → find all call sites
4. Read relevant sections with full context

**Refactoring with Validation:**

1. treesit_info → understand exact boundaries of code to change
2. xref_find_references → see what might be affected
3. Make edits
4. getDiagnostics → verify no errors (BEFORE user sees changes)
5. claude-code-ide-extras-lsp/format_buffer → format
6. Present changes

**Build/Test Cycle:**

1. claude-code-ide-extras-projectile/read_project_dir_locals → see baseline commands
2. Understand pattern (build system, flags, directories)
3. Craft appropriate variation for current task
4. claude-code-ide-extras-projectile/task_start → launch (non-blocking)
5. Tell user: "Starting build/test..."
6. claude-code-ide-extras-projectile/task_wait → poll until finished, get output size
7. Decide based on output size and need:
   - Small output (<500 lines): task_query for full output
   - Large output + need beginning: task_query with start_line=1, num_lines=N
   - Large output + need end: task_query with start_line=-N, num_lines=N
   - Large output + looking for specific errors: task_search with pattern
8. Parse output, report results

**Learning Emacs Internals:**

1. claude-code-ide-extras-emacs/apropos("compilation") → find related symbols
2. claude-code-ide-extras-emacs/describe("compilation-in-progress", "variable") → understand specifics
3. See actual current value in user's session

---

## Tool Selection Guidelines

**Use semantic tools when:**
- Finding symbols → xref_find_apropos (not Grep)
- Finding references → xref_find_references (not Grep)
- Getting type info → lsp_describe_thing_at_point (not reading definitions)
- Understanding code structure → treesit (not Read full file; imenu broken for C++)
- Validating code correctness → getDiagnostics (not compile)
- Running builds/tests → projectile tasks (not Bash)

**Still use traditional tools when:**
- Searching string literals or comments → Grep
- Reading implementation details → Read
- Simple file operations → standard file tools

**Key principle**: Semantic understanding via MCP > text manipulation via Bash/Grep

---

## When to Use Projectile `task_start(task_type="run", ...)` vs Bash

**Use projectile `run` task when:**
- Command produces output you'll want to query/search/analyze
- Output might be large (can use `task_search` for specific patterns without retrieving everything)
- You want to check output size first (`task_wait`) before deciding retrieval strategy
- You need persistent buffer you can query multiple times in different ways
- Examples:
  - `git log`, `git diff --stat` - examine history/changes
  - `find . -name "*.cpp"` - then search results for specific patterns
  - Analysis scripts producing structured output
  - Any command where you'd normally pipe to `grep`, `head`, `tail`

**Use Bash directly when:**
- Simple operations with no meaningful output to examine: `mv`, `cp`, `mkdir`, `touch`
- Very small, immediate results where buffer infrastructure is overkill
- Operations where you need side effects, not output analysis
- Quick one-off commands with trivial output

**Key principle**: If the output is a valuable artifact you'll want to work with flexibly, use projectile `run`. If it's just immediate throwaway results or side effects, use Bash.

---

