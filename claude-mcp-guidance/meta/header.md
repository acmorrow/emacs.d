# MCP Tool Usage Guidance

The following custom guidance has been configured for MCP tools:

## Philosophy

You have access to Emacs-powered tools via claude-code-ide.el MCP server. These provide semantic understanding of code (LSP, tree-sitter, xref) and integration with the user's actual development environment (projectile, dir-locals). **Always prefer these over Bash/Grep when the task is semantic** (understanding code structure, finding symbols, running builds/tests).

If you find yourself reaching for bash, PAUSE AND THINK: is there an emacs MCP integration that does this job better. If so, ALWAYS USE THE EMACS MCP.

---

## Common Workflows

**Finding and Understanding Code:**

1. claude-code-ide-extras-emacs/find_file(file) → ensure buffer ready with LSP initialized (ALWAYS do this FIRST!)
2. xref_find_apropos("pattern", file) → find symbols matching pattern
3. claude-code-ide-extras-lsp/describe_thing_at_point(file, line, col) → get type info and docs
4. claude-code-ide-extras-emacs/xref_find_references_at_point(file, line, col) → find all call sites
5. Read relevant sections with full context

**Refactoring with Validation:**

1. claude-code-ide-extras-emacs/find_file(file) → ensure buffer ready with LSP (ALWAYS FIRST!)
2. treesit_info → understand exact boundaries of code to change
3. claude-code-ide-extras-emacs/xref_find_references_at_point → see what might be affected
4. Make edits with Write/Edit tools
5. getDiagnostics → verify no errors (BEFORE user sees changes)
6. claude-code-ide-extras-lsp/format_buffer → format
7. Present changes

**Build/Test Cycle:**

1. get_project_buffer_local_variables(file, "^projectile-project-") → get build/test commands
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

**CRITICAL: LSP Tool Prerequisite**
- **ALWAYS call `claude-code-ide-extras-emacs/find_file(file)` BEFORE using baseline LSP/semantic tools** (xref_find_apropos, getDiagnostics, treesit_info, imenu_list_symbols)
- Reason: Baseline tools from claude-code-ide.el don't force LSP initialization. If LSP is deferred (lsp-deferred in hooks), these tools fail or return incomplete results until the buffer is displayed to the user
- Our extras tools (xref_find_references_at_point, xref_find_definitions_at_point, lsp/describe_thing_at_point, lsp/format_buffer) already handle this internally - no find_file needed

**Use semantic tools when:**
- Finding symbols → xref_find_apropos (not Grep) [REQUIRES find_file first!]
- Finding references → xref_find_references_at_point (not Grep or deprecated xref_find_references)
- Getting type info → lsp_describe_thing_at_point (not reading definitions)
- Understanding code structure → treesit (not Read full file; imenu broken for C++) [REQUIRES find_file first!]
- Validating code correctness → getDiagnostics (not compile) [REQUIRES find_file first!]
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

## Baseline MCP Tools from claude-code-ide

**Semantic Code Navigation:**

- **`xref_find_apropos(pattern, file_path)`** - Find symbols matching pattern
  - Pattern-based search across project
  - LSP-aware, understands scope and types
  - Use for: "Find all methods named bar", "What symbols contain 'create'?"

- **`xref_find_references(identifier, file_path)`** - Find all uses of a symbol
  - **Deprecated**: Use `claude-code-ide-extras-emacs/xref_find_references_at_point` instead for reliable results

- **`imenu_list_symbols(file_path)`** - List file structure (functions, classes, vars)
  - Returns symbol names with locations
  - Use for: Quick overview before reading, navigating large files
  - Faster than Read when you need "what's in this file?"
  - Pair with Read: imenu first to see structure, then read specific sections

- **`treesit_info(file_path, line?, column?, include_ancestors?, include_children?, whole_file?)`** - Get AST structure
  - Returns tree-sitter syntax tree with node types, ranges, hierarchy
  - **WARNING**: `whole_file: true` generates massive output (100k+ tokens) - very expensive!
  - Use for: Understanding code structure before refactoring, finding exact boundaries
  - Use `include_ancestors: true` to see enclosing scope (function, class)
  - Use `include_children: true` to see what's inside a node
  - **Best practice**: Query specific locations (line/column), not whole file
  - **Example**: Before extracting code, query treesit to find exact function boundaries

**Real-Time Validation:**

- **`getDiagnostics(uri?)`** - Get LSP errors/warnings
  - Returns diagnostics for file(s) without compiling
  - **IMPORTANT**: Check after edits to catch errors early, BEFORE presenting changes to user
  - Use for: Validating refactorings, catching type errors instantly

**Project Context:**

- **`project_info()`** - Get project root, file count, current buffer
  - Quick context about where you're working

---

