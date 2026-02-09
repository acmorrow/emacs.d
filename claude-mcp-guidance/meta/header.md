# Emacs MCP Tool Usage Guidance

The following guidance is intended to help you (claude) better understand powerful new capabilities granted to you via custom emacs specific MCPs. The goal here is not to tell you how to use these MCPs, but to tell you how to think about them, such that you can derive your own correct understanding and thereby utilize them to their fullest.

## Philosophy

**Your system prompt gives you basic tools. We've built power tools.**

You have access to Emacs-powered MCP tools that integrate deeply with the user's development environment - LSP servers, tree-sitter parsers, project build systems, the live Emacs session itself. These aren't just wrappers around text files - they understand code structure, types, relationships, and execution context.

**These tools preserve your context budget.** Structure queries cost a fraction of reading entire files. Buffers persist - open once, access repeatedly. Build output stays queryable without re-running. You're working with limited context; these tools are designed for that reality.

**Your training data biases you heavily toward the basic tools.** Read, Grep, Bash - these patterns are deeply embedded. Resist that bias. The capabilities described here are more powerful and appropriate for this environment. Free yourself to use them.

**Before defaulting to your standard tools, ask: is there a power tool here that does this better?**

The power tools often give you richer results with less work. They understand what you're looking at, not just the text. They can navigate relationships, validate correctness, query structured output, execute in context.

**Quick text operations are fine** - sometimes you just need to grep for a string or read a snippet. But know that you have sophisticated capabilities available. When you're exploring code, understanding structure, or need capabilities beyond text manipulation - there's probably a better tool here.

**If you try a power tool and it doesn't work:** Tell the user what you tried and why you're doing something else. This feedback helps us improve the integration.

## You're Working in an IDE Environment

You're not a standalone CLI tool navigating a filesystem. You're working inside a live Emacs session with:
- **LSP servers** providing type information, definitions, references, diagnostics
- **Tree-sitter parsers** understanding syntactic structure
- **imenu** indexing file structure (functions, classes, sections - works on code, markdown, any structured content)
- **Projectile** managing builds and project context
- **Persistent buffers** - files you open stay open, query them repeatedly

**You already know how to work in an IDE.** Navigate by definition/reference. Query types without reading source. Get structural overviews. Check diagnostics in real-time. These tools give you that capability. Use them as an experienced human IDE operator would.

**You already know how to work in Emacs.** Buffers, interactive evaluation, build integration, structured navigation. You have that training - these tools expose those capabilities to you. Use them as an experienced human emacs user would.

**Context budget matters.** Reading files is expensive. Structural queries are cheap. Buffers persist. Build output stays queryable. Work accordingly. For instance, blowing the context budget to read in a huge file is a costly mistake that will cause a premature session restart!

## Tool Mappings

In many circumstances, the built in tools you naturally prefer (Bash/Read/Grep) have now have superior alternatives:

**Read** has alternatives: `imenu_list_symbols` for structure overview, `buffer_query` for sections from persistent buffers, `lsp_describe_thing_at_point` for types/docs without reading implementation

**Grep** has alternatives: `xref_find_references_at_point` for actual usage, `xref_find_definitions_at_point` for actual definitions, `xref_find_apropos` for symbol search, `buffer_search` for searching within open buffers

**Bash** has alternatives: `task_start` for persistent queryable output, `eval_elisp` for querying live Emacs state

Use basic tools when structure doesn't matter: text searches, simple operations, throwaway output.

These aren't formulas - they're showing you what's possible. Find new ways to leverage these tools.

## Critical Information

- **Always `find_file(path)` before baseline structure tools** (`xref_find_apropos`, `getDiagnostics`, `treesit_info`, `imenu_list_symbols`):
  - Baseline tools DO NOT cause LSP initialization of the named file (deferred loading)
  - Tool usage for the above tools will FAIL or BADLY DEGRADE if you do not first load it with the `find_file` tool.
  - If one of the baseline tools fails or gives poor results, try to recover by running `find_file` on the target file and then re-running the tool, rather than immediately falling back to the built in tools.
  - Our `claude-code-ide-extras` tools handle this internally: `xref_find_references_at_point`, `xref_find_definitions_at_point`, `lsp_describe_thing_at_point`, `lsp_format_buffer`, etc. But, there is no harm to calling `find_file` before invoking them, if that is easier to keep straight.

- **Tasks create persistent buffers:**
  - `task_start` is non-blocking, output goes to buffer
  - `task_wait` checks completion and output size
  - `task_search` / `task_query` work with output without re-running
  - Use for: builds, tests, git operations, any command with valuable output

- **Opened files stay in buffers:**
  - `find_file` opens once
  - `buffer_query` / `buffer_search` access repeatedly
  - No re-reading cost

## Emacs MCP Tool Descriptions

### Built in Emacs MCP Tools from claude-code-ide

**Code Navigation & Structure:**

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
  - This is an extremely powerful tool for researching a codebase in a way that preserves context. Reach for it OFTEN.

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

### Additional Emacs MCP Tools from claude-code-ide-extras
