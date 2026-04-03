# Plan: Transition to Native org-ql Sorting in org-mcp

## Goal
Replace the legacy Python-based sorting script (`sort_backlog.py`) with native
`org-ql` sorting using the `agile-gtd--item-rank` numeric ranking system. This
ensures that urgency logic is centralized in Emacs Lisp and available directly
to the `org-mcp` server.

## Current Context
- **Urgency Logic**: Located in `packages/agile-gtd/agile-gtd.el`. The function `agile-gtd--item-rank` calculates a numeric value where lower = more urgent.
- **Ranking Factors**: Effective priority (inherits from parent if parent is higher) and deadline proximity.
- **MCP Server**: Located in `~/work/org-mcp/org-mcp.el`. It currently uses `org-ql` but doesn't expose `rank` or use it for sorting in a standardized way for the GTD workflow.

## Step 1: Verify & Fix Ranking Logic
- [ ] Extend `packages/agile-gtd/test/agile-gtd-rank-test.el`.
    - Test the 
- [ ] Verify that lower rank = more urgent in priority and deadline tests.
- [ ] run agile-gtd tests with Eask

## Step 2: Update `org-mcp` Metadata Extraction
- [ ] **Modify `org-mcp--ql-extract-match`**:
    - Location: `~/work/org-mcp/org-mcp.el`.
    - Change: Add customization for org-mcp to load additional properties into the results.
      rank . #'agilte-gtd--item-rank
      this should evaluate to loading `(rank . ,(agile-gtd--item-rank))`
      parent-priority should also be loaded via customizations and not as default `(parent-priority . ,(...))`
- [ ] implement tests to verify setting the additional properties

## Step 3: Implement Opinionated GTD Tools
The queries backing the query-inbox(), query-next(&optional tag) and
query-backlog(&optional tag) shall be customazable in org-mcp.

Sorting function shall be customizable as well.
Default shall be nil (for now)
Provide configuration example with a simple org-ql sexp and a priority sorting.
Provide configuration example with agile-gtd query functions.

exemplary configuration
(setq org-mcp-query-inbox-fn #'agile-gtd-inbox
      org-mcp-query-backlog-fn #-agile-gtd-query-backlog 
      org-mcp-query-next-fn #-agile-gtd-query-next)

Check the signature for these functions. backlog and next need to allow an optional parameter.

leave current stored org-ql logic in untouched!

## Step 4: Cleanup & Skill Integration
- [ ] **Delete Script**: `rm ~/.claude/skills/org-gtd-workflow/sort_backlog.py`.
- [ ] **Update Skill**: Edit `~/.claude/skills/org-gtd-workflow/SKILL.md`.
    - Remove instructions about manual sorting via Python.
    - Add instructions for using the new `query-inbox`, `query-next`, and `query-backlog` tools.
