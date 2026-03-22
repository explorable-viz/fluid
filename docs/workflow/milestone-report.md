# Milestone Report Format

A milestone report is generated when a milestone branch is merged to develop. It provides an audit trail and metrics for the work done.

## Format

```
# Milestone: <name>

## Summary
- Issues completed: N
- Issues remaining: N (with statuses)
- Date range: YYYY-MM-DD to YYYY-MM-DD

## Issues

### #<number>: <title>
- **Status**: Done | Awaiting Decision | Paused
- **Commits**: N total, N by Claude, N by human
- **Files modified**: N
- **Decision points**: autonomous | human intervention required
  - <brief description of any decisions escalated or made>

(repeated for each issue)

## Metrics
- Total commits: N
- Claude-authored commits: N (N%)
- Human-authored commits: N (N%)
- Total files modified: N
- Issues completed autonomously: N
- Issues requiring human intervention: N
```

## Generation

The report is generated from git history and GitHub issue data:
- Commits attributed to Claude are identified by the `Co-Authored-By: Claude` trailer.
- Decision points are identified from GitHub issue comments and "Awaiting Decision" status transitions.
- File counts come from `git diff --stat` between the milestone branch base and head.

## Storage

Reports are committed to `docs/reports/<milestone-name>.md` (e.g. `docs/reports/fluid-0.12.md`).
