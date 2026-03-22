# Development Process

## Pacing

After every non-trivial step, Claude must pause, briefly review what was done, and commit before proceeding. This ensures:
- Each commit is small and reviewable
- The human can intervene or redirect at any point
- Progress is never lost to a context window limit or interrupted session

## Issue lifecycle

1. **Selection**: Pick an issue based on risk/reward. Prefer issues with existing test coverage or where tests can be added.
2. **Branch**: Create a branch named `<issue-number>-<short-description>`.
3. **Implementation**: Work incrementally, committing after each non-trivial step.
4. **Testing**: Run `./script/test-website-all.sh` and any other relevant tests before declaring done.
5. **PR**: Push branch and create PR linking the issue.
6. **Review and merge**: See below.

## PR flow

All changes go through PRs, even when the committer has bypass permissions. This maintains the audit trail.

**Current state**: Claude creates PRs; human reviews, approves, and merges.

**Goal**: Evolve towards Claude-based PR approval, with human intervention only when necessary (e.g. architectural decisions, risky changes, external-facing changes). This requires either a dedicated bot account or organisation-level policy changes.

## Tracking Claude contributions

- Commits by Claude include `Co-Authored-By: Claude <noreply@anthropic.com>` trailer.
- This is searchable via `git log --grep="Co-Authored-By: Claude"`.
- GitHub issue comments record design decisions and progress.
- PRs created by Claude link to the issue and summarise what was done and why.

## GitHub conventions

- **Labels**: Use existing labels (`implementation`, `testing`, `setup`, `documentation`). Add `claude-automated` for issues where Claude drove the implementation.
- **Milestones**: Respect existing milestone assignments; don't reassign without human approval.
- **Issue bodies**: Keep checklist items updated as work progresses.

## Token permissions

The `gh` CLI token needs the following scopes for full workflow automation:
- `repo` scope (includes issues, PRs, comments), or for fine-grained tokens: **Issues: Read and write**
