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

## Escalation

When the best way to proceed isn't obvious, Claude should not guess — instead:

1. Add a comment to the GitHub issue explaining the decision required, the options considered, and a recommendation if possible.
2. Move the issue to "Awaiting Decision" status in the GitHub Project board.
3. Stop work on that issue and move to other work if available.

The comment should be self-contained so the human can make the decision asynchronously.

Escalation also applies to **mutations on shared state** (project board fields, branch protection rules, organisation settings). These should always be flagged for human approval, even when the token has permission. See [incidents.md](incidents.md) for examples.

## Tracking Claude contributions

- Commits by Claude include `Co-Authored-By: Claude <noreply@anthropic.com>` trailer.
- This is searchable via `git log --grep="Co-Authored-By: Claude"`.
- GitHub issue comments record design decisions and progress.
- PRs created by Claude link to the issue and summarise what was done and why.

## GitHub conventions

- **Labels**: Use existing labels (`implementation`, `testing`, `setup`, `documentation`). Add `claude-automated` for issues where Claude drove the implementation.
- **Milestones**: Respect existing milestone assignments; don't reassign without human approval.
- **Issue bodies**: Keep checklist items updated as work progresses.

## Manual setup requirements

Some workflow capabilities depend on GitHub settings (token scopes, project board configuration, branch protection rules) that cannot be changed by Claude. When Claude encounters a missing permission or setting, it should:

1. Document the requirement in the pending tasks section below.
2. Note the specific setting change needed and why.
3. Continue with available work.

## Token permissions

The `gh` CLI token needs the following scopes for full workflow automation:
- `repo` scope (includes issues, PRs, comments), or for fine-grained tokens: **Issues: Read and write**
- `project` scope — needed to update project board statuses (e.g. moving issues to "Awaiting Decision")

## Pending manual tasks

- [x] Add `project` scope to the GitHub PAT so Claude can update project board statuses (needed for the escalation workflow: moving issues to "Awaiting Decision")
- [x] Add "Awaiting Decision" status option to the GitHub Project board (created via GraphQL API once token scope was granted)
