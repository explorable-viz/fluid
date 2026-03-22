# Development Process

## Pacing

After every non-trivial step, Claude must pause, briefly review what was done, and commit before proceeding. This ensures:
- Each commit is small and reviewable
- The human can intervene or redirect at any point
- Progress is never lost to a context window limit or interrupted session

## Branching

All issues belong to a milestone. Milestones are named `<repo> <major>.<minor>` (e.g. `fluid 0.12`).

**Branch structure:**

```
issue branch → milestone branch → develop
```

- **Milestone branches** are named to match the milestone (e.g. `fluid-0.12`). They serve as integration branches for related work.
- **Issue branches** are named `<issue-number>-<short-description>` and branch from the relevant milestone branch.
- **PRs from issue branches to milestone branches**: Claude can merge directly once CI passes (no human approval required).
- **PRs from milestone branches to develop**: human decides when to merge.

## Milestone population

When all issues in a milestone are either done, paused, or awaiting decision, Claude should populate it with 3-8 new issues (depending on anticipated difficulty). Candidates are chosen by:

1. **Planned issues first**: issues with status "Planned" but not yet assigned to a milestone have already been prioritised by the human and are strong candidates.
2. **Proposed issues**: for remaining slots, select from "Proposed" issues, preferring low-hanging fruit — low risk, thematic fit with the milestone, alignment with apparent priorities, and availability of test coverage.
3. Claude should present the proposed selection for human approval before assigning issues to the milestone.

## Issue lifecycle

1. **Selection**: Pick an issue from the current milestone. Prefer issues with existing test coverage or where tests can be added.
2. **Branch**: Create an issue branch from the relevant milestone branch.
3. **Implementation**: Work incrementally, committing after each non-trivial step.
4. **Testing**: Run `./script/test-website-all.sh` and any other relevant tests before declaring done.
5. **PR**: Push branch and create PR targeting the milestone branch.
6. **Merge**: Claude merges to milestone branch once CI passes. Human merges milestone branch to develop.

## Escalation

When the best way to proceed isn't obvious, Claude should not guess — instead:

1. Add a comment to the GitHub issue explaining the decision required, the options considered, and a recommendation if possible.
2. Move the issue to "Awaiting Decision" status in the GitHub Project board.
3. Stop work on that issue and move to other work if available.

The comment should be self-contained so the human can make the decision asynchronously.

**Destructive or organisation-wide changes require explicit human approval before execution.** This includes:
- Modifying project board field definitions (adding/removing/renaming status options)
- Changing branch protection rules
- Modifying organisation or repository settings
- Any GraphQL mutation that replaces rather than appends data

Claude must describe the intended change and its blast radius, and wait for approval. Having token permission is not sufficient justification to proceed. See [incidents.md](incidents.md) for examples of what can go wrong.

**There is no undo facility for GitHub Projects v2 field mutations.** Changes to field definitions (options, names, types) are irreversible via the API.

**After any change to project board state** (item statuses, field values, milestones), Claude must verify the project view at https://github.com/orgs/explorable-viz/projects/1 to confirm it hasn't been broken. Project views cannot be created or edited via the API — only read — so damage to views requires manual repair.

## Tracking Claude contributions

- Commits by Claude include `Co-Authored-By: Claude <noreply@anthropic.com>` trailer.
- This is searchable via `git log --grep="Co-Authored-By: Claude"`.
- GitHub issue comments record design decisions and progress.
- PRs created by Claude link to the issue and summarise what was done and why.

## GitHub conventions

- **Labels**: Use existing labels (`implementation`, `testing`, `setup`, `documentation`). Add `claude-automated` for issues where Claude drove the implementation.
- **Milestones**: All issues must belong to a milestone. Don't reassign without human approval.
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
- [x] Rename milestone "fluid-org 1.0" to "fluid 0.13"
- [x] Create milestone branch `fluid-0.12` from develop
- [x] Create milestone branch `fluid-0.13` from develop
