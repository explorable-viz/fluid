# Development Process

## Pacing

After every non-trivial step, Claude must pause, briefly review what was done, and commit before proceeding. This ensures:
- Each commit is small and reviewable
- The human can intervene or redirect at any point
- Progress is never lost to a context window limit or interrupted session

When implementing a new feature, always look for a consolidation or refactoring opportunity that allows it to be built from existing behaviours, rather than reinventing the wheel.

## Branching

All issues belong to a milestone. Milestones are named `<repo> <major>.<minor>` (e.g. `fluid 0.12`).

**Branch structure:**

```
issue branch → milestone branch → develop → staging → release
```

- **Milestone branches** are named to match the milestone (e.g. `fluid-0.12`). They serve as integration branches for related work.
- **Issue branches** are named `<issue-number>-<short-description>` and branch from the relevant milestone branch.
- **PRs from issue branches to milestone branches**: Claude can merge directly once CI passes (no human approval required).
- **PRs from milestone branches to develop**: a developer decides when to merge. A [milestone report](milestone-report.md) is generated at this point.

## Deployment

```
develop → staging → release
```

Deployments are managed manually by a developer.

1. **Merge `develop` to `staging`**: triggers the deploy workflow, which builds and publishes the `fluid-org` SvelteKit site to GitHub Pages (f.luid.org).
2. **Test the live site.**
3. **If good**: merge `staging` to `release`. `release` always reflects the last-known-good deployment.
4. **If broken**: reset `staging` to `release` to roll back.

## Milestone population

When all issues in a milestone are either done, paused, or awaiting decision, Claude should populate it with 3-8 new issues (depending on anticipated difficulty). Candidates are chosen by:

1. **Planned issues first**: issues with status "Planned" but not yet assigned to a milestone have already been prioritised by a developer and are strong candidates.
2. **Proposed issues**: for remaining slots, select from "Proposed" issues, preferring low-hanging fruit — low risk, thematic fit with the milestone, alignment with apparent priorities, and availability of test coverage.

Adding an issue to a milestone bumps it from Proposed to Planned.

This can be done autonomously — no human approval needed.

After populating, run `./script/check-project-integrity.sh` to verify invariants.

## Issue lifecycle

1. **Selection**: Pick an issue from the current milestone. Prefer issues with existing test coverage or where tests can be added.
2. **Branch**: Create an issue branch from the relevant milestone branch.
3. **Implementation**: Work incrementally, committing after each non-trivial step.
4. **Testing**: Run `./script/test-website-all.sh` and any other relevant tests before declaring done.
5. **PR**: Push branch and create PR targeting the milestone branch.
6. **Merge**: Claude merges to milestone branch once CI passes. Delete the issue branch (local and remote) after merging. A developer merges milestone branch to develop.

Minor documentation, process changes, and trivial fixes can be committed directly to the current milestone branch without a separate issue branch or PR.

## Escalation

When the best way to proceed isn't obvious, Claude should not guess — instead:

1. Add a comment to the GitHub issue explaining the decision required, the options considered, and a recommendation if possible.
2. Move the issue to "Awaiting Decision" status in the GitHub Project board.
3. Stop work on that issue and move to other work if available.

The comment should be self-contained so a developer can make the decision asynchronously.

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

- **Issue titles**: Use a noun phrase describing the goal (e.g. "Python-compatible names for standard library functions"), not an imperative (e.g. "Rename standard library functions").
- **New issues**: Must be added to the [Fluid project board](https://github.com/orgs/explorable-viz/projects/1) and assigned a status of Proposed or Planned.
- **Labels**: Use existing labels (`implementation`, `testing`, `setup`, `documentation`).
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

- [x] Re-add `project` scope (`read:project`, `project`) to the GitHub PAT — token regenerated and `gh` re-authenticated
- [x] Enable "Automatically delete head branches" in repo settings (Settings → General)

Completed tasks are recorded in [docs/reports/completed-manual-tasks.md](../reports/completed-manual-tasks.md).
