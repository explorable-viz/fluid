# Development Process

## Checklists

### After every push
- [ ] Check GitHub Actions for the pushed branch — confirm the run passes or diagnose failures

### After every commit
- [ ] Review what was done before proceeding to next step

### After merging an issue branch
- [ ] Delete the issue branch (local and remote)
- [ ] Update the milestone report if the issue is complete

### After populating a milestone
- [ ] Run `./script/check-project-integrity.sh` to verify invariants

### After any change to project board state
- [ ] Verify the project view at https://github.com/orgs/explorable-viz/projects/1

### Before npm publish
- [ ] Bump version in `fluid/package.json`
- [ ] Run `./script/stage-website.sh article`
- [ ] Run full test suite: `./script/test-website-all.sh`

## Principles

- Pause, review, and commit after every non-trivial step.
- Look for consolidation/refactoring opportunities — build from existing behaviours rather than reinventing.
- Minor documentation, process changes, and trivial fixes can be committed directly to the current milestone branch.

## Branching

All issues belong to a milestone. Milestones are named `<repo> <major>.<minor>` (e.g. `fluid 0.12`).

```
issue branch → milestone branch → develop → staging → release
```

- **Milestone branches**: named to match the milestone (e.g. `fluid-0.12`).
- **Issue branches**: named `<issue-number>-<short-description>`, branched from the milestone branch.
- **PRs to milestone branches**: Claude can merge directly once CI passes.
- **PRs to develop**: a developer decides when to merge. A [milestone report](milestone-report.md) is generated at this point.

## Deployment

```
develop → staging → release
```

Managed manually by a developer:
1. Merge `develop` to `staging` — triggers deploy to GitHub Pages (f.luid.org).
2. Test the live site.
3. If good: merge `staging` to `release`.
4. If broken: reset `staging` to `release`.

## NPM publishing

Package version tracks the milestone (e.g. `fluid 0.12` → `0.12.x`). Publish from `fluid/` via `yarn workspace @explorable-viz/fluid build-publish`.

## Issue lifecycle

1. **Select** an issue from the current milestone.
2. **Branch** from the milestone branch.
3. **Implement** incrementally, committing after each step.
4. **Test**: run `./script/test-website-all.sh` and any other relevant tests.
5. **Push and create PR** targeting the milestone branch.
6. **Merge** once CI passes. Delete the branch. Update milestone report.

See [issue-lifecycle.md](issue-lifecycle.md) for the state machine and invariants.

## Milestone population

When all issues in a milestone are done, paused, or awaiting decision, populate with 3–8 new issues:

1. **Planned issues first** — already prioritised by a developer.
2. **Proposed issues** — prefer low-hanging fruit, thematic fit, test coverage.

Adding to a milestone bumps Proposed → Planned. This can be done autonomously.

## Escalation

When the best approach isn't obvious, Claude should not guess:

1. Comment on the GitHub issue with options and a recommendation.
2. Move the issue to "Awaiting Decision" on the project board.
3. Move to other work.

**Destructive or organisation-wide changes require explicit approval.** This includes modifying project board fields, branch protection, org settings, or any mutation that replaces rather than appends data. See [incidents.md](incidents.md).

There is no undo for GitHub Projects v2 field mutations.

## Tracking

- Commits by Claude include `Co-Authored-By: Claude <noreply@anthropic.com>` trailer.
- Searchable via `git log --grep="Co-Authored-By: Claude"`.
- Issue comments record design decisions.
- PRs link to issues and summarise what was done.

## GitHub conventions

- **Issue titles**: noun phrase describing the goal.
- **New issues**: add to [project board](https://github.com/orgs/explorable-viz/projects/1) as Proposed or Planned.
- **Labels**: use existing labels (`implementation`, `testing`, `setup`, `documentation`).
- **Milestones**: all issues must belong to a milestone.
- **Issue bodies**: keep checklist items updated.

## Token setup

Set `GH_TOKEN` per repo via `.claude/settings.local.json` (gitignored):

```json
{"env": {"GH_TOKEN": "<fine-grained-pat>"}}
```

This avoids keyring conflicts across multiple GitHub organisations. The token needs:
- **Pull requests, Issues**: Read and write
- **Organisation projects**: Read, Write, Admin

## Manual setup requirements

When Claude encounters a missing permission or setting:
1. Document it in the pending tasks section.
2. Note the specific change needed.
3. Continue with available work.

## Pending manual tasks

None.

Completed tasks are recorded in [docs/reports/completed-manual-tasks.md](../reports/completed-manual-tasks.md).
