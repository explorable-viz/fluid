# Development Process

## Checklists

### After every push
- [ ] Check GitHub Actions for pushed branch — confirm run passes or diagnose failures

### After every commit
- [ ] Review what was done before proceeding

### After merging issue branch
- [ ] Delete issue branch (local and remote)
- [ ] Update milestone report if issue is complete

### After populating milestone
- [ ] Run `./script/check-project-integrity.sh` to verify invariants

### After any change to project board state
- [ ] Verify project view at https://github.com/orgs/explorable-viz/projects/1

### Before npm publish
- [ ] Bump version in `fluid/package.json`
- [ ] Run `./script/stage-website.sh article`
- [ ] Run full test suite: `./script/test-website-all.sh`

## Principles

- Pause, review, and commit after every non-trivial step.
- Look for consolidation/refactoring opportunities — build from existing behaviours rather than reinventing.
- Minor documentation, process changes, and trivial fixes can be committed directly to current milestone branch.

## Branching

All issues belong to a milestone. Milestones named `<repo> <major>.<minor>` (e.g. `fluid 0.12`).

```
issue branch → milestone branch → develop → staging → release
```

- **Milestone branches**: named to match milestone (e.g. `fluid-0.12`).
- **Issue branches**: named `<issue-number>-<short-description>`, branched from milestone branch.
- **PRs to milestone branches**: Claude can merge directly once CI passes.
- **PRs to develop**: developer decides when to merge. [Milestone report](milestone-report.md) generated at this point.

## Deployment

```
develop → staging → release
```

Managed manually by developer:
1. Merge `develop` to `staging` — triggers deploy to GitHub Pages (f.luid.org).
2. Test live site.
3. If good: merge `staging` to `release`.
4. If broken: reset `staging` to `release`.

## NPM publishing

Package version tracks milestone (e.g. `fluid 0.12` → `0.12.x`). Publish from `fluid/` via `yarn workspace @explorable-viz/fluid build-publish`.

## Issue lifecycle

1. **Select** issue from current milestone.
2. **Branch** from milestone branch.
3. **Implement** incrementally, committing after each step.
4. **Test**: run `./script/test-website-all.sh` and any other relevant tests.
5. **Push and create PR** targeting milestone branch.
6. **Merge** once CI passes. Delete branch. Update milestone report.

See [issue-lifecycle.md](issue-lifecycle.md) for state machine and invariants.

## Milestone population

When all issues in milestone are done, paused, or awaiting decision, populate with 3–8 new issues:

1. **Planned issues first** — already prioritised by developer.
2. **Proposed issues** — prefer low-hanging fruit, thematic fit, test coverage.

Adding to milestone bumps Proposed → Planned. Can be done autonomously.

## Escalation

When best approach isn't obvious, Claude should not guess:

1. Comment on GitHub issue with options and recommendation.
2. Move issue to "Awaiting Decision" on project board.
3. Move to other work.

**Destructive or organisation-wide changes require explicit approval.** This includes modifying project board fields, branch protection, org settings, or any mutation that replaces rather than appends data. See [incidents.md](incidents.md).

No undo for GitHub Projects v2 field mutations.

## Tracking

- Commits by Claude include `Co-Authored-By: Claude <noreply@anthropic.com>` trailer.
- Searchable via `git log --grep="Co-Authored-By: Claude"`.
- Issue comments record design decisions.
- PRs link to issues and summarise what was done.

## GitHub conventions

- **Issue titles**: noun phrase describing goal.
- **New issues**: add to [project board](https://github.com/orgs/explorable-viz/projects/1) as Proposed or Planned.
- **Labels**: use existing labels (`implementation`, `testing`, `setup`, `documentation`).
- **Milestones**: all issues must belong to milestone.
- **Issue bodies**: keep checklist items updated.

## Token setup

Set `GH_TOKEN` per repo via `.claude/settings.local.json` (gitignored):

```json
{"env": {"GH_TOKEN": "<fine-grained-pat>"}}
```

Avoids keyring conflicts across multiple GitHub organisations. Token needs:
- **Pull requests, Issues**: Read and write
- **Organisation projects**: Read, Write, Admin

## Manual setup requirements

When Claude encounters missing permission or setting:
1. Document in pending tasks section.
2. Note specific change needed.
3. Continue with available work.

## Pending manual tasks

None.

Completed tasks recorded in [docs/reports/completed-manual-tasks.md](../reports/completed-manual-tasks.md).
