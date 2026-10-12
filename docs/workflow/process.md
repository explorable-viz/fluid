# Development Process

Prose, code, commits, issues, pull requests and board Status follow the `technical-authoring`, `coding-conventions` and `github` skills. This document covers what is specific to Fluid.

## Branching

All issues belong to a milestone. Milestones named `<repo> <major>.<minor>` (e.g. `fluid 0.12`).

```
issue branch → milestone branch → develop → staging → release
```

- **Milestone branches**: named to match milestone (e.g. `fluid-0.12`).
- **Issue branches**: named `<issue-number>-<short-description>`, branched from milestone branch; PRs target milestone branch.
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

Package version tracks milestone (e.g. `fluid 0.12` → `0.12.x`). Before publishing: run `./script/test-website-all.sh`; working tree clean (publish script commits the version bump). Publish via `yarn workspace @fluid-org/fluid build-publish` (from any directory). Script bumps patch version, commits, tags `vX.Y.Z`, builds, stages website, and publishes to npm. After it succeeds, push manually: `git push && git push --tags`.

Auth: needs an npm automation token in `~/.npmrc` (`//registry.npmjs.org/:_authToken=...`) — bypasses 2FA.

## Issue lifecycle

Select issue from current milestone; branch, implement, test, PR, review and merge per `github` skill. After merging: update milestone report if issue is complete. See [issue-lifecycle.md](issue-lifecycle.md) for state machine and invariants.

## Milestone population

When all issues in milestone are done, paused, or awaiting decision, populate with 3–8 new issues:

1. **Planned issues first** — already prioritised by developer.
2. **Proposed issues** — prefer low-hanging fruit, thematic fit, test coverage.

Adding to milestone bumps Proposed → Planned. Can be done autonomously. Afterwards run `./script/check-project-integrity.sh` to verify invariants.

## Project board

- New issues: add to [project board](https://github.com/orgs/fluid-org/projects/1) (`script/issue-status.sh <issue> <Status>` adds an absent issue) and populate **Status** (usually `Proposed` for new work) and **Aspect**. Also set Type, labels (`implementation`, `testing`, `setup`, `documentation`), and milestone when appropriate.
- After any change to board state, verify project view at https://github.com/orgs/fluid-org/projects/1.
- Destructive mutations of board state: see [incidents.md](incidents.md).

## Tracking

- Commits by Claude are authored by the `rolyp-claude-bot` GitHub account; no `Co-Authored-By` trailer is added (it would produce a duplicate avatar on GitHub).
- Searchable via `git log --author=rolyp-claude-bot`.
- Issue comments record design decisions.

## Testing

Repo is a Yarn monorepo.

`yarn workspace @fluid-org/fluid test` runs the unit test suite via `fluid/test/Test.purs`'s `tests`, which defaults to `allTests`. To run a focused subset:

1. Edit `fluid/test/Test.purs`: change `tests = allTests` to `tests = scratchpad`.
2. Edit `scratchpad` to select cases. Whole suite: `scratchpad = second void <$> suite misc_cases (1 × false)`. Specific files: `scratchpad = filterSuite [ "lambda.fld", "self.fld" ] misc_cases suite` (use `illFormed_cases` and `illFormedSuite` for ill-formed).
3. `yarn workspace @fluid-org/fluid build && yarn workspace @fluid-org/fluid test`.
4. When done, revert `tests = allTests`. Leave `scratchpad` as-is; it's a working area.

Do not invent ad hoc CLI invocations of `fluid.mjs` to run individual `.fld` files — the test harness sets up paths, prelude, and reports errors in the same form tests assert against.

`yarn workspace @fluid-org/fluid test` doesn't exercise website fixtures. Run `./script/test-website-all.sh` from the repo root after changes that touch parsing, desugaring, evaluation, or well-formedness; before pushing a commit that completes a meaningful chunk of work; or whenever in doubt about wider impact. Don't run it on every change — it's slow (a few minutes per website).

## Pending manual tasks

When Claude encounters a missing permission or setting, note the specific change needed here and continue with available work.

None.

Completed tasks recorded in [docs/reports/completed-manual-tasks.md](../reports/completed-manual-tasks.md).
