# Development Process

Prose, code, commits, issues, pull requests and board Status follow the `technical-authoring`, `coding-conventions` and `github` skills; branching, milestones, deployment and publishing the `release-cycle` skill.

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
