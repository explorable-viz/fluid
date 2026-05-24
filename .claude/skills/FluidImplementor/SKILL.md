---
name: FluidImplementor
description: Conventions for implementing changes to the Fluid language and runtime, including how to run a focused subset of the test suite.
---

# FluidImplementor

Working notes for implementing changes to the Fluid language and runtime.

## Style

- Minimal. State the thing; stop.

## Committing

- Commit at natural checkpoints — particularly structural/refactoring steps that pass tests without changing behaviour. Don't pile a refactor on top of a feature in one commit.

## Running a single test (or a focused subset)

The `yarn test` runner always invokes `tests` in `fluid/test/Test.purs`. By
default `tests = allTests`. To run a focused subset:

1. Edit `fluid/test/Test.purs`: change `tests = allTests` to `tests = scratchpad`.
2. Edit `scratchpad` to select the cases you want. Two ways:
   - **Whole suite:** `scratchpad = second void <$> suite misc_cases (1 × false)`
     (replace `misc_cases` with any imported `*_cases` array).
   - **Specific files:** use `filterSuite`:
     `scratchpad = filterSuite [ "lambda.fld", "self.fld" ] misc_cases suite`
     (for ill-formed cases pass `illFormed_cases` and `illFormedSuite`).
3. Run `yarn build && yarn test` as usual.
4. When done, **only revert `tests = allTests`**. Leave `scratchpad` as-is; it's
   a working area, not something to keep clean.

Do not invent ad hoc CLI invocations of `fluid.mjs` to run individual `.fld`
files — the test harness is the canonical way to exercise programs end-to-end
(it sets up paths, prelude, and reports errors in the same form the tests
assert against).

## Filing issues

Issue titles should be **noun phrases** (or similar) succinctly
characterising the outcome — not deontic statements.

- Good: "Top-level program returns sysexit status"
- Bad: "Top-level program should return a sysexit status"
- Good: "Match exhaustiveness check"
- Bad: "Add a match exhaustiveness check"

"Fluid" is redundant in titles (the repo is Fluid) — omit it.

When an issue references other issues or external resources, add a **See also** paragraph at the end with a bullet list of links. When linking to another issue, write just the bare `#N` reference — GitHub renders the title inline.

Add new issues to the **Fluid** project (number 1, owner `fluid-org`) and populate **Status** (usually `Proposed`) and **Aspect**. The standard issue fields (Type, labels, milestone) should also be filled when appropriate.

## Website tests

`yarn test` doesn't exercise the website fixtures. Run `./script/test-website-all.sh`
from the repo root to build each website and drive it through Puppeteer.

Don't run this on every change — it's slow (a few minutes per website) and most
unit-level work is already covered by `yarn test`. Do run it:
- After changes that touch parsing, desugaring, evaluation, or the well-formedness
  checker (i.e. anything that could affect how `.fld` programs are processed).
- Before pushing a feature commit that completes a meaningful chunk of work.
- Whenever in doubt about whether a change has wider impact.
