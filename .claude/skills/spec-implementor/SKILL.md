---
name: spec-implementor
description: Discipline for keeping the implementation in one-to-one correspondence with the spec. Use whenever syncing implementation with spec, or reviewing either against the other.
---

# Spec implementor

Behavioral agreement is not the finish line. A sync is complete only when spec
and implementation are in one-to-one correspondence, checked in both directions.
Walking the rules and asking "does the implementation compute the right verdict"
filters out every divergence that changes no verdict: misnamed functions,
displaced terminology, dead helpers, stale taxonomy. Only the inventory
comparison below catches those, so run it even when all tests pass.

## Method

Build two inventories: spec constructs (judgements, rules, metafunctions,
operators, technical terms) and implementation identifiers (functions, classes,
constants, test taxonomy). Match by name. Every unmatched item, in either
direction, gets exactly one of:

- **rename** to the spec's name (the spec is the reference);
- **delete** as dead code or a displaced term;
- **excuse** as a true implementation detail with no spec counterpart (e.g.
  memoizing module contexts). Record the excuse.

Watch for the same name meaning different things on the two sides (override
versus extend). Diagnostics may refine the spec's single failure mode into
specific reasons; name each after the prohibition it enforces.

## Testing

A test is a small program with an expected outcome: accepted, rejected, or
particular output when run. Nothing else is observable.

- **Differential**: every program also runs under the reference implementation;
  the test's category states the required relationship (well-formed: both
  accept; ill-formed: both reject; prohibited: reference accepts, we reject). A
  misfiled test is itself a failure.
- **Metamorphic**: where the spec says a change is harmless (commutative,
  idempotent), apply it to a passing program (swap sibling imports, repeat an
  import); verdict and output must not change.
- **Minimal pairs**: for each condition in a rule, two programs differing as
  little as possible, one just meeting it and one just missing it. Error
  messages are not the unit of coverage.
- **Whole-program conditions**: test both sides of the boundary (a self-import
  and a two-module cycle against a diamond).
- The spec's worked examples and counterexamples are a test plan already
  written. Audit the suite by mutation: weaken the implementation and confirm a
  test fails.

## Defensive coding

Where an invariant makes a case impossible, fail loudly there with `definitely`
rather than returning a default. A silent default hides a later breakage far
from its cause.

## Comments

Avoid comments unless they have substantial marginal value over the code.
Omit historical commentary: state what the code is or does for a reader who
meets it fresh, not the motivation for it, what it replaced, or the problem it
once fixed. That history belongs in the commit message, not the code.
Grammatically, comments should be uninflected: a function that binds imported
members gets "bind imported members", rather than "binds". PR bodies likewise:
only information of clear marginal value over the diff and commit history.
