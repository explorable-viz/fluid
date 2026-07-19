---
name: spec-implementor
description: Discipline for keeping the implementation in one-to-one correspondence with the spec. Use whenever syncing implementation with spec, or reviewing either against the other.
---

# Spec implementor

Behavioral agreement is not the finish line. A sync is complete only when the
spec and the implementation are in one-to-one correspondence, checked in both
directions.

## Method

1. Build two inventories: spec constructs (judgements, rules, metafunctions,
   operators, technical terms, section headings) and implementation identifiers
   (functions, classes, constants, test taxonomy, docs).
2. Match by name. Every unmatched item, in either direction, gets exactly one
   of:
   - **rename** to the spec's name (the spec is the reference; `wraps` in the
     spec means a function called `wraps` in the implementation);
   - **delete** as dead code or a displaced term (unused macros, stale labels,
     helpers with no callers);
   - **excuse** as a true implementation detail with no spec-level counterpart
     (e.g. memoizing module contexts, which is caching for idempotent
     checking). Record the excuse.
3. Watch for the same name meaning different things on the two sides (the spec
   distinguishes override from extend; the implementation must not use one
   name for both).
4. Diagnostics may refine the spec's single failure mode into specific reasons;
   that is a sanctioned refinement. Name each reason after the prohibition or
   premise it enforces.

## Order of work

Spec first, then tests, then implementation. When the spec changes, write or
adjust the failing test before touching the implementation.

## Comments

Avoid comments unless they have substantial marginal value over the code.
Grammatically, comments should be uninflected: a function that binds imported
members might have the comment "bind imported members", rather than "binds".

## Testing

A test is a small program with an expected outcome: accepted, rejected, or
producing particular output when run. Nothing else is observable, so nothing
else is tested.

- **Differential**: every program also runs under the reference
  implementation, and the test's category states the required relationship
  (well-formed: both accept; ill-formed: both reject; prohibited: reference
  accepts, we reject). A misfiled test is itself a failure.
- **Metamorphic**: when the spec says a change is harmless (a commutative or
  idempotent operation), apply it to a passing program: swap sibling imports,
  repeat an import. Verdict and output must not change.
- **Differing pairs**: when the spec says a difference matters (a biased
  operation, a law noted not to hold), the two programs must come out
  differently; the spec's own counterexample is usually the pair to use.
- **Minimal pairs**: for each condition in a rule, two programs differing as
  little as possible, one just meeting it and one just missing it. Error
  messages are not the unit of coverage; one message can arise from several
  unrelated situations, each needing its own pair.
- **Whole-program conditions**: test both sides of the boundary, the smallest
  broken case and the largest legal case that resembles one (a self-import
  and a two-module cycle against a diamond).
- **The spec's prose is a test plan the authors already wrote**: every worked
  example, counterexample, and justifying sentence describes a program; each
  should exist as a test.
- Use programs large enough that recursive rules actually recurse; a
  one-level example can pass for the wrong reason.
- Audit the suite by mutation: deliberately weaken the implementation and
  confirm a test fails; a green suite has a hole.

## Why behavioral auditing misses this

Walking the spec's rules and asking "does the implementation compute the right
verdict" filters out every divergence that changes no verdict: misnamed
functions, displaced terminology, dead helpers, stale taxonomy. Those are
visible only to the inventory comparison above, so run it even when all tests
pass.

## Tooling

A list of things to check goes stale silently: point checkers at whole
directories rather than named files, and occasionally verify that every
checker still sees everything it should.
