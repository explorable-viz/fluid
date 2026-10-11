---
name: spec-implementor
description: Discipline for keeping the checker (and other implementations) in one-to-one correspondence with the spec. Use whenever syncing implementation with spec, or reviewing either against the other.
---

# Spec implementor

Load the coding-conventions skill first; it holds the comment, identifier and commit rules shared
with other implementations.

A sync is complete only when the spec and the implementation are in one-to-one
correspondence, checked in both directions, not when their verdicts agree.

## Method

1. Build two inventories: spec constructs (judgements, rules, metafunctions,
   operators, technical terms, section headings) and implementation identifiers
   (functions, classes, constants, test taxonomy, docs).
2. Match by name. Every unmatched item, in either direction, gets one of:
   - **rename** to the spec's name (the spec is the reference; `wraps` in the
     spec means a function called `wraps` in the checker);
   - **delete** as dead code or a displaced term (unused macros, stale labels,
     helpers with no callers).
3. Watch for the same name meaning different things on the two sides (the spec
   distinguishes override from extend; the implementation must not use one
   name for both).
4. Diagnostics may refine the spec's single failure mode into specific reasons;
   that is a sanctioned refinement. Name each reason after the prohibition or
   premise it enforces, and word the message as the spec does where it
   describes the failure.
5. Run the inventory comparison even when all tests pass; verdict checks miss
   misnamed functions, displaced terminology, dead helpers and stale taxonomy.

## Sequential check against the spec

Check in both directions, in order, rather than jumping to the parts you suspect.

Start with the specification and read it top down. For each judgement, rule and metafunction, find the
definition in the implementation that carries the same name and serves the same purpose.

Then read the implementation the same way. For each definition, find the construct in the
specification of the same or a similar name that it implements. A definition that implements no single
construct is renamed, moved to the construct it belongs to, or deleted.

Act at the point the walk reaches, not before, so that whatever breaks is traceable to the one
definition you changed.

## Order of work

Spec first, then tests, then implementation. When the spec changes, write or
adjust the failing test before touching the checker.

## Implementation style

Write in the functional subset of the host language: no reassignment of a variable once bound, no
in-place mutation, no loops that accumulate into a variable. Build each value in one expression, by
comprehension, recursion or a fold, so that a definition reads like the rule it implements.

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
- **Spec prose**: every worked example, counterexample and justifying sentence
  describes a program; each should exist as a test.
- The spec's corpus is run whole by the differential harness, not copied into
  the suite, which holds only programs without a corpus counterpart.
- Use programs large enough that recursive rules actually recurse; a
  one-level example can pass for the wrong reason.
- Audit the suite by mutation: weaken the implementation deliberately and
  confirm a test fails.

## Tooling

Point checkers at whole directories rather than named files, and occasionally
verify that every checker still sees everything it should.
