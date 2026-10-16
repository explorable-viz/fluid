# Milestone: fluid 1.0

## Summary
- Issues completed: 31
- Issues remaining: 0 (#1610, #1604, #1537, #1431 and #1413 moved to fluid 1.1)
- Date range: 2026-05-21 to 2026-10-16

## Issues

### #1245: Remove backwards desugaring
- **Status**: Done
- **Closed**: 2026-05-21
- **Commits**: not on an issue branch merged since 2026-08-04

### #1415: Pythonic ternary conditional
- **Status**: Done
- **Closed**: 2026-05-22
- **Commits**: not on an issue branch merged since 2026-08-04

### #1534: `NoneType`
- **Status**: Done
- **Closed**: 2026-05-23
- **Commits**: not on an issue branch merged since 2026-08-04

### #1038: Replace `GaloisConnection` by `ConjugatePair`
- **Status**: Done
- **Closed**: 2026-06-28
- **Commits**: not on an issue branch merged since 2026-08-04

### #1436: Modules as objects
- **Status**: Done
- **Closed**: 2026-07-19
- **Commits**: not on an issue branch merged since 2026-08-04

### #1548: Reject `.x` on dictionaries
- **Status**: Done
- **Closed**: 2026-07-19
- **Commits**: not on an issue branch merged since 2026-08-04

### #201: Dataclasses and static resolution of module members
- **Status**: Done
- **Closed**: 2026-07-24
- **Commits**: not on an issue branch merged since 2026-08-04

### #755: Match rest of record in record pattern
- **Status**: Done
- **Closed**: 2026-09-22
- **Commits**: not on an issue branch merged since 2026-08-04

### #1239: Sequential pattern-matching for function clauses
- **Status**: Done
- **Closed**: 2026-09-22
- **Commits**: not on an issue branch merged since 2026-08-04

### #1240: Improved pattern-matching
- **Status**: Done
- **Closed**: 2026-09-22
- **Commits**: not on an issue branch merged since 2026-08-04

### #927: `group_by`
- **Status**: Done
- **Closed**: 2026-09-27
- **Commits**: not on an issue branch merged since 2026-08-04

### #954: Dictionary comprehensions
- **Status**: Done
- **Closed**: 2026-09-27
- **Commits**: not on an issue branch merged since 2026-08-04

### #1435: Structural equality for non-primitives
- **Status**: Done
- **Closed**: 2026-09-27
- **Commits**: not on an issue branch merged since 2026-08-04

### #1437: Modules with primitives
- **Status**: Done
- **Closed**: 2026-09-27
- **Commits**: not on an issue branch merged since 2026-08-04

### #1461: Intermediate ENOENT/404 errors in `fluidSrcPath` resolution
- **Status**: Done
- **Closed**: 2026-09-27
- **Commits**: not on an issue branch merged since 2026-08-04

### #1479: `+` for string concatenation
- **Status**: Done
- **Closed**: 2026-09-27
- **Commits**: not on an issue branch merged since 2026-08-04

### #1510: Python-compatible names for standard library functions
- **Status**: Done
- **Closed**: 2026-09-27
- **Commits**: not on an issue branch merged since 2026-08-04

### #1546: Align `Bool` with Python `bool`
- **Status**: Done
- **Closed**: 2026-09-27
- **Commits**: not on an issue branch merged since 2026-08-04

### #1555: Detect package directories in browser
- **Status**: Done
- **Closed**: 2026-09-27
- **Commits**: 5 total, 5 by Claude, 0 by human
- **Files modified**: 22
- **Pull requests**: #1573

### #1558: Sync to PurePy 0.16 (without full type system)
- **Status**: Done
- **Closed**: 2026-09-27
- **Commits**: not on an issue branch merged since 2026-08-04

### #1560: Lists as flat sequences
- **Status**: Done
- **Closed**: 2026-09-27
- **Commits**: not on an issue branch merged since 2026-08-04

### #1530: Sync to PurePy 0.12
- **Status**: Done
- **Closed**: 2026-09-29
- **Commits**: not on an issue branch merged since 2026-08-04
- **Decision points**: human intervention
  - Definite-assignment: contexts (Γ), result type (Returns | Assigns Δ), override/merge ops; checkDA unifies prior `check` + `checkScope`.
  - Multi-clause `def` reconciliation: Fluid's overlap-rejection plus non-contiguous-clauses check is a strict generalisation of PurePy's distinct-names rule (overlap detection at desug, contiguity at recDefsFwd).
  - Match semantics: runtime fall-through to Pass; DA merges with `Assigns ∅` (no exhaustiveness check until PurePy defines match — tracked in pending design).
  - Implicit None via attribute grammar: `Clause` carries `a` annotation, `checkProgram` returns `S.Stmt TyResult`, post-validation `implicitNone` injects `return None` at function bodies whose TyResult is `Assigns _`.
  - Top-level program returning value (test infrastructure surface) split out as #1537.

### #1547: Align `Maybe` with `Optional`
- **Status**: Done
- **Closed**: 2026-09-29
- **Commits**: 1 total, 1 by Claude, 0 by human
- **Files modified**: 18
- **Pull requests**: #1578

### #1411: Depending on `this` broken
- **Status**: Done
- **Closed**: 2026-10-06
- **Commits**: not on an issue branch merged since 2026-08-04

### #1554: Dependence graph redesign
- **Status**: Done
- **Closed**: 2026-10-06
- **Commits**: 179 total, 179 by Claude, 0 by human
- **Files modified**: 198
- **Pull requests**: #1579, #1580, #1583, #1584, #1586, #1587, #1588

### #1556: Test compliance against pure-py-spec repo
- **Status**: Done
- **Closed**: 2026-10-07
- **Commits**: 24 total, 24 by Claude, 0 by human
- **Files modified**: 125
- **Pull requests**: #1590

### #1570: Builtins `range`, `sum`, `min`, `max`, `abs`, `zip`, `reversed`, `map` and `filter`
- **Status**: Done
- **Closed**: 2026-10-08
- **Commits**: 6 total, 6 by Claude, 0 by human
- **Files modified**: 14
- **Pull requests**: #1595

### #1288: Forward slicing doesn't pick out intermediate matrices
- **Status**: Done
- **Closed**: 2026-10-15
- **Commits**: not on an issue branch merged since 2026-08-04

### #1589: Sync to PurePy 0.18
- **Status**: Done
- **Closed**: 2026-10-15
- **Commits**: 212 total, 212 by Claude, 0 by human
- **Files modified**: 583
- **Pull requests**: #1592, #1593, #1594, #1596, #1598, #1599, #1600, #1601, #1602, #1603, #1606, #1609, #1611

### #1608: Import prelude explicitly
- **Status**: Done
- **Closed**: 2026-10-16
- **Commits**: 1 total, 1 by Claude, 0 by human
- **Files modified**: 56
- **Pull requests**: #1617

### #1612: Sync to PurePy 0.22
- **Status**: Done
- **Closed**: 2026-10-16
- **Commits**: 38 total, 38 by Claude, 0 by human
- **Files modified**: 43
- **Pull requests**: #1615, #1616

## Metrics
- Total commits since branch point with develop (2026-08-04): 649
- Claude-authored commits: 649 (100%)
- Human-authored commits: 0 (0%)
- Total files modified: 990
