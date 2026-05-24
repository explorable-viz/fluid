# Milestone: fluid 1.0

## Summary
- Issues completed: 1
- Issues remaining: 2 (#1510, #1512 — Planned; #1537 — Proposed)
- Date range: 2026-05-21 to ongoing

## Issues

### #1530: Sync to PurePy 0.9
- **Status**: Done
- **Commits**: ~93 (#1530-tagged on branch), majority Claude-authored
- **Files modified**: 208 (insertions 2333, deletions 1203)
- **Decision points**: human intervention
  - Definite-assignment: contexts (Γ), result type (Returns | Assigns Δ), override/merge ops; checkDA unifies prior `check` + `checkScope`.
  - Multi-clause `def` reconciliation: Fluid's overlap-rejection plus non-contiguous-clauses check is a strict generalisation of PurePy's distinct-names rule (overlap detection at desug, contiguity at recDefsFwd).
  - Match semantics: runtime fall-through to Pass; DA merges with `Assigns ∅` (no exhaustiveness check until PurePy defines match — tracked in pending design).
  - Implicit None via attribute grammar: `Clause` carries `a` annotation, `checkProgram` returns `S.Stmt TyResult`, post-validation `implicitNone` injects `return None` at function bodies whose TyResult is `Assigns _`.
  - Top-level program returning value (test infrastructure surface) split out as #1537.
- **Branch**: `1530-purepy-0.9-sync` → `fluid-1.0` (PR pending)

## Metrics
- Total commits on branch: 106 (93 tagged #1530)
- Claude-authored commits (Co-Authored-By trailer): 47+
- Issues completed autonomously: 0
- Issues requiring human intervention: 1 (#1530)
