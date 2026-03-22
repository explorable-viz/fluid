# Issue Lifecycle

## States

### Open — passive
Issues not currently being worked on.

- **Proposed**: candidate for future work; not yet prioritised.
- **Planned**: prioritised for inclusion in a milestone. Adding an issue to a milestone bumps it from Proposed to Planned.

### Open — active
Issues currently being worked on or blocked.

- **In Progress**: actively being worked on.
- **Paused**: was in progress but temporarily stopped (e.g. by human request).
- **Awaiting Decision**: blocked on a decision requiring human input.

### Closed
- **Done**: completed.
- **Rejected**: will not be done.

## Transitions

```
Proposed ──→ Planned ──→ In Progress ──→ Done
                              │
                              ├──→ Paused ──→ In Progress
                              │
                              └──→ Awaiting Decision ──→ In Progress
                                                     └──→ Paused

Any open state ──→ Rejected
```

- **Proposed → Planned**: issue is prioritised, or added to a milestone.
- **Planned → In Progress**: work begins.
- **In Progress → Paused**: temporarily stopped (e.g. waiting on unrelated work, human request).
- **In Progress → Awaiting Decision**: blocked on a decision; Claude adds a comment explaining the options.
- **Awaiting Decision → In Progress**: decision made, work resumes.
- **Awaiting Decision → Paused**: decision is to defer.
- **In Progress → Done**: work complete, PR merged.

## Invariants

Enforced by `script/check-project-integrity.sh`:

- Every open issue has a Status.
- No milestoned issue is Proposed (should be Planned or higher).
- Every active issue (In Progress, Paused, Awaiting Decision) has a milestone.
- In a closed milestone, all issues are Done or Rejected.

## Other statuses

- **Meeting**: used for non-issue items on the project board. Not part of the issue lifecycle and excluded from integrity checks.

## Milestone closure

Before closing a milestone:
1. All active issues (In Progress, Paused, Awaiting Decision) must be removed from the milestone.
2. Removed issues drop back to Planned (no milestone).
3. Only Done and Rejected issues remain in the closed milestone.
