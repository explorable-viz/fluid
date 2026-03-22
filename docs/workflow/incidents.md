# Workflow Incidents

## 2026-03-22: Project board status assignments lost

**What happened**: Using the `updateProjectV2Field` GraphQL mutation to add an "Awaiting Decision" status option replaced all existing status options with new IDs. This orphaned all existing status assignments across 1058 project items.

**Root cause**: The GitHub Projects API requires all options to be specified when updating a single-select field — there is no "append option" operation. The mutation created new option IDs even for options with the same names, breaking all existing assignments.

**Impact**: All project item statuses (In Progress, Paused, Done, etc.) were lost.

**Lesson**: Destructive mutations on shared project state must be flagged for human approval, even when the token has permission. This falls under the same category as force-pushing or dropping database tables.

**Action**: Added to escalation criteria in process doc.
