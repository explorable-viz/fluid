# Fluid Development Workflow

## Principles

### Transparency
Progress on the workflow itself, and design decisions made on individual issues, are recorded. Mechanisms to be determined (e.g. GitHub issues, commit messages, decision logs).

### Auditability
Easy reporting on development velocity, amount of Claude activity, amount of human intervention/approval needed, and other metrics that help evaluate the effectiveness of the workflow.

### Autonomy
Gradually build towards more autonomy in decision-making, new issue creation, and prioritisation -- but not at the expense of the other principles.

### Reliability
Continual investment in quality-preserving and quality-enhancing aspects of the workflow: test coverage, enforcing that Claude is following the workflow correctly, and catching regressions early.

### Incremental improvement
Continual improvement of the workflow itself, including documenting the workflow, identifying problems, and feeding them back into workflow changes.

## Process

Before writing code, prose, commits, issues or pull requests, load the `conventions:coding-conventions`, `conventions:technical-authoring` and `conventions:github` skills, from the `conventions@fluid-org` plugin ([fluid-org/claude-plugins](https://github.com/fluid-org/claude-plugins)), which Claude Code offers to install when the project is opened; load `conventions:spec-implementor` as well when working against the PurePy specification. Branching, milestones, deployment and publishing are in the `release-cycle` skill. Building and testing: [CONTRIBUTING.md](CONTRIBUTING.md). Issue state machine and invariants: [docs/workflow/issue-lifecycle.md](docs/workflow/issue-lifecycle.md).

[docs/workflow/milestone-report.md](docs/workflow/milestone-report.md) covers the milestone report format and generation process — read when closing a milestone or producing release notes.
