# Completed Manual Tasks

- Add `project` scope to the GitHub PAT so Claude can update project board statuses (needed for the escalation workflow: moving issues to "Awaiting Decision")
- Add "Awaiting Decision" status option to the GitHub Project board (created via GraphQL API once token scope was granted)
- Rename milestone "fluid-org 1.0" to "fluid 0.13"
- Create milestone branch `fluid-0.12` from develop
- Create milestone branch `fluid-0.13` from develop
- Add `PROJECT_PAT` secret to the repo (a PAT with `project` scope) for the `check-project-integrity` GitHub Actions workflow
