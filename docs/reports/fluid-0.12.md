# Milestone: fluid 0.12

## Summary
- Issues completed: 8
- Issues remaining: 2 (#1492, #1427)
- Date range: 2026-02-04 to ongoing

## Issues

### #1489: Svelte components for Fluid web pages
- **Status**: Done
- **Commits**: 84 total, 84 by Claude
- **Files modified**: ~837 (excluding output dirs)
- **Decision points**: human-driven (initial Svelte migration design)
- **Branch**: `Svelte-components` → develop (PR #1494)

### #1491: Migrate `article` and `literate-execution` to SvelteKit
- **Status**: Done
- **Commits**: included in #1489 branch (Claude-attributed)
- **Decision points**: human intervention
  - CSS scoping: view-styles.css initially scoped under `#fig`, broke data pane styling; reverted to global

### #1485: Matrix index resolution out by 1 on mouseover
- **Status**: Done
- **Commits**: 2 total, 0 by Claude
- **Files modified**: 2
- **Decision points**: autonomous
- **Branch**: `convolution-mouseover-bug` → develop (PR #1486)

### #1459: Unary `not`
- **Status**: Done
- **Commits**: 9 total, 0 by Claude
- **Files modified**: 7
- **Decision points**: autonomous
- **Branch**: `op-table-fixities` → develop (PR #1493)

### #1434: Migrate remaining website tests to JavaScript
- **Status**: Done
- **Commits**: 2 on dedicated branch, all by Claude (bulk of work done on `Svelte-components` branch before workflow conventions adopted)
- **Files modified**: ~20 (webtest-lib.mjs, test files, PureScript test cleanup)
- **Decision points**: human intervention
  - Chose plain JS over PureScript Aff-based test utilities
  - CSS regression test approach discussed; `checkComputedStyle` added
- **Branch**: `1434-migrate-PureScript-website-tests` → develop (PR #1495)

### #1385: PureScript website tests ignored on Ubuntu
- **Status**: Done (resolved by #1434)
- **Commits**: 0 (no dedicated work; resolved as side effect of #1434)
- **Files modified**: 0
- **Decision points**: autonomous (identified as already resolved, closed with comment)

### #1484: `README.md`: end-user instead of dev setup
- **Status**: Done
- **Commits**: ~5, all by Claude
- **Files modified**: 3 (README.md, CONTRIBUTING.md, script/setup/install.sh)
- **Decision points**: human intervention
  - curl one-liner vs manual instructions; install from npm not clone
  - VS Code file association instead of MagicPython extension

### #1490: Website revamp
- **Status**: Done
- **Commits**: ~22, all by Claude
- **Files modified**: ~15 (deploy workflow, landing page, static examples, obsolete scripts)
- **Decision points**: human intervention
  - Staging branch deployment flow (develop → staging → release)
  - PurePy content and code examples chosen collaboratively
  - CodeMirror component reuse instead of highlight.js

## Metrics
- Total commits: ~124
- Claude-authored commits: ~112 (90%)
- Human-authored commits: ~12 (10%)
- Issues completed autonomously: 3 (38%) — #1485, #1459, #1385
- Issues requiring human intervention: 5 (62%) — #1489, #1491, #1434, #1484, #1490

## Notes
- The `Co-Authored-By: Claude` convention was adopted partway through this milestone. Earlier commits lack this attribution. Future milestones will have accurate tracking from the start.
- The branching workflow (issue → milestone → develop) was also adopted mid-milestone. Earlier issues went directly to develop.
