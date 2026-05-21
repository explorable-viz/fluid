# Refactoring Observations

Notes for potential inclusion in the refactor-first paper.

## Seams (Feathers)

Michael Feathers' *Working Effectively with Legacy Code* introduces the concept of a **seam**: a place where behaviour can be altered without editing the code at that point. Like a seam in fabric — where two pieces join, and can be pulled apart without tearing either.

In a cross-language migration (JS → PureScript), extracting a JS function creates a seam at the call site. The extracted function can then be replaced with a PureScript implementation without touching the calling code. The seam is the composition point where the two languages meet.

## Scoped testing for faster iteration

Full test suites (all websites, both browsers) provide comprehensive regression coverage but slow iteration. During micro-refactoring, running only tests relevant to the current change ("scoped testing") gives faster feedback.

Scoping can be by website (test only `article`), by page (test only `convolution`), by viewport (desktop vs mobile), or by feature (only selection tests). The full suite runs before merging to milestone branch; scoped tests run during iteration.
