# fluid 1.0

## Features
- Sync to PurePy 0.12, 0.16, 0.18 and 0.22; checker passes the PurePy 0.22 conformance suite, apart from two extensions (#1530, #1558, #1589, #1612)
- Pythonic ternary conditional (#1415)
- `NoneType` (#1534)
- Modules as objects (#1436)
- Modules with primitives (#1437)
- Dataclasses and static resolution of module members (#201)
- Improved pattern-matching (#1240)
- Rest of record matched in record pattern (#755)
- Sequential pattern-matching for function clauses (#1239)
- `+` for string concatenation (#1479)
- `group_by` (#927)
- Dictionary comprehensions (#954)
- Structural equality for non-primitives (#1435)
- Builtins `range`, `sum`, `min`, `max`, `abs`, `zip`, `reversed`, `map` and `filter` (#1570)
- Package directories detected in browser (#1555)

## Bug fixes
- `.x` rejected on dictionaries (#1548)
- Intermediate ENOENT/404 errors in source path resolution (#1461)
- Dependence on `this` (#1411)
- Intermediate matrices picked out by forward slicing (#1288)

## Breaking changes
- `Bool` aligned with Python `bool` (#1546)
- `Maybe` aligned with `Optional` (#1547)
- Lists as flat sequences (#1560)
- Python-compatible names for standard library functions (#1510)
- Prelude imported explicitly; `builtins` the only implicit module (#1608)

## Other
- Backwards desugaring removed (#1245)
- `GaloisConnection` replaced by `ConjugatePair` (#1038)
- Dependence graph redesign (#1554)
- Compliance tested against the PurePy conformance suite, installed from a `purepy` release (#1556)
