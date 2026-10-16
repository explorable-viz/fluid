# fluid 1.0

## Features
- Pythonic ternary conditional (#1415)
- `NoneType` (#1534)
- Modules as objects (#1436)
- Modules with primitives (#1437)
- Dataclasses and static resolution of module members (#201)
- Improved pattern-matching (#1240)
- Match rest of record in record pattern (#755)
- Sequential pattern-matching for function clauses (#1239)
- `+` for string concatenation (#1479)
- `group_by` (#927)
- Dictionary comprehensions (#954)
- Structural equality for non-primitives (#1435)
- Builtins `range`, `sum`, `min`, `max`, `abs`, `zip`, `reversed`, `map` and `filter` (#1570)
- Python-compatible names for standard library functions (#1510)

## Bug fixes
- Depending on `this` broken (#1411)
- Forward slicing doesn't pick out intermediate matrices (#1288)
- Intermediate ENOENT/404 errors in `fluidSrcPath` resolution (#1461)
- Detect package directories in browser (#1555)

## Other
- Test compliance against pure-py-spec repo (#1556)
- Align `Bool` with Python `bool` (#1546)
- Align `Maybe` with `Optional` (#1547)
- Lists as flat sequences (#1560)
- Import prelude explicitly (#1608)
- Reject `.x` on dictionaries (#1548)
- Remove backwards desugaring (#1245)
- Replace `GaloisConnection` by `ConjugatePair` (#1038)
- Dependence graph redesign (#1554)
