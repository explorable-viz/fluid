# fluid 1.0

## Features
- Pythonic ternary conditional (#1415)
- `NoneType` (#1534)
- Modules as objects, with primitives (#1436, #1437)
- Dataclasses and static resolution of module members (#201)
- Improved pattern-matching (#1240, #755, #1239)
- `+` for string concatenation (#1479)
- `group_by` (#927)
- Dictionary comprehensions (#954)
- Structural equality for non-primitives (#1435)
- Builtins `range`, `sum`, `min`, `max`, `abs`, `zip`, `reversed`, `map` and `filter` (#1570)
- `bool`, `Optional` and flat lists, following Python (#1546, #1547, #1560)
- Python-compatible names for standard library functions (#1510)
- Prelude imported explicitly (#1608)

## Bug fixes
- `.x` rejected on dictionaries (#1548)
- Source path resolution in browser (#1461, #1555)
- Dependence on `this` (#1411)
- Intermediate matrices in forward slicing (#1288)

## Other
- Conformance to PurePy 0.22, apart from two extensions (#1530, #1558, #1589, #1612, #1556)
- Dependence graph redesign (#1554)
