# fluid 1.0

## Features
- Pythonic ternary conditional (#1415)
- `NoneType` in place of `Unit` (#1534)
- Dataclasses and static resolution of module members (#201)
- Modules with primitives (#1437)
- `+` for string concatenation (#1479)
- Dictionary comprehensions (#954)
- `Optional` in place of `Maybe` (#1547)
- Lists as flat sequences (#1560)
- Python-compatible names for standard library functions (#1510)
- Prelude imported explicitly (#1608)

## Bug fixes
- `.x` rejected on dictionaries (#1548)
- Spurious errors from source path resolution (#1461)
- Package directories detected in browser (#1555)

## Other
- Conformance to PurePy 0.22, apart from two extensions (#1530, #1558, #1589, #1612, #1556)
- Dependence graph redesign (#1554)
