## Fluid

A pure functional dialect of Python, based on [PurePy](https://github.com/pure-py/pure-py-spec), with a dependency-tracking runtime for creating interactive outputs linked to data.

[![build](https://github.com/explorable-viz/fluid/actions/workflows/deploy.yml/badge.svg)](https://github.com/explorable-viz/fluid/actions/workflows/deploy.yml)

## Getting started

Requires [Node.js](https://nodejs.org/) >= 22. In a new project directory:

```bash
curl -fsSL https://raw.githubusercontent.com/explorable-viz/fluid/release/script/setup/install.sh | bash
```

### VS Code

To get Python syntax highlighting for `.fld` files, add to `settings.json`:

```json
"files.associations": {
    "*.fld": "python"
}
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for developer setup, building, testing, and the development workflow.
