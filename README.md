## Fluid: Language-integrated data provenance

Fluid is a pure functional programming language with a provenance-tracking runtime and [Pythonic syntax](https://github.com/pure-py/pure-py-spec). Fluid is implemented in PureScript and runs in the browser.

[![build](https://github.com/explorable-viz/fluid/actions/workflows/deploy.yml/badge.svg)](https://github.com/explorable-viz/fluid/actions/workflows/deploy.yml)

Website: [f.luid.org](https://f.luid.org)

## Getting started

### Prerequisites

- [Node.js](https://nodejs.org/) >= 22
- Enable Yarn via [Corepack](https://nodejs.org/api/corepack.html): `corepack enable`

### Creating a Fluid project

```bash
mkdir my-project && cd my-project
yarn init -y
yarn add @explorable-viz/fluid
```

For an example of a Fluid website built with SvelteKit, see the [`article`](website/article) directory in this repo, or the [fluid-article](https://github.com/explorable-viz/fluid-article) template repository.

### Running programs from the command line

Fluid programs (`.fld` files) can be evaluated from the command line:

```
npx fluid evaluate -f <path>
```

The path is relative and should not include the `.fld` extension:

```
npx fluid evaluate -f example/range
```

### VS Code setup

Install the [MagicPython](https://marketplace.visualstudio.com/items?itemName=MagicStack.MagicPython) extension for Python syntax highlighting of `.fld` files.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for developer setup, building, testing, and the development workflow.
