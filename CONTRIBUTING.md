# Contributing to Fluid

## Development setup

### Prerequisites

- [Node.js](https://nodejs.org/) >= 22
- Enable Yarn via [Corepack](https://nodejs.org/api/corepack.html): `corepack enable`
- git
- (Windows only) [Ubuntu WSL](https://ubuntu.com/desktop/wsl)

### Initial configuration

```bash
git clone git@github.com:explorable-viz/fluid.git
cd fluid
./script/setup/dev-setup.sh
yarn install
```

## Building

```bash
yarn workspace @explorable-viz/fluid build
```

This compiles PureScript, bundles the Fluid runtime, and produces `output-es/` (ES module output).

## Running websites locally

The websites are SvelteKit apps under `website/`. To run one in dev mode:

```bash
cd website/article
yarn dev
```

For a production-like preview:

```bash
yarn build && yarn preview
```

## Testing

### PureScript unit tests

```bash
yarn workspace @explorable-viz/fluid test
```

### Website tests (Puppeteer)

Tests all SvelteKit websites with Puppeteer (Chrome and Firefox):

```bash
./script/test-website-all.sh
```

Or test a single website:

```bash
cd website/article
yarn test
```

### Browser tests

```bash
yarn workspace @explorable-viz/fluid test-browser
```

Opens a browser window; check the JavaScript console for test results.

## Publishing to npm

From the monorepo root:

```bash
yarn workspace @explorable-viz/fluid build-publish
```

This bumps the patch version (via `npm version patch`), builds in production mode, stages the `article` website, publishes to npm, and pushes the version tag. Version tracks milestone (e.g. `0.12.x` for milestone `fluid 0.12`).

## VS Code

- Install the [PureScript IDE](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript) extension
- In extension settings, select `Add Npm Path`
- Avoid having PureScript installed globally

Windows users:
- Launch VS Code from the Ubuntu (WSL) terminal
- Install the WSL extension

## Workflow

See [CLAUDE.md](CLAUDE.md) for the development workflow, including branching strategy, issue lifecycle, and milestone management.
