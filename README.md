## Fluid: Language-integrated data provenance

Fluid is an experimental programming language which integrates a bidirectional dynamic analysis to connect outputs to data sources in a fine-grained way. Fluid is implemented in PureScript and runs in the browser.

[![develop](https://github.com/explorable-viz/fluid/actions/workflows/develop.yml/badge.svg)](https://github.com/explorable-viz/fluid/actions/workflows/develop.yml)
[![GitHub pages](https://github.com/explorable-viz/fluid/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/explorable-viz/fluid/actions/workflows/pages/pages-build-deployment)

## Installation

### Software required
- git
- Node.js >=14.0.0
- yarn >= 1.22

### For Windows Users

- [Ubuntu WSL](https://ubuntu.com/desktop/wsl)

### Building

- Clone the repository (for Windows users, do this under the Ubuntu WSL)
- Run `./script/setup/dev-setup.sh` from the top-level directory
- Run `yarn build`

#### Running tests on command line

After building, tests can be run from the command line via `yarn test-all`

#### Running tests in browser
- As per command-line tests above, but run `yarn test-browser`, which opens
a browser window.
- To observe the status of tests, click `Debug` in the browser window, and then open the JavaScript Console for your browser (e.g., via the Developer Tools).

#### Running the fluid.org website locally

(Assumes you have already run `yarn build`)

- `yarn serve fluid-org` (you may be prompted to proceed: type `y`).
- Open a browser at the served URL (usually `127.0.0.1:8080`)

#### Run Puppeteer tests for page Y of website X

Rebuild with `puppeteerTests.headless` set to `false` to run in browser. Then:
- `yarn bundle-website X`
- `./script/test-page.sh X X.Y`

## Development via VS Code

The following are some notes on developing Fluid using VS Code.

- Avoid having PureScript installed globally
- Install the PureScript IDE extension
- In the PureScript IDE extension settings, select `Add Npm Path`

- For Windows users:
	- Launch VSCode through Ubuntu (WSL) terminal
	- Install WSL extension in VSCode
