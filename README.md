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
- `yarn build` and then `yarn test-all`

#### Running tests in browser
- As per command-line tests above, but run `yarn test-browser`
- Hit Debug in the browser window that opens, and then open Developer Tools or equivalent

#### Running the fluid.org website locally
- `yarn build`
- `yarn serve fluid-org`
- Open a browser at the served URL (usually `127.0.0.1:8080`)

#### Run Puppeteer tests for page Y of website X
Rebuild with `puppeteerTests.headless` set to `false` to run in browser. Then:
- `yarn bundle-website X`
- `./script/test-page.sh X X.Y`
