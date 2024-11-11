## Fluid: Language-integrated data provenance

Fluid is an experimental programming language which integrates a bidirectional dynamic analysis to connect outputs to data sources in a fine-grained way. Fluid is implemented in PureScript and runs in the browser.

[![develop](https://github.com/explorable-viz/fluid/actions/workflows/develop.yml/badge.svg)](https://github.com/explorable-viz/fluid/actions/workflows/develop.yml)
[![GitHub pages](https://github.com/explorable-viz/fluid/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/explorable-viz/fluid/actions/workflows/pages/pages-build-deployment)

## Installation

### Software required
- Windows users: Ubuntu (WSL)
- git
- Node.js >=14.0.0
- yarn

### For Windows Users
- Clone the repository under Ubuntu WSL

### Notes
- Run `script/setup/dev-setup.sh` after cloning repository
- Avoid having PureScript installed globally
- `Add Npm Path` is selected in PureScript IDE extension settings
- VSCode for Windows users:
	- Launch VSCode through Ubuntu (WSL) terminal
	- Install WSL extension in VSCode

#### Running tests on command line
- `yarn build` and then `yarn test-all`

#### Running tests in browser
- As per command-line tests above, but run `yarn test-browser`
- Hit Debug in the browser window that opens, and then open Developer Tools or equivalent

#### Running web app
- `yarn build`
- `yarn serve fluid-org`
- Open a browser at the served URL (usually `127.0.0.1:8080`)
