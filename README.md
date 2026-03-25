## Fluid: Language-integrated data provenance

Fluid is a pure functional programming language, with a provenance-tracking runtime and Pythonic syntax. Fluid is implemented in PureScript and runs in the browser.

[![develop](https://github.com/explorable-viz/fluid/actions/workflows/develop.yml/badge.svg)](https://github.com/explorable-viz/fluid/actions/workflows/develop.yml)
[![GitHub pages](https://github.com/explorable-viz/fluid/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/explorable-viz/fluid/actions/workflows/pages/pages-build-deployment)

## End-user setup

### Software required
- Node.js >=18.0.0
- yarn >= 1.22

### Initial configuration

Building a Fluid website usually involves building a Node application:

- `yarn add @explorable-viz/fluid`
- `yarn install` to install Node dependencies
- `yarn install-website article` to copy example article website from `@exploreable-viz/fluid`
- Add `dist/` and `website/` folders to `.gitignore` 

### Bundling and serving website
- `yarn bundle-website $WEBSITE_NAME` to bundle website to `dist/$WEBSITE_NAME`
- `npx http-serve dist/$WEBSITE_NAME -c-1` to serve website at localhost

## Development setup

### Additional software required
- git
- (Windows only) [Ubuntu WSL](https://ubuntu.com/desktop/wsl)

### Initial configuration

- Clone repository (for Windows users, under Ubuntu WSL)
- Run `./script/setup/dev-setup.sh` from the top-level directory
- `yarn install` to install Node dependencies

## Use

- `yarn build` to build interpreter

### Running programs from the command line

Fluid examples in the `dist/fluid/fluid` can be evaluated from the command line as follows
(from the top-level directory):

```
npx fluid evaluate -f <path>
```
Note that the path is relative and should not include the `.fld` extension, e.g. for the `range.fld` example:
```
% npx fluid evaluate -f example/range
((0, 0) : ((0, 1) : ((1, 0) : ((1, 1) : []))))
Success
```

### Running websites locally (as part of Fluid development)
As an example, to build and run the website `literate-execution`:
- `yarn build` to ensure Fluid source code has been compiled (can be skipped on subsequent runs)
- `yarn bundle-website literate-execution` (can be skipped if the website being run is `fluid-org`)
- `yarn serve literate-execution` (you may be prompted to proceed; press `y`)
- Open a browser to the served URL (defaults to `127.0.0.1:8080`)

Note: `yarn bundle-serve` is a convenient shorthand for `yarn bundle-website` followed by `yarn serve`

## Testing

### Running the tests from the command line

After building, tests can be run from the command line via `yarn test-all`

### Running tests in browser
- As per command-line tests above, but run `yarn test-browser`, which opens
a browser window.
- To observe the status of tests, click `Debug` in the browser window, and then open the JavaScript Console for your browser (e.g., via the Developer Tools).

### Run Puppeteer tests for website X

- `yarn bundle-website X`
- `yarn test-website X`

Rebuild with `puppeteerTests.headless` set to `false` to run in browser.

## Development via VS Code

The following are some notes on developing Fluid using VS Code.

- Avoid having PureScript installed globally
- Install the PureScript IDE extension
- In the PureScript IDE extension settings, select `Add Npm Path`

- For Windows users:
	- Launch VSCode through Ubuntu (WSL) terminal
	- Install WSL extension in VSCode
