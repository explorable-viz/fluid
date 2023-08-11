## Fluid: Language-integrated data provenance

Fluid is an experimental programming language which integrates a bidirectional dynamic analysis to connect outputs to data sources in a fine-grained way. Fluid is implemented in PureScript and runs in the browser.

For the latest developments, see [news](news.md).

[![develop](https://github.com/explorable-viz/fluid/actions/workflows/develop.yml/badge.svg)](https://github.com/explorable-viz/fluid/actions/workflows/develop.yml)
[![GitHub pages](https://github.com/explorable-viz/fluid/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/explorable-viz/fluid/actions/workflows/pages/pages-build-deployment)
[![v0.3.1-typescript](https://github.com/explorable-viz/fluid/actions/workflows/v0.3.1-typescript.yml/badge.svg)](https://github.com/explorable-viz/fluid/actions/workflows/v0.3.1-typescript.yml)

### Software required

- git
- yarn
- `yarn install` to install application dependencies

#### Running the tests

- `yarn build-tests` and then `yarn tests` for core tests
- `yarn build-app-tests` and then `yarn app-tests` for app tests
- To debug the tests in the browser, use `yarn tests-browser` , hit Debug in the Chrome browser that opens, and then open Developer Tools

#### Running the web app

- `yarn serve-app` to build and serve app
- open a browser (preferably Chrome) at the served URL (usually `127.0.0.1:8080`).
