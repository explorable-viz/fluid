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

#### Running the tests on the command line

- `yarn build-tests` and then `yarn tests` for core tests
- `yarn build-app-tests` and then `yarn app-tests` for app tests

#### Running the tests in the browser

- `yarn build-tests` as above, then `yarn tests-browser`
- hit Debug in the browser window that opens, and then open Developer Tools

#### Running the web app

- `yarn serve-app` to build and serve app
- open a browser (preferably Chrome) at the served URL (usually `127.0.0.1:8080`).

#### Regenerating data for energy linked-inputs

 - Download energy data from [here](https://ember-climate.org/data-catalogue/yearly-electricity-data/) (choose yearly full release long format (CSV))
 - Download gdp data from [here](https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?most_recent_value_desc=true), download button on the right, choose csv, and extract the contents into your downloads
 - Leave both files in `~/Downloads/`, then run `energy_data.py`, if you want to change countries or years of interest, you may need to do some fiddling, since some countries have different names in both datasets.