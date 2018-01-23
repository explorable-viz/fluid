# Lambda: the Ultimate Spreadsheet

[![CircleCI](https://circleci.com/gh/rolyp/lambdacalc.svg?style=svg&circle-token=c86993fd6b2339b45286ddfc5a4c0c0d2401ffd7)](https://circleci.com/gh/rolyp/lambdacalc)

![LambdaCalc](http://i.imgur.com/ERSxpE0.png "LambdaCalc")

**Note:** This continues the release notes for the
  [experiment](https://github.com/rolyp/experiment) repository, but
  restarts the version numbering at version 0.0.1.

# Installation

- Ensure you have [nodejs](https://nodejs.org/en/download/current/)
(version 4.2.6 or above). Then:

  ```bash
  cd src/
  sudo npm install typings typescript --global
  sudo npm install
  ```
- `--unsafe-perm` may be required for `npm install` if the `postinstall`
  hook fails (but not recommended).

- To start the app (with automatic reloading when source files change),
  run `npm start`.
