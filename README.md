## Interactive data provenance for data visualisation

![purescript](https://github.com/explorable-viz/fluid/workflows/purescript/badge.svg)
![typescript](https://github.com/explorable-viz/fluid/workflows/typescript/badge.svg)

### Recent activity

(**W** indicates a Wrattler issue.)

| When | Activity | Issue(s) |
| --- | --- | --- |
| 26 Nov 19 | Demo for Wrattler workshop for DSTL ||
| 25 Nov 19 | First pass over nested coordinate frames and “domain units” | [183](https://github.com/rolyp/fluid/issues/183), [121](https://github.com/rolyp/fluid/issues/121), [180](https://github.com/rolyp/fluid/issues/180), [112](https://github.com/rolyp/fluid/issues/112) |
| 19 Nov 19 | Stacked bar, grouped bar and multiline chart examples | [110](https://github.com/rolyp/fluid/issues/110), [250](https://github.com/rolyp/fluid/issues/250) |
| 4 Nov 19 | Merge with Wrattler `develop` | [247](https://github.com/rolyp/fluid/issues/247) |
| 25 Oct 19 | “Swap node” proof-of-concept | [237](https://github.com/rolyp/fluid/issues/237) |
| 21 Oct 19 | Basic delta-visualisation for IC 2019 presentation | [220](https://github.com/rolyp/fluid/issues/220), [225](https://github.com/rolyp/fluid/issues/225), [230](https://github.com/rolyp/fluid/issues/230), [232](https://github.com/rolyp/fluid/issues/232), [235](https://github.com/rolyp/fluid/issues/235), [238](https://github.com/rolyp/fluid/issues/238) |
| 5 Oct 19 | Reinstate execution indexing | [213](https://github.com/rolyp/fluid/issues/213), [214](https://github.com/rolyp/fluid/issues/214) |
| 20 Sep 19 | Import data from another Wrattler cell | [193](https://github.com/rolyp/fluid/issues/193) |
| 15 Sep 19 | Moved slicing annotations from values to traces | [204](https://github.com/rolyp/fluid/issues/204) |
| 31 Aug 19 | Preliminary Wrattler integration | [192](https://github.com/rolyp/fluid/issues/192) |
| 30 Aug 19 | Publish as [npm library](https://www.npmjs.com/package/@rolyp/fluid) | [192](https://github.com/rolyp/fluid/issues/192) |
| 20 Aug 19 | Demo to Luke Marsden of [Dotscience](https://dotscience.com/) ||
| 18 Jul 19 | Demo to REG interview panel ||
| 17 Jul 19 | Demo to [Prodo](https://prodo.ai) ||
| 29 Jun 19 | Migrate to Nearley parser for improved error-reporting | [190](https://github.com/rolyp/fluid/issues/190) |
| 25 Jun 19 | Demo for HPI [Software Architecture Group](https://www.hpi.uni-potsdam.de/hirschfeld/index.html)||
| 18 Jun 19 | Preliminary design for linking visualisations | [164](https://github.com/rolyp/fluid/issues/164), [188](https://github.com/rolyp/fluid/issues/188) |
| 10 Jun 19 | Library code for axes | [53](https://github.com/rolyp/fluid/issues/53), [111](https://github.com/rolyp/fluid/issues/111) |

### In progress/forthcoming

| When | Feature/change | Issue(s) |
| --- | --- | --- |

### Possible submissions

| Deadline    | Venue            |
| --- | --- |
| <s>2 Aug 2019</s> | <s>LIVE 2019</s> |
| 2 Aug 2019 | IC 2019 :heavy_check_mark: |
| <s>5 Dec 2019</s> | <s>Eurovis 2020</s> |

### Installation

Ensure you have a recent version of [nodejs](https://nodejs.org/en/download/current/) and install using `yarn install`. The `yarn` scripts (invoked using `yarn run`) are as follows:

- `browser-test` to run tests in debug mode with Chrome
- `test` to run tests in headless mode, retesting when files change
- `bundle` to bundle the app
- `bundle-watch` to bundle the app, rebundling when files change
- `continuous-test` to run `test` and `bundle-watch` in parallel

![LambdaCalc](http://i.imgur.com/ERSxpE0.png "LambdaCalc")
