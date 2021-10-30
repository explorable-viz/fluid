## Linked visualisations via Galois dependencies

![purescript](https://github.com/explorable-viz/fluid/workflows/purescript/badge.svg)
![typescript](https://github.com/explorable-viz/fluid/workflows/typescript/badge.svg)

To install and run the software see the [POPL 2022 artifact evaluation instructions](artifact-evaluation.md).

### Directory structure

The Fluid source code used for the tests and web app are found in the [`fluid/example`](fluid/example) directory. The core library is found in `lib/prelude`, with the matrix convolution functions in `lib/convolution`. The dataset used for the linking examples is in the folder `fluid/dataset`. Fluid test files have the extension `.fld`; the examples in the `fluid/slicing` folder also come with `.expect.fld` files, which capture the expected selection state on the program that arises from a backward analysis.

### Overview of tests and test infrastructure

#### Testing analyses described in paper

Test helpers (see below) are provided for testing the backward analysis (`bestBwd`) and the composition of the backward analysis with the De Morgan dual of the forward analysis (`testLink`). There is no support yet for standalone tests of the forward analysis, for reasons given below. However, all `testBwd` tests test the round-trip of the backward analysis followed by the forward analysis: the last argument passed to `testBwd` (of type string) is the expected (prettyprinted) output after the round-trip. (See `testWithSetup` in `test/Util.purs` for the implementation.) In some cases (such as the `map` test) the output selection is preserved by the round-trip; in other cases (such as `intersperse` or `filter`) the output selection gets larger. This is the key round-tripping property for forwards-after-backwards described in the paper.

The implementation does support running the forward analysis independently of the backward analysis, running the forward analysis without the De Morgan dual, and forward-analysing from code selections to output selections. However, providing a way of specifying a program or environment selection as input to the forward analysis will require some work. For example, to support program selections, we would either need to extend the parser to support something like the underscore notation, or provide some kind of zipper-like API that allows a test case to navigate through a program and create selections. We do intend to explore this in the future.

#### Test helpers

These are defined in `test/Util.purs`. Usage examples can be found in `test/Main.purs`. Note that most tests perform a forward and backward round-trip as a sanity-check, but only `testBwd` and `testLink` actually verify that the analysis results are as expected.

- `test`: The most basic kind of test. Desugars the test program, evaluates it to obtain a trace, and performs a forward and backward analysis (over both evaluation and desugaring) to sanity-check that they execute without runtime failure. The output of the round-trip is printed to the console, and the (prettyprinted) output is compared against a supplied expected value. (Typically there is no selection, so the comparison is of unselected values, and thus does not test the functionality of the analysis.)

- `testWithDataset`: Similar to `test`, but additionally loads a dataset (also represented as a `.fld` source file) and ensures that the test runs in an environment where that dataset has been bound to a variable. Only used for the legacy graphics tests (see below).

- `testBwd` Similar to `test`, but additionally checks that the backward analysis step produces an expected selection on the (prettyprinted) source program. The emitted source selection is printed to the console along with the result of the round-trip, which will now contain selection information. The prettyprinter will add underscores to selected expressions and values, allowing the expected source program selection to be provided as an additional source file (with extension `.expected.fld` in the same folder as the test) where selected expressions are demarked by underscores. Note, however, that the underscore notation is not supported by the parser, and so one cannot initiate a forward analysis by adding underscores to a `.fld` source file.

- `testLink` Tests the linking feature. Given two test programs and a (shared) dataset, desugars and evaluates both programs, applies a selection to the output of the first program, performs a backward analysis to produce a selection on the shared data, and then performs the De Morgan dual of the forward analysis to produce a selection on the output of the second program.  The output of the second program is printed to the console, and the (prettyprinted) output is compared against a supplied expected value, which must use underscores to represent the expected selection.

#### Test suites

The test suites are defined in `test/Main.purs` and is organised as follows:

- `test_scratchpad` is useful for running tests one at a time; see **Running individual tests** above.

- `test_linking` defines three linking tests, using the helper `testLink`. The source programs are in `fluid/example/linking`. There are tests for the bar chart/line chart and convolution examples in the paper, and also a simple linking test involving (nested) pairs.

- `test_bwd` defines several tests of the backwards analysis and associated round-trip, using the `testBwd` helper. The source programs are in `fluid/example/slicing`; each `.fld` file is paired with a `.expect.fld` containing the expectation for the source program selection. For example, the `filter` test shows that if you backward analyse with just the first cons cell in the output selected, then after the round-trip, the selection grows to include the first element in the list as well. (If you retain enough information to know that the output is at least one element long, you also retain enough information to know what that element is.) The file `filter.expect.fld` shows that various source elements are selected, such as the cons constructor in in the definition of `filter`.

- `test_desugaring` defines several tests which exercise the desugaring, using the `test` helper. The tests only check that the program desugars and executes correctly, not that the (source) program desugars to the expected core representation. For example, `list-comp-4` shows that the list comprehension `[ x | x : xs <- [[5], [4], [3], []] ]` desugars into a program that evaluates to the list `[5, 4, 3]`.

- `test_misc` defines several tests which verify that various primitives, library functions and data types work as expected. For example, `filter` and `length` test list functions from the prelude; `array` tests array construction expressions; and `div-mod-quot-rem` tests 4 related primitive operations from the prelude.

- `test_graphics` tests the (now deprecated) graphics library developed for the 0.3 release.

### Creating your own tests

You may wish to experiment with different Fluid source files and test expectations; we would be happy to assist with this.
