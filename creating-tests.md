
## Creating your own tests

### Linking tests

To create your own **linking** test, we suggest starting by copying the simple `pairs-1`/`pairs-2` example in `test_linking` and adapting it to your needs. Specific steps are:
1. Start by removing any tests from the list of tests in `test_scratchpad`.
2. Add your new test case there, using the `testLink` helper. You will need to provide two arguments. The first is of type `LinkConfig` providing the test configuration, with the following fields:
   - `file1` and `file2` are `File` arguments pointing to Fluid source files; these will be evaluated to provide the two outputs to be linked.
   - `dataFile`: another `File` argument pointing to a Fluid source file containing an expression; this will be bound to a variable and become part of the shared context in which `file1` and `file2` will be evaluated.
   - `dataVar`: the name of this shared variable which will be in scope in `file1` and `file2`.
   - `v1_sel` the selection to be applied to the first view (see below).
3. The second argument to `testLink` is the _output expectation_ for the second view: the selection you expect on its output. This must be provided as a string, with underscores used to represent the expected selection. For the pairs example, `"(3, (_5_, _7_))"` represents a pair whose the second component is also a pair, each component of which is selected.
4. To specify `v1_sel`, you cannot use the underscore notation. Instead, you must construct a value directly representing the selection on the output of the first program. See other tests for some examples; there are also helpers `selectCell` (for arrays), `selectPair` (for pairs) and `selectNth` (for lists). The constant `hole` can be used to indicate that an entire subvalue is unselected.
5. Create the three files mentioned in the `LinkConfig`. Place these in the `fluid/example/linking` directory with the other linking tests:
    - `your-test-1.fld` and `your-test-2.fld`, defining the two views. These are just regular Fluid source files; they are free to mention the variable you provided as the value of `dataVar` in step 2.
    - `your-test-data.fld`, which contains an expression whose value will be bound to the variable named in `dataVar`.
6. When you are happy with your new test, move it from `test_scatchpad` to the `test_linking` suite.

### Backward/forward round-tripping tests

To create your own **backward/forward round-tripping** test, we suggest starting by copying one of the simpler tests from `test_bwd`, such as the `zipWith` test, and adapting it to your needs. Specific steps are:
1. Start by removing any tests from the list of tests in `test_scratchpad`.
2. Add your new test case there, using the `testBwd` helper. You will need to provide four arguments:
    - A `File` argument pointing to `.fld` source file for the test, which will be evaluated to provide an output.
    - A `File` argument pointing to a `.expect.fld` file containing the expected selection on the source program that will result from the backward analysis. This should be a regular Fluid source file, with underscores used to represent the expected selection. For example in `zipWith-1.expect.fld`, the program fragment `y ** _2_` expresses that the second argument to `**` is selected.
    - The output selection to be used for the backward analysis. For this, you cannot use the underscore notation, but must instead construct a value directly representing the selection, as detailed above. For the `zipWith` example, the expression `(selectNth 1 (Float true 25.0))` selects the first element (starting from zero) of the output of `zipWith`.
    - The _output expectation_ for the round-trip: the selection you expect on the output after the forward analysis has completed. This must be provided as a string, with underscores used to represent the expected selection. For the `zipWith`example, `"(13.0 : (_25.0_ : (41.0 : [])))"` represents the output where the second element has been selected. In the case of `zipWith`, this happens to be essentially the same as the selection that was used to initiate the backward analysis; the selection round-trips exactly. Only the notation is different: one is provide as a string, the other as a PureScript expression representing a Fluid expression. More generally, the selection gets larger.
3. Create the two files `your-test.fld` and `your-test.expect.fld` (or similar) mentioned in the `testBwd` arguments. Place these in the `fluid/example/slicing` directory, with the other round-tripping tests.
4. When you are happy with your new test, move it from `test_scatchpad` to the `test_bwd` suite.
