
## Creating your own tests

We provide instructions for creating your own linking and round-tripping tests. (Adding your own desugaring or miscellaneous tests is easier and we omit the instructions.)

To create your own **linking** (`test_linking`) test, we suggest starting by copying the simple `pairs-1`/`pairs-2` example in `test_linking` and adapting it to your needs. Specific steps are:
1. Start by removing any tests from the list of tests in `test_scratchpad`.
2. Add your new test case there, using the `testLink` helper. You will need to provide two arguments. The first is of type `LinkConfig` providing the test configuration, with the following fields:
   - `file1` and `file2` are `File` arguments pointing to Fluid source files; these will be evaluated to provide the two outputs to be linked
   - `dataFile`: another `File` argument pointing to a Fluid source file containing an expression; this will be bound to a variable and become part of the shared context in which `file1` and `file2` will be evaluated
   - `dataVar`: the name of this shared variable which will be in scope in `file1` and `file2`
   - `v1_sel` the selection to be applied to the first view (see below)
3. The second argument to `testLink` is the _output expectation_ for the second view: the selection you expect on its output. This must be provided as a string, with underscores used to represent the expected selection. For the pairs example, `"(3, (_5_, _7_))"` represents a pair whose the second component is also a pair, each component of which is selected.
4. To specify `v1_sel`, you cannot use the underscore notation. Instead, you must construct a value directly representing the selection on the output of the first program. See other tests for some examples; there are also helpers `selectCell` (for arrays), `selectPair` (for pairs) and `selectNth` (for lists). The constant `hole` can be used to indicate that an entire subvalue is unselected.
5. Create the three files mentioned in the `LinkConfig`. Place these in the `fluid/example/linking` directory with the other linking tests.
    - Files `your-test-1.fld` and `your-test-2.fld`, defining the two views. These are just regular Fluid source files; they are free to mention the variable your provided as the value of `dataVar` in step 2.
    - File `your-test-data.fld`, which simply contains an expression that will be bound to the value of `dataVar`.
6. When you are happy with your new test, move it from `test_scatchpad` to the `test_linking` suite.

To create your own **backwards/forwards round-tripping** (`test_bwd`) test:
1. Start by removing the existing content of `test_scratchpad`.
2. [TODO]
3. When you are happy with your new test, move it from `test_scatchpad` to the `test_bwd` suite.
