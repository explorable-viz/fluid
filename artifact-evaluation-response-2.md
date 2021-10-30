# Response to review comment @A7

Many thanks again for your diligence and useful probing of our artifact. We have added a `README.md` with the information that was previously in the **Additional artifact description** section of `artifact-evaluation.md`, and linked the two documents. Where possible have placed any additional instructions in the `README`. **Please note that in the final version of the paper, Figure 16 is now Figure 18, and the instructions in `artifact-evaluation.md` now refer to Figure 18.**

## Questions pertaining to functionality of the artifact:

> 1. How do I run a test and see the output? This is question (1) from my previous comment @a7.

We have changed the test runner so that every tests emits its (prettyprinted) output(s) to the console. The outputs that you should expect to see for each kind of test is described in the `README.md`.

> 2. Can I be pointed to at least one example of forward analysis among the existing benchmarks?

All `testBwd` tests actually test the round-trip of the backward analysis followed by the forward analysis, but there is no support for standalone tests of the forward analysis. This is described in more detail in the `README.md`. (The linking tests, by contrast, test the backwards analysis followed by the De Morgan dual of the forward analysis.)

> 3. Please make the fixes pertaining to (6) from @a7

Thanks for spotting these inconsistencies. We have made the necessary fixes and clarifications, and amended the instructions for Step 4 in `artifact-evaluation.md`:

## Good to do for sake of clarity:

> 3. Are there any other examples of the linking feature apart from the convolutions among the existing benchmarks? Please point me to one if possible.

There is indeed an additional linking example. This is now mentioned under `test_linking` in the `README.md`.

> 4. I would at least like to be able to change the underscores on a test case and see that the new output now has the corresponding relevant bits highlighted. As I mentioned in question (3) in @a7, I am wondering if this is simpler than creating a new test from scratch. I believe this is important because being able to do at least this much makes your tool less opaque, and might elicit more trust in your artifact even as it relates to your existing set of benchmarks. As it stands currently, I'm locked into just witnessing the success on your specific examples with specific selections.

Unfortunately this is not possible. The underscore notation is supported by the prettyprinter, but not by the parser, and so can currently only be used for testing the backward analysis. This is now documented in the `README.md`.

### Good to do, even if outside scope of artifact that will be archived for this conference:

> 5. I was unable to find the source of these benchmarks/tests in the paper as well as the artifact. Did you create them yourself? If so, please include a general description of the artifacts and perhaps list the 'classes' of tests in the set of tests, with an example for each. I'm essentially asking for a profile of your benchmarks.

The different classes of test are now documented under **Test suites** in the `README.md`.

> 6. One simple example showing how to construct a new test, with further clarification if those instructions differ for the forward, backward, and linking analyses. I must mention that this is essentially the barrier to considering the artifact reusable at any level, and if that can be achieved by a short description of a simple example that perhaps even already exists, that would be great. I presume no expertise on my part with respect to how your tool actually works, but I am wondering whether the entire artifact could be improved to the point of being reusable by simply adding a 'tutorial' file with a couple of example and short, basic instructions after which one can play around with the system to discover more features.

We have added some instructions for creating new tests to the `README.md` and will continue to improve this.
