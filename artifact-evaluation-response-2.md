# Response to review comment @A7

Many thanks again for your diligence and useful probing of our artifact.

## Questions pertaining to functionality of the artifact:

> 1. How do I run a test and see the output? This is question (1) from my previous comment @a7.

We have changed the test runner so that every tests emits its output(s) to the console, prettyprinted where appropriate. The outputs that you can expect to see for each kind of test is described in the **Additional artifact description** section of `artifact-evaluation.md`.

> 2. Can I be pointed to at least one example of forward analysis among the existing benchmarks?

The implementation does support running the forward analysis independently of the backward analysis, running the forward analysis without the De Morgan dual, and forward-analysing from code selections to output selections. We agree that these would be interesting topics to explore, but we had neither the time nor space to explore these in the paper.

> 3. Please make the fixes pertaining to (6) from @a7

## Good to do for sake of clarity:

> 3. Are there any other examples of the linking feature apart from the convolutions among the existing benchmarks? Please point me to one if possible.

> 4. I would at least like to be able to change the underscores on a test case and see that the new output now has the corresponding relevant bits highlighted. As I mentioned in question (3) in @a7, I am wondering if this is simpler than creating a new test from scratch. I believe this is important because being able to do at least this much makes your tool less opaque, and might elicit more trust in your artifact even as it relates to your existing set of benchmarks. As it stands currently, I'm locked into just witnessing the success on your specific examples with specific selections.

### Good to do, even if outside scope of artifact that will be archived for this conference:

> 5. I was unable to find the source of these benchmarks/tests in the paper as well as the artifact. Did you create them yourself? If so, please include a general description of the artifacts and perhaps list the 'classes' of tests in the set of tests, with an example for each. I'm essentially asking for a profile of your benchmarks.

> 6. One simple example showing how to construct a new test, with further clarification if those instructions differ for the forward, backward, and linking analyses. I must mention that this is essentially the barrier to considering the artifact reusable at any level, and if that can be achieved by a short description of a simple example that perhaps even already exists, that would be great. I presume no expertise on my part with respect to how your tool actually works, but I am wondering whether the entire artifact could be improved to the point of being reusable by simply adding a 'tutorial' file with a couple of example and short, basic instructions after which one can play around with the system to discover more features.
