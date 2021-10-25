## Remaining considerations for judging whether claims in artifact-evaluation.md support my experience with the artifact:

> 1. I would like to be able to run an existing test and get an output, rather than see that the expected output has been achieved. Changing the .expect.fld file and witnessing an error seems insufficient to me.


> 2. It seems to me from the paper that there are 'forward' and 'backward' analyses, apart from the linking feature. But I'm confused by the fact that the tests indicated in steps 1,2, and 4 all pertain to output-to-input kind of analyses. I would like to see a simple example of a forward analysis, however trivial, in order to see that the artifact can do this. In particular, what kinds of forward analysis can I do? Am I only allowed to highlight inputs, or can I also highlight fragments of the code to see what outputs depend on the fragment? It would be great if there were an example that showed both a forward and a backward analysis, demonstrating the round-tripping capability that the paper talks about.

> 3. Given (1), I would like to be able to make small changes and view the change in the output. For instance, I would like to change the part of the data highlighted and see that the output now highlights the new relevant bits (both input -> output and output -> input analyses). This may be related to (5), but perhaps this task is simpler to do than writing a new test from scratch.

> 4. The tests under ``tests_misc`` in ``Main.purs`` like ``factorial`` or ``flatten`` do not seem to contain underscores at all. Are these tests doing anything towards the features in the artifact, or are just tests for the code base at large? If they are the latter, which tests have the dependency analyses?

## Advanced questions/artifact evaluation considerations:

> 5. I would like to know how to construct both forward and backward tests (very simple ones) from scratch. Walking me through an example of some transformation like the example in Step 4 would be useful (assuming that I have some knowledge of functional programming but no experience with PureScript). I have to note that this is not a stumbling block and if it were not possible to do this for someone unfamiliar with the system, I'd be fine with that.

## Notes/clarifications not relating to claims or artifact evaluation considerations:

> 6. The tests you have in the scratchpad array of tests for Step 4 in ``Main.purs`` do not seem to _exactly_ match the instances represented in Fig.16 in the paper (as was mentioned in the AE). For instance, the first test highlights two ``:`` constructors instead of one. While this isn't a problem for me because the outputs in the artifact itself seem to match the test case input and I believe I can roughly correspond the two, there is a disconnect with the AE text. Please fix this as it will make the artifact more usable.

> 7. Please provide one or two tests separately for each of the forward analysis, backward analysis, and the brushing-and-linking features as part of the artifact and mention them in the AE instruction file. While it's great to look at the nice visuals, it would also be good to have simple examples that one can execute and investigate.

> 8. I understand the many of the round-tripping or linking-by-dual features I'm asking for in (2), (3), (4), or (7) are already demonstrated by the convolution example. But I am asking for an example whose intended behavior is captured not 'visually' but 'symbolically', in the sense that I can do some small list transformation or arithmetic computation and check by dumb manual symbol pushing that the data substructures corresponded by the analyses do correspond in the appropriate way.
