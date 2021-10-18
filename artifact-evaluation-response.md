Many thanks for your efforts in evaluating our artifact; we very much appreciate this. We respond to your questions below and have updated our artifact and instructions. We have also created tag `v0.4.0` in GitHub (https://github.com/explorable-viz/fluid/tree/v0.4.0) permanently referencing the (corrected) version of the artifact, and have updated the artifact evaluation instructions to point to this.

> 1. The web app does not display several bar charts as mentioned in the artifact-evaluation.md under Step 1. I can only see the one corresponding to Fig.2 (I am using Chrome on Ubuntu). In fact, the web page itself does not speak about Fig.1 at all.

Sorry about this. In the process of editing the final HTML, it seems we accidentally deleted the `div` tag used to insert the figure into the DOM. We have restored this; if you update your local copy of the repository and rebuild the web server, you should now see the additional raw images required for Fig. 1.

We also supplied the missing explanation of how the generated images relate to Fig. 1, and added section headings (one for each figure) to make it easier to read. (The section for Fig. 1 follows Fig. 2 because one of the Fig. 2 images is reused as part of Fig. 1.)

We updated `artifact-evaluation.md` to state that there should be three bar charts, rather than "several".

> 2. Are the visualizations meant to be interactive? If that is indeed the case, I am also not able to do that beyond 'hovering' over the bars. This may be related to my confusion regarding question 1 above.

These figures are not intended to be interactive, although d3.js adds tooltips to the bar charts automatically. We added clarifying text to the web page to set expectations appropriately. Sorry for not being clear about this.

> 3. Is it the case that the test files have .fld format and the expected outputs have .expect.fld format?

Correct. We have updated the "Additional artifact description" section of `artifact-evaluation.md` accordingly.

> 4. With the answer to 3 above, how do I run an individual test? The artifact-evaluation.md document often refers to the instructions above for running the test suite, but those instructions only pertain to running the entire suite, rather than individual tests.

Running individual tests is possible but requires a small amount of manual effort. We have added a new section to `artifact-evaluation.md` entitled "Running individual tests" which details how to do this.

> 5. I would like to run each test and render the visualizations, as opposed to running the tests in headless mode. If I am not mistaken, this would be required to carry out Step 4 as described in the artifact-evalaution.md
