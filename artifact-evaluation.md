# POPL 2022 Artifact Evaluation instructions

We were unable to get a VirtualBox VM running under macOS Big Sur, so we provide a Docker-based artifact instead.

## List of supported claims

The artifact consists of an implementation, in PureScript, of the programming language described in the paper (called _Fluid_ in the non-anonymised submission). More specifically, the artifact implements:
- the core language described in § 2;
- the bidirectional analysis described in § 3, and its De Morgan dual as described in § 4;
- surface language described in § 5.

The specific claims supported by the artifact relate to various figures in the paper, as detailed below.

### Figure 1

[§ 4, second sentence] "In particular [forward evaluation] can answer questions like: “what data is needed to compute this bar in a bar chart?”, and indeed **we were able to use our implementation to generate Figure 1**." The function ```linkingFigs``` in `src\app\Demo.purs` generates 6 output images; 4 of these were combined to produce Figure 1. See Evaluation Step 1.

### Figure 2

[§ 4.1, final sentence] "This is the approach implemented in Fluid, and **we used this to generate Figure 2** in § 1.2". Of the 6 images produced by ```linkingFigs```, 3 were combined to produce Figure 2. See Evaluation Step 2.

### Figure 13

[§ 4.2, paragraph 2] "**Fluid was used to generate the diagrams in Figure 13**..." The function ```convolutionFigs``` in `src\app\Demo.purs` generates 9 output images; 4 were combined to produce Figure 13a, and 5 were combined to produce Figure 13b. See Evaluation Step 3.

### Figure 16

[§ 5.1, paragraph 1] "**Figure 16 shows how the end-to-end mapping would appear to a user.**" The function ```testBwd``` of `src\test\Main.purs` runs the test `section-5-example`, which generates the raw information required for Figure 16.

## Download, installation, and sanity-testing

### Software required

- git
- Docker; we have tested with Docker Desktop 4.1.0 for macOS

### Obtaining the artifact

Follow the following steps:
- `git clone https://github.com/explorable-viz/fluid.git` to get this repo
- ensure Docker service is running
- `docker build -t fluid-webapp .` to build Docker image

### How to run the artifact

To run the web app and verify that everything is working:
- `docker run -p 1234:1234 -it fluid-webapp` to start web server in Docker container
- open a browser (preferably Chrome) at `http://localhost:1234/`.

Some text will load (including a heading saying "Fluid 0.4"); if everything is working correctly, the images required for Evaluation Steps 1-3 will load after about 30 seconds.

Alternatively, to get a shell prompt from which you can run tests:
- `docker run -it --entrypoint=/bin/bash fluid-webapp` to shell into the container

Then sanity-check that the sources compile cleanly with:
- `yarn run clean-tests`
- `yarn build-tests`

This should report 0 warnings, 0 errors, and conclude with "build suceeded". The compiled output is written to `dist/test/app.js`.

Finally, verify the tests with:

- `yarn run tests` to run the tests in Chrome headless mode

56 tests should pass in about 4 minutes. With the exception of the test `slicing/section-5-example` (Evaluation Step 4 below), the tests do not relate to specific claims made in the paper, but serve to validate the implementation as a whole.

## Evaluation instructions
- how to evaluate each claim in the paper that the artifact supports
- commands that generate evaluation data, and list of how to check evaluation data is similar to claims in paper
- for each command, output files it writes to, and how similar you expect the artifact results to be

### Step 1

### Step 2

### Step 3

### Step 4

## Additional artifact description

The Fluid source code used for the tests and web app are found in the `fluid/example` directory. The core library is found in `lib/prelude`, with the matrix convolution functions in `lib/convolution`. The dataset used for the linking examples is in the folder `fluid/dataset`.

Reviewers may wish to experiment with different Fluid source files and test expectations; we would be happy to assist with this.
