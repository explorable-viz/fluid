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

[§ 5.1, paragraph 1] "**Figure 16 shows how the end-to-end mapping would appear to a user.**" The function

## Download, installation, and sanity-testing

### Software required

- git
- Docker; we have tested with Docker Desktop 4.1.0 for macOS

### Obtaining and verifying artifact

Follow the following steps:
- `git clone https://github.com/explorable-viz/fluid.git` to get this repo
- ensure Docker service is running
- `docker build -t fluid-webapp .` to build Docker image

To run the web app and verify that everything is working:
- `docker run -p 1234:1234 -it fluid-webapp` to start web server in Docker container
- open a browser (preferably Chrome) at `http://localhost:1234/`.

Some text will load; if everything is working correctly, the images required for Evaluation Steps 1-4 will load after about 30 seconds.

Alternatively, to get a shell prompt from which you can explore the implementation yourself, run:
- `docker run -it --entrypoint=/bin/bash fluid-webapp`

However, running the tests (via `yarn run tests`) , or any other project target that requires a web browser, will not work in the headless Docker environment.

### Directory layout

### How to run the artifact
- invoking build system; how to run the artifact on small test cases; expected output
- what directory to run each command from, what output files it generates, and how to compare output files to paper

## Evaluation instructions
- how to evaluate each claim in the paper that the artifact supports
- commands that generate evaluation data, and list of how to check evaluation data is similar to claims in paper
- for each command, output files it writes to, and how similar you expect the artifact results to be

### Step 1

### Step 2

### Step 3

## Additional artifact description

- how artifact is organized, which scripts and source files correspond to which experiments and components in the paper
- how reviewers can try their own inputs to the artifact
