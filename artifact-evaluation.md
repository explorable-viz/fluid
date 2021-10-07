# POPL 2022 Artifact Evaluation instructions

We were unable to get a VirtualBox VM running under macOS Big Sur, so we provide a Docker-based artifact for evaluation instead.

## List of supported claims

Our artifact consists of an implementation, in PureScript, of the programming language Fluid described in the paper. More specifically, the artifact implements:
- the core language described in § 2;
- the bidirectional analysis described in § 3, and its De Morgan dual as described in § 4;
- surface language described in § 5.

The specific paper claims supported by the artifact relate to various figures in the paper, as detailed in the subsections below.

### Figure 1

[§ 4, second sentence] "In particular [forward evaluation] can answer questions like: “what data is needed to compute this bar in a bar chart?”, and indeed **we were able to use our implementation to generate Figure 1**."

### Figure 2

[§ 4.1, final sentence] "This is the approach implemented in Fluid, and **we used this to generate Figure 2** in § 1.2".

### Figure 13

[§ 4.2, paragraph 2] "**Fluid was used to generate the diagrams in Figure 13**..."

### Figure 16

[§ 5.1, paragraph 1] "**Figure 16 shows how the end-to-end mapping would appear to a user.**"

- link to source code and corresponding step of evaluation instructions

## Download, installation, and sanity-testing

### Software required

- git
- Docker; we have tested with Docker Desktop 4.1.0 for macOS

### Obtaining and verifying artifact

Follow the following steps:
- `git clone https://github.com/explorable-viz/fluid.git` to get this repo
- ensure Docker service is running
- `docker build -t fluid-webapp .` to build Docker image
- `docker run -p 1234:1234 -it fluid-webapp` to start web server in Docker container

To verify that the installation is working, open a browser (preferably Chrome) at `http://localhost:1234/`. Some text will load; if everything is working correctly, some images will load after about 30 seconds.

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
