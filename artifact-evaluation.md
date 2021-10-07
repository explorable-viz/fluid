# POPL 2022 Artifact Evaluation instructions

We were unable to get a VirtualBox VM running under macOS Big Sur, so we provide a Docker-based installation instead.

## List of claims

- all claims made in paper, organised by paper section; reference portion of artifact supporting that claim
- for each "claim", link to source code and corresponding step of evaluation instructions

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
