# POPL 2022 Artifact Evaluation instructions

We were unable to get a VirtualBox VM running under macOS Big Sur, so we provide a Docker-based installation instead.

- `git clone https://github.com/explorable-viz/fluid.git` to get this repo
- ensure Docker is installed and running
- `docker build -t fluid-webapp .` to build Docker image
- `docker run -p 1234:1234 -it fluid-webapp` to start web server in Docker container
- open browser (preferably Chrome) at `http://localhost:1234/`

## List of claims

- all claims made in paper, organised by paper section; reference portion of artifact supporting that claim
- for each "claim", link to source code and corresponding step of evaluation instructions

## Download, installation, and sanity-testing

- instructions for obtaining artifact and ensuring it works
- list software reviewer will need along with version numbers and platforms that are known to work
- list all files the reviewer will need to download (such as the virtual machine image)
- note the guest OS used in the virtual machine
- explain directory layout
- all steps necessary to set up artifact and ensure it works, including: invoking build system; how to run the artifact on small test cases, benchmarks, or proofs; expected output
- what directory to run each command from, what output files it generates, and how to compare output files to paper

## Evaluation instructions
- how to run complete artifact, and then evaluate each claim in the paper that the artifact supports
- commands that generate evaluation data, and list of how to check evaluation data is similar to claims in paper
- for each command, output files it writes to, and how similar you expect the artifact results to be

## Additional artifact description

- how artifact is organized, which scripts and source files correspond to which experiments and components in the paper
- how reviewers can try their own inputs to the artifact
