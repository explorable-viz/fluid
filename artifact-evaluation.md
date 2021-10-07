# POPL 2022 Artifact Evaluation instructions

We were unable to get a VirtualBox VM running under macOS Big Sur, so we provide a Docker-based installation instead.

- `git clone https://github.com/explorable-viz/fluid.git` to get this repo
- ensure Docker is installed and running
- `docker build -t fluid-webapp .` to build Docker image
- `docker run -p 1234:1234 -it fluid-webapp` to start web server in Docker container
- open browser (preferably Chrome) at `http://localhost:1234/`
