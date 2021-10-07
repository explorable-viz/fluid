# POPL 2022 Artifact Evaluation instructions

- `git clone https://github.com/explorable-viz/fluid.git` to get repo
- `docker build -t fluid-webapp .` to build Docker image
- `docker run -p 1234:1234 -it fluid-webapp` to start web server in Docker container
- open browser (preferably Chrome) at `http://localhost:1234/`
