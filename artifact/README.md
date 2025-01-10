### Installation (clone git repo):
    1. Clone the repository to a location of your choice
    2. From the root directory of the repository, build the docker image: `docker build -f artifact/Dockerfile -t esop-artifact .`
    3. Run and connect to the image with `docker run -p 8080:8080 -it esop-artifact bash`
### Getting Started (Zenodo download):
    1. Download the tarball
    2. Extract the tarball with gzip: `gunzip esop-artifact.tar.gz`
    3. Load the image into docker: `docker load -i esop-artifact.tar`
    4. Run and connect to the image with `docker run -p 8080:8080 -it esop-artifact bash`
### Testing the installation:
    - Inside the image, run `yarn test` or `yarn test-all`, which also runs the website related tests

From here, there are 2 options:
### Reproducing tables in Section 5:
    1. Inside the image, run `yarn benchmark` in order to run all test programs 10 times, 
      collects the results to `benchmark/benchmarks_artifact.csv`
    2. run `./artifact/script/rebuild_benchmarks.sh` from the default directory of the image

### Reproducing figures in Section 2:
    1. Inside the image, run `yarn serve esop2025-artifact`
      then navigate to one of the subordinate webpages by navigating in your browser to
      `https://localhost:8080/`, then selecting either of the links to `fig2` or `fig4`
    2. Note that the figures in the paper feature additional curved arrows which were manually added
      for illustrative purposes and do not appear on the web versions.
#### Valid Webpages:
```
dist/esop2025-artifact
|── fig2
|── fig4
```

### Running additional experiments:
    1. If you want to experiment with additional experimental webpages,
      run and connect to the image with `docker run -p 8080:8080 -it esop-artifact bash`
    2. Run `yarn serve $1`, where `$1` is one of `misc`, `fluid-org`
      then navigate to one of the subordinate webpages at `https://localhost:8080/$2`, where `$2` is 
      taken from one of the choices below.

#### Valid Webpages:
```
dist/misc
|── energy-scatter
|── figure-spm4b
|── methane
|── non-renewables
|── renewables-linked
|── table-spm1
```
```
dist/fluid-org
|── 0.3.1
|── 0.6.1
|── convolution
|── convolution-wrapped
|── moving-average
|── student-projects
```
### Creating Your Own Experiments
    1. TODO
