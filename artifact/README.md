## Getting Started

The prebuilt Docker image only supports x86_64 architectures.

### Downloading and connecting to Docker image:

1. Download compressed tarball from Zenodo
2. Extract tarball with gzip: `gunzip esop-artifact.tar.gz`
3. Load image into docker: `docker load -i esop-artifact.tar`
4. Run and connect to the image with `docker run -p 8080:8080 -it esop-artifact bash`

Alternatively, to rebuild Docker image from scratch:

1. Clone repository to local folder
2. From root directory of repository, run `docker build -f artifact/Dockerfile -t esop-artifact .`
3. Run and connect to the image as above

### Testing the installation

Inside the image, run `yarn test` to run the core test, or `yarn test-all`, which also runs the website-related tests.

## Step-by-Step Instructions for reproducibility:

### Reproducing tables in Section 5
1. Inside the image, run `yarn benchmark` in order to run all test programs 10 times,
collects the results to `benchmark/benchmarks_artifact.csv`
2. run `./artifact/script/rebuild_benchmarks.sh` from the default directory of the image

### Reproducing figures in Section 2
1. Inside the image, run `yarn serve esop2025-artifact`
then navigate to one of the subordinate webpages by navigating in your browser to
`https://localhost:8080/`, then selecting either of the links to `fig2` or `fig4`
2. Note that the figures in the paper feature additional curved arrows which were manually added
for illustrative purposes and do not appear on the web versions.

```
dist/esop2025-artifact
|── fig2
|── fig4
```

## Running additional experiments:
    1. If you want to experiment with additional experimental webpages,
      run and connect to the image with `docker run -p 8080:8080 -it esop-artifact bash`
    2. Run `yarn serve misc`, then navigate to one of the subordinate webpages at
    `https://localhost:8080/$1`, where `$1` is taken from one of the choices below.
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

### Creating Your Own Web-page Experiment
    1. Download the ESOP-artifact release from https://github.com/explorable-viz/fluid
    2. Follow the README instructions at the root of the repository to set up your development environment.
    3. Navigate to `website/Misc`
    4. create a new html file, and json file with the same name (`Experiment.html`, `Experiment.json`)
    5. choose one of the other websites (for example `Renewables`) and copy the contents into your own (`Renewables.html -> Experiment.html`, `Renewables.json -> Experiment.json`)
    6. Navigate to the root directory and build the website with `yarn bundle-website -w Misc`
    7. Launch the web-server with `yarn serve misc`
    8. In your browser, navigate to `https://localhost:8080/experiment`
    9. If you wish to modify the source code for your experiment, it will be found in the `fluid/` directory.
       Modifying the code of an experiment or its JSON file is beyond the scope of this README.
