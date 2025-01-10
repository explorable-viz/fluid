## Getting Started

The prebuilt Docker image natively supports x86_64 architectures and ARM64 architectures in emulation mode.

### Downloading and connecting to Docker image:

1. Download compressed tarball from Zenodo
2. Extract tarball with gzip: `gunzip esop-artifact.tar.gz`
3. Load image into Docker: `docker load -i esop-artifact.tar`
4. Run and connect to image with `docker run -p 8080:8080 -it esop-artifact bash`

Alternatively, to rebuild Docker image from scratch:

1. Clone repository to local folder
2. From root directory of repository, run `docker build -f artifact/Dockerfile -t esop-artifact .`
3. Run and connect to the image as above

### Testing the installation

Inside the image, run `yarn test` to run the core test, or `yarn test-all`, which also runs the website-related tests.

## Step-by-Step Instructions for Reproducing Paper Content

### Reproducing tables in Section 5

1. Inside the image, run `yarn benchmark`to run all test programs 10 times and
collect timing results in `benchmark/benchmarks_artifact.csv`. This step can take several minutes but should provide feedback after each benchmark.
2. Run `./artifact/script/rebuild_benchmarks.sh` from working directory of image
3. In `./benchmark` you should find:
  - `benchmarks_artifact.csv`
  - a folder `tex` containing:
    - LaTeX files for tables 1, 2 and 3
    - a top-level LaTeX file `benchmarks.tex` collating the three tables into one document for convenience
    - `benchmarks.pdf` which will have been built from from `benchmarks.tex`

### Reproducing figures in Section 2

1. Inside the image, run `yarn serve esop2025-artifact`
2. Open a browser at `https://localhost:8080/` and select links to `fig2` and `fig4`

Note that the figures in the paper feature additional curved arrows; these were added manually
for illustrative purposes and do not appear on the web versions.

```
dist/esop2025-artifact
|── fig2
|── fig4
```

## Creating your own experiments and figures

To create your own experiments and figures, you will need to clone the repository and then modify, build and serve the `misc` website as follows.

1. Close the repository https://github.com/explorable-viz/fluid and switch to the tag `ESOP2025-artifact`
2. Follow the README instructions at the root of the repository to set up your development environment
3. From the root directory, verify that you can build the website with `yarn bundle-website -w Misc`. This should generate the following structure in `dist`:

```
dist/misc
|── energy-scatter
|── figure-spm4b
|── methane
|── non-renewables
|── renewables-linked
|── table-spm1
```

4. Navigate to `website/Misc`
5. Pick an existing example in that folder (e.g. `Renewables`) and copy the corresponding HTML and JSON files for your new example (e.g. `Renewables.html -> Experiment.html`, `Renewables.json -> Experiment.json`)
6. From the root directory, rebuild the website with `yarn bundle-website -w Misc`
7. Run `yarn serve misc`
8. In your browser, navigate to `https://localhost:8080/$1`, where `$1` is one of the existing  example folders or your new one (`experiment`)
9. If you wish to modify the Fluid source code for your web page, it will be found in the `fluid/` directory
