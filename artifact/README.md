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

1. Inside the image, run `yarn benchmark`to run all test programs 10 times and collect timing results in `benchmark/benchmarks_artifact.csv`. This step can take several minutes but should provide feedback after each benchmark.
2. Run `./artifact/script/rebuild_benchmarks.sh $1` from working directory of image, where $1 is either `benchmark/benchmarks_artifact.csv` or `benchmark/benchmarks_paper.csv`, if you want to exactly recreate the tables from the paper
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

To create your own experiments and figures, follow the following two steps.

### 1. Set up and verify development environment

1. Close the repository https://github.com/explorable-viz/fluid and switch to the tag `esop2025-artifact`
2. Follow the `README.md` instructions at the root of the repository to set up your development environment
3. From the root directory, verify that you can build the `misc` website with `yarn bundle-website -w Misc`. This should generate the following website structure in `dist`:

```
dist/misc
|── energy-scatter
|── figure-spm4b
|── methane
|── non-renewables
|── renewables-linked
|── table-spm1
```

3. Run `yarn serve misc` and verify that you can navigate to `https://localhost:8080/$1`, where `$1` is one of the web pages above

### 2. Create your own example web page

1. Navigate to `website/Misc`, pick an existing example in that folder (e.g. `Renewables`) and copy the corresponding HTML and JSON files to new files for your example (e.g. `Renewables.html -> Experiment.html`, `Renewables.json -> Experiment.json`)
2. From the root directory, rebuild the website with `yarn bundle-website -w Misc`
3. Run `yarn serve misc`; your example should be available at `https://localhost:8080/experiment`
4. Modify the Fluid source file for your example, which will be found at `fluid/P.fld`, where `P` is the path specified as the value of `"file"` in `Experiment.json`
5. After each modification, rebuild the website with `yarn bundle-website -w Misc` and run `yarn serve misc` to see the updated web page
