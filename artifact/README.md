### Generic instructions:
    - Download the tarball
    - Extract the tarball with gzip: `gunzip esop-artifact.tar.gz`
    - Load the image into docker: `docker load -i esop-artifact.tar`
    - Run the image with `docker run -p 8080:8080 -it esop-artifact bash`
### Run Tests:
    - Inside the image, run `yarn test` or `yarn test-all`, which also runs the website related tests
### Run Benchmarks:
    - Inside the image, run `yarn benchmark` in order to run all test programs 10 times, 
      collects the results to `benchmark/benchmarks_artifact.csv`
    - run `./artifact/script/rebuild_benchmarks.sh` from the default directory of the image

### Test webpages:
    - Inside the image, run `yarn serve $1`, where `$1` is one of `misc`, `fluid-org`, `esop2025-artifact`
      then navigate to one of the subordinate webpages at `https://localhost:8080/$2`, where `$2` is 
      taken from one of the choices below.

#### Valid Webpages:
```
dist/misc
├── energy-scatter
├── figure-spm4b
├── methane
├── non-renewables
├── renewables-linked
└── table-spm1
```

```
dist/fluid-org
├── 0.3.1
├── 0.6.1
├── convolution
├── convolution-wrapped
├── moving-average
└── student-projects
```

```
dist/esop2025-artifact
├── fig2
└── fig4
```
