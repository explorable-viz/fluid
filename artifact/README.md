### Generic instructions:
    - Download the tarball 
    - Extract the tarball
    - Run the image with `docker run -p 8080:8080 -it esop-artifact bash`
### Run Tests:
    - inside the image, run `yarn test` or `yarn test-all`, which also runs the website related tests

### Test webpages:
    - to serve a webpage, run `yarn serve $1`, where `$1` is one of `misc`, `fluid-org`, `esop2025-artifact`
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
