<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# mlr3spatiotempcv 0.2.2.9001

- add tests
- `autoplot()`: allow adjusting point size via `...`
- optimize spatiotemp-viz vignette
- update autoplot examples and related documentation
- Remove duplicate resources in Tasks "see also" fields


# mlr3spatiotempcv 0.2.2.9000

- Internal changes only.


# mlr3spatiotempcv 0.2.2

- Internal changes only.


# mlr3spatiotempcv 0.2.1.9001

- Skip a test on Solaris and macOS 3.6


<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# mlr3spatiotempcv 0.2.1.9000

- Same as previous version.


# mlr3spatiotempcv 0.2.1

- Add support for `rasterLayer` argument in `blockCV::spatialBlock()` (#94)
- Ensure that `blockCV::spatialBlock()` functions actually returns the same result when invoked via {mlr3spatiotempcv} (#93).
  Among other issues, `blockCV::spatialBlock(selection = "checkerboard")` was ignored.
- Get coordinates names from {sf} objects dynamically.
  Before some functions would have errored if the coordinate names were not named "x" and "y".


# mlr3spatiotempcv 0.2.0

- Add support for {sf} objects for Task*ST creation (#90)
- "Getting Started" vignette: add example how to create a spatial task


# mlr3spatiotempcv 0.1.1

- CRAN-related changes
- Support ordered factors in TaskClassifST creation (#84)


# mlr3spatiotempcv 0.1.0

- Initial CRAN release

