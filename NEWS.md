<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# mlr3spatiotempcv 0.3.0.9001

- Add `ResamplingSpCVDisc` (#115)


# mlr3spatiotempcv 0.3.0.9000

- Same as previous version.


# mlr3spatiotempcv 0.3.0

## New Features

- `autoplot.ResamplingSptCVCstf()`: add 2D plotting method (#106)
- `autoplot.ResamplingSptCVCstf()`: add arguments `show_omitted` and `static_image` (#100)
- `autoplot()` (all methods): allow adjusting point size via `...` (#98)

## Maintenance

- Remove {GSIF} package due to CRAN archival and host the `cookfarm` dataset standalone
- Use `Cstf` method for spatiotemporal viz vignette
- Fix help page content of `ResamplingRepeatedSptCVCstf` (beforehand the Cluto method was referenced accidentally)
- Fix segfault in `autoplot.ResamplingSpcvBlock` example when rendering pkgdown site (unclear why this happens when `show_labels = TRUE`)
- Update `autoplot()` examples and related documentation
- Remove duplicate resources in Tasks "see also" fields
- Skip a test on Solaris and macOS 3.6
- Optimize "Spatiotemporal Visualization" vignette


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

