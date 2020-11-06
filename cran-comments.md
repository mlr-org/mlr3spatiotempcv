## Test environments
* local R installation, R 4.0.3
* GitHub Actions, R 4.0.3 and R-devel
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

Changes to previous submission:

- Commented in some examples in `autoplot()`
- Wrapped one example in `\dontrun{}` because it requires external software
- Require testthat v3.0.0 and run tests in parallel
