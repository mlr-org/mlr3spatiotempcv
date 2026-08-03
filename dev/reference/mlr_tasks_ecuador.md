# Ecuador Classification Task

Data set created by Jannes Muenchow, University of Erlangen-Nuernberg,
Germany. This dataset should be cited as Muenchow et al. (2012) (see
reference below). The publication also contains additional information
on data collection and the geomorphology of the area. The data set
provided here is (a subset of) the one from the 'natural' part of the
RBSF area and corresponds to landslide distribution in the year 2000.

## Usage

``` r
data(ecuador)
```

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) inheriting
from
[mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html).

## Usage

    mlr_tasks$get("ecuador")
    tsk("ecuador")

## References

Muenchow, J., Brenning, A., Richter, M., 2012. Geomorphic process rates
of landslides along a humidity gradient in the tropical Andes.
Geomorphology, 139-140: 271-284.

## See also

[Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html) of
[Tasks](https://mlr3.mlr-org.com/reference/Task.html):
[mlr3::mlr_tasks](https://mlr3.mlr-org.com/reference/mlr_tasks.html)

`as.data.table(mlr_tasks)` for a complete table of all (also dynamically
created) [Tasks](https://mlr3.mlr-org.com/reference/Task.html).

Other Task:
[`TaskClassifST`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/TaskClassifST.md),
[`TaskRegrST`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/TaskRegrST.md),
[`mlr_tasks_cookfarm_mlr3`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/mlr_tasks_cookfarm_mlr3.md),
[`mlr_tasks_diplodia`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/mlr_tasks_diplodia.md)
