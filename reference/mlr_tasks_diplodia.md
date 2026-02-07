# Diplodia Classification Task

Data set created by Patrick Schratz, University of Jena (Germany) and
Eugenia Iturritxa, NEIKER, Vitoria-Gasteiz (Spain). This dataset should
be cited as Schratz et al. (2019) (see reference below). The publication
also contains additional information on data collection. The data set
provided here shows infections of trees by the pathogen *Diplodia
Sapinea* in the Basque Country in Spain. Predictors are environmental
variables like temperature, precipitation, soil and more.

## Usage

``` r
data(diplodia)
```

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) inheriting
from
[mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html).

## Usage

    mlr_tasks$get("diplodia")
    tsk("diplodia")

## References

Schratz P, Muenchow J, Iturritxa E, Richter J, Brenning A (2019).
“Hyperparameter tuning and performance assessment of statistical and
machine-learning algorithms using spatial data.” *Ecological Modelling*,
**406**, 109–120.
[doi:10.1016/j.ecolmodel.2019.06.002](https://doi.org/10.1016/j.ecolmodel.2019.06.002)
.

## See also

[Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html) of
[Tasks](https://mlr3.mlr-org.com/reference/Task.html):
[mlr3::mlr_tasks](https://mlr3.mlr-org.com/reference/mlr_tasks.html)

`as.data.table(mlr_tasks)` for a complete table of all (also dynamically
created) [Tasks](https://mlr3.mlr-org.com/reference/Task.html).

Other Task:
[`TaskClassifST`](https://mlr3spatiotempcv.mlr-org.com/reference/TaskClassifST.md),
[`TaskRegrST`](https://mlr3spatiotempcv.mlr-org.com/reference/TaskRegrST.md),
[`mlr_tasks_cookfarm_mlr3`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_tasks_cookfarm_mlr3.md),
[`mlr_tasks_ecuador`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_tasks_ecuador.md)
