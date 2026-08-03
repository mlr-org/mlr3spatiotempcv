# mlr3spatiotempcv: Spatiotemporal Resampling Methods for 'mlr3'

Extends the mlr3 machine learning framework with spatio-temporal
resampling methods to account for the presence of spatiotemporal
autocorrelation (STAC) in predictor variables. STAC may cause highly
biased performance estimates in cross-validation if ignored. A JSS
article is available at
[doi:10.18637/jss.v111.i07](https://doi.org/10.18637/jss.v111.i07) .

## Main resources

- Book on mlr3: <https://mlr3book.mlr-org.com>

- mlr3book section about spatiotemporal data:
  <https://mlr3book.mlr-org.com/chapters/chapter13/beyond_regression_and_classification.html#spatiotemp-cv>

- package vignettes:
  <https://mlr3spatiotempcv.mlr-org.com/dev/articles/>

### Miscellaneous mlr3 content

- Use cases and examples: <https://mlr3gallery.mlr-org.com>

- More classification and regression tasks:
  [mlr3data](https://CRAN.R-project.org/package=mlr3data)

- Connector to [OpenML](https://www.openml.org):
  [mlr3oml](https://CRAN.R-project.org/package=mlr3oml)

- More classification and regression learners:
  [mlr3learners](https://CRAN.R-project.org/package=mlr3learners)

- Even more learners: <https://github.com/mlr-org/mlr3extralearners>

- Preprocessing and machine learning pipelines:
  [mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines)

- Tuning of hyperparameters:
  [mlr3tuning](https://CRAN.R-project.org/package=mlr3tuning)

- Visualizations for many mlr3 objects:
  [mlr3viz](https://CRAN.R-project.org/package=mlr3viz)

- Survival analysis and probabilistic regression:
  [mlr3proba](https://CRAN.R-project.org/package=mlr3proba)

- Cluster analysis:
  [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster)

- Feature selection filters:
  [mlr3filters](https://CRAN.R-project.org/package=mlr3filters)

- Feature selection wrappers:
  [mlr3fselect](https://CRAN.R-project.org/package=mlr3fselect)

- Interface to real (out-of-memory) data bases:
  [mlr3db](https://CRAN.R-project.org/package=mlr3db)

- Performance measures as plain functions:
  [mlr3measures](https://CRAN.R-project.org/package=mlr3measures)

- Parallelization framework:
  [future](https://CRAN.R-project.org/package=future)

- Progress bars:
  [progressr](https://CRAN.R-project.org/package=progressr)

## References

Schratz P, Muenchow J, Iturritxa E, Richter J, Brenning A (2019).
“Hyperparameter tuning and performance assessment of statistical and
machine-learning algorithms using spatial data.” *Ecological Modelling*,
**406**, 109–120.
[doi:10.1016/j.ecolmodel.2019.06.002](https://doi.org/10.1016/j.ecolmodel.2019.06.002)
.

Valavi R, Elith J, Lahoz-Monfort JJ, Guillera-Arroita G (2018).
“blockCV: an R package for generating spatially or environmentally
separated folds for k-fold cross-validation of species distribution
models.” *bioRxiv*. [doi:10.1101/357798](https://doi.org/10.1101/357798)
.

Meyer H, Reudenbach C, Hengl T, Katurji M, Nauss T (2018). “Improving
performance of spatio-temporal machine learning models using forward
feature selection and target-oriented validation.” *Environmental
Modelling & Software*, **101**, 1–9.
[doi:10.1016/j.envsoft.2017.12.001](https://doi.org/10.1016/j.envsoft.2017.12.001)
.

Zhao Y, Karypis G (2002). “Evaluation of Hierarchical Clustering
Algorithms for Document Datasets.” *11th Conference of Information and
Knowledge Management (CIKM)*, 51-524.
[doi:10.1145/584792.584877](https://doi.org/10.1145/584792.584877) .

## See also

Useful links:

- <https://mlr3spatiotempcv.mlr-org.com/>

- <https://github.com/mlr-org/mlr3spatiotempcv>

- <https://mlr3book.mlr-org.com>

- Report bugs at <https://github.com/mlr-org/mlr3spatiotempcv/issues>

## Author

**Maintainer**: Patrick Schratz <patrick.schratz@gmail.com>
([ORCID](https://orcid.org/0000-0003-0748-6624))

Authors:

- Marc Becker <marcbecker@posteo.de>
  ([ORCID](https://orcid.org/0000-0002-8115-0400))

Other contributors:

- Jannes Muenchow <jannes.muenchow@uni-jena.de>
  ([ORCID](https://orcid.org/0000-0001-7834-4717)) \[contributor\]

- Michel Lang <michellang@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-9754-0393)) \[contributor\]
