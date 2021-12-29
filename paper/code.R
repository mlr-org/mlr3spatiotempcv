## ----setup-2, echo=FALSE, purl=T---------------------------------------------------
# for knitr spin later
knitr::opts_chunk$set(fig.path = "./figs/",
  cache = TRUE,
  cache.lazy = TRUE)


## ----rsmp-schema, echo=FALSE, fig.cap="Conceptual overview of various spatial partitioning schemas. Starting from unpartioned spatial observations (top left) either a 'spatial block partitioning' or a 'spatial leave-one-out resampling' is applied in the first step. A spatial block partitioning can further be turned into a 'leave-one-block-out resampling' or a 'k-fold CV resampling at the block level'. The use of a buffer is theoretically possible in any scenario but in practice only offered by specific method implementations.", out.width="100%",fig.pos="ht"----
knitr::include_graphics("pdf/rsmp-schema.pdf")


## ----buffer-non-eval, eval=FALSE---------------------------------------------------
## library("mlr3")
## library("mlr3spatiotempcv")
## task = tsk("ecuador")
## rsmp_buffer = rsmp("spcv_buffer", theRange = 1000)
##
## autoplot(rsmp_buffer, size = 0.8, task = task, fold_id = 1)


## ----buffer-eval, fig.cap='Visualization of the spatial buffering method from package blockCV (method "spcv\\_buffer" in  mlr3spatiotempcv). The buffer distance is 1000 m.', out.width="40%", fig.pos="ht",echo=FALSE----
library("mlr3")
library("mlr3spatiotempcv")
task = tsk("ecuador")
rsmp_buffer = rsmp("spcv_buffer", theRange = 1000)
rsmp_buffer

autoplot(rsmp_buffer,
  size = 0.8, task = task, fold_id = 1) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


## ----disc-non-eval, eval=FALSE-----------------------------------------------------
## rsmp_disc = rsmp("spcv_disc", folds = 100, radius = 300L, buffer = 400L)
## rsmp_disc
##
## autoplot(rsmp_disc, size = 0.8, task = task, fold_id = 1)


## ----disc-eval, fig.cap='Visualization of one training set / test set combination generated with the leave-one-disc-out method from package sperrorest (method "spcv\\_disc" in mlr3spatiotempcv). The disc has a radius of 300 m and is surrounded by a 400-m buffer.', out.width="40%", echo=FALSE,fig.pos="ht"----
library("mlr3")
library("mlr3spatiotempcv")
task = tsk("ecuador")
rsmp_disc = rsmp("spcv_disc", folds = 100, radius = 300L, buffer = 400L)
rsmp_disc

autoplot(rsmp_disc,
  size = 0.8, task = task, fold_id = 1) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


## ----coords-non-eval, eval=FALSE---------------------------------------------------
## rsmp_coords = rsmp("spcv_coords", folds = 5)
##
## autoplot(rsmp_coords, size = 0.8, fold_id = 1, task = task)


## ----coords-eval, fig.cap='Leave-one-block-out CV based on $k$-means clustering of the coordinates as implemented in package sperrorest (method "spcv\\_coords" in mlr3spatiotempcv).', out.width="40%", fig.pos="ht", echo=FALSE----
rsmp_coords = rsmp("spcv_coords", folds = 5)

autoplot(rsmp_coords,
  size = 0.8, fold_id = 1, task = task) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


## ----tile-non-eval, eval=FALSE-----------------------------------------------------
## rsmp_tiles = rsmp("spcv_tiles", nsplit = c(3L, 4L))
##
## autoplot(rsmp_tiles, size = 0.8, fold_id = 1, task = task)


## ----tile, fig.cap='Leave-one-block-out resampling from package sperrorest (method "spcv\\_tiles" in package mlr3spatiotempcv with argument nsplit = c(3,4) indicating the number of rows and columns).', out.width="40%", fig.pos="ht", echo=FALSE----
rsmp_tiles = rsmp("spcv_tiles", nsplit = c(3L, 4L))

autoplot(rsmp_tiles,
  size = 0.8, fold_id = 1, task = task) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


## ----custom-cv-non-eval, eval=FALSE------------------------------------------------
## breaks = quantile(task$data()$dem, seq(0, 1, length = 6))
## zclass = cut(task$data()$dem, breaks, include.lowest = TRUE)
##
## rsmp_custom = rsmp("custom_cv")
## rsmp_custom$instantiate(task, f = zclass)
##
## autoplot(rsmp_custom, size = 0.8, task = task, fold_id = 1)


## ----custom-cv, fig.cap='Leave-one-level-out (custom) resampling from package mlr3 (method "custom\\_cv"). A factor variable is used to define all partitions.', out.width="40%", fig.pos="ht", echo=FALSE----
breaks = quantile(task$data()$dem, seq(0, 1, length = 6))
zclass = cut(task$data()$dem, breaks, include.lowest = TRUE)

rsmp_custom = rsmp("custom_cv")
rsmp_custom$instantiate(task, f = zclass)

autoplot(rsmp_custom, size = 0.8, task = task, fold_id = 1) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


## ----block-random-non-eval, eval=FALSE---------------------------------------------
## rsmp_block_random = rsmp("spcv_block", range = 1000, folds = 5)
##
## autoplot(rsmp_block_random, size = 0.8, fold_id = 1, task = task,
##   show_blocks = TRUE, show_labels = TRUE)


## ----block-random, fig.cap='Random resampling of square spatial blocks using the implementation in package blockCV (method "spcv\\_block" with option selection = "random" in mlr3spatiotempcv). The size of the squares is 1000 m, and four out of the 19 blocks were assigned to the test partition.', out.width="40%", fig.pos="ht", echo=FALSE----
rsmp_block_random = rsmp("spcv_block", range = 1000, folds = 5)

autoplot(rsmp_block_random,
  size = 0.8, fold_id = 1, task = task,
  show_blocks = TRUE, show_labels = TRUE) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


## ----block-systematic-non-eval, eval=FALSE-----------------------------------------
## rsmp_block_systematic = rsmp("spcv_block",
##   range = 1000, folds = 5, selection = "systematic"
## )
##
## autoplot(rsmp_block_systematic, size = 0.8, fold_id = 1, task = task,
##   show_blocks = TRUE, show_labels = TRUE)


## ----block-systematic, fig.cap='Sytematic resampling of square spatial blocks using the implementation in package blockCV (method "spcv\\_block" with option selection = "systematic" in mlr3spatiotempcv). The size of the squares is 1000 m, and four out of the 19 blocks were assigned to this test sample.', out.width="40%", fig.pos="ht", echo=FALSE----
rsmp_block_systematic = rsmp("spcv_block",
  range = 1000, folds = 5,
  selection = "systematic"
)

autoplot(rsmp_block_systematic,
  size = 0.8, fold_id = 1, task = task,
  show_blocks = TRUE, show_labels = TRUE) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


## ----cv-non-eval-1, eval=FALSE-----------------------------------------------------
## task_cv = tsk("ecuador")
## group = as.factor(kmeans(task$coordinates(), 8)$cluster)
## task_cv$cbind(data.frame("group" = group))
## task_cv$set_col_roles("group", roles = "group")
##
## rsmp_cv_group = rsmp("cv", folds = 3)$instantiate(task_cv)
##
## print(rsmp_cv_group$instance)


## ----cv-non-eval-2, fig.cap='Cross-Validation at the block level including predefined groups from package mlr3 (method "cv"). A factor variable (named "group") was used to define the grouping. Each class is either assigned to the test or training set.', out.width="40%", fig.pos="ht", eval=FALSE----
## autoplot(rsmp_cv_group, size = 0.8, task = task_cv, fold_id = 1)


## ----cv-eval-1, echo=FALSE---------------------------------------------------------
task_cv = tsk("ecuador")
group = as.factor(kmeans(task$coordinates(), 8)$cluster)
task_cv$cbind(data.frame("group" = group))
task_cv$set_col_roles("group", roles = "group")

rsmp_cv_group = rsmp("cv", folds = 3)$instantiate(task_cv)

print(rsmp_cv_group$instance)


## ----cv-eval-2, fig.cap='Cross-Validation at the block level including predefined groups from package mlr3 (method "cv"). A factor variable is used to define the grouping. Each class is either assigned to the test or training set.', out.width="40%", fig.pos="ht", echo=FALSE----
autoplot(rsmp_cv_group, size = 0.8, task = task_cv, fold_id = 1) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


## ----env-1-non-eval, eval=FALSE----------------------------------------------------
## rsmp_env = rsmp("spcv_env", features = "distdeforest", folds = 5)
##
## rsmp_env_multi = rsmp("spcv_env", features = c("distdeforest", "slope"), folds = 5)
##
## plot_env_single = autoplot(rsmp_env, size = 0.5, fold_id = 1, task = task) +
##
##   plot_env_multi = autoplot(rsmp_env_multi, size = 0.5, fold_id = 1, task = task)
##
## library("patchwork")
## plot_env_single + plot_env_multi


## ----env-1-eval, fig.cap='Environmental leave-one-block-out CV from package blockCV using one (left, "distdeforest") and two (right, "distdeforest" and "slope") predictors to define blocks in the feature space. Due to feature space clustering observations are not (necessarily) grouped in the spatial domain.', out.width="100%", fig.pos="ht", echo=FALSE----
rsmp_env = rsmp("spcv_env",
  features = "distdeforest", folds = 5)

rsmp_env_multi = rsmp("spcv_env",
  features = c("distdeforest", "slope"), folds = 5)

plot_env_single = autoplot(rsmp_env,
  size = 0.5, fold_id = 1, task = task) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.02)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.02)) +
  ggplot2::theme(plot.title = ggtext::element_textbox(size = 8),
    axis.text = ggplot2::element_text(size = 8))

plot_env_multi = autoplot(rsmp_env_multi,
  size = 0.5, fold_id = 1, task = task) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.02)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.02)) +
  ggplot2::theme(plot.title = ggtext::element_textbox(size = 8),
    axis.text = ggplot2::element_text(size = 8))

library("patchwork")
plot_env_single + plot_env_multi + plot_layout(guides = "collect")


## ---- results='hide'---------------------------------------------------------------
data = cookfarm_sample
data$Date = rep(c(
  "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
  "2020-05-01"), times = 1, each = 100)
b = mlr3::as_data_backend(data)
task_spt = TaskRegrST$new(
  id = "cookfarm", b, target = "PHIHOX",
  extra_args = list(
    coordinate_names = c("x", "y"), coords_as_features = FALSE,
    crs = 26911)
)

rsmp_cstf_time = rsmp("sptcv_cstf", folds = 5, time_var = "Date")
rsmp_cstf_time$instantiate(task_spt)

autoplot(rsmp_cstf_time,
  fold_id = 5, task = task_spt, plot3D = TRUE
)


## ----lto, echo=FALSE, out.width="70%", fig.cap='Perspective plot of "leave-time-out" CV from package CAST (method "sptcv\\_cstf" and argument time\\_var = "Date"). Only five folds and five time points were used in this example. Note that the blue dots correspond to five discrete time levels, which appear as a point cloud due to the viewing angle.', fig.pos="ht"----
knitr::include_graphics("png/lto.png")


## ----results='hide'----------------------------------------------------------------
rsmp_cstf_loc = rsmp("sptcv_cstf", folds = 5, space_var = "SOURCEID")
rsmp_cstf_loc$instantiate(task_spt)

autoplot(rsmp_cstf_loc,
  fold_id = 5, task = task_spt, plot3D = TRUE)


## ----llo, echo=FALSE, out.width="70%", fig.cap='Birds-eye view of "leave-location-out" CV from package CAST (method "sptcv\\_cstf" and argument space\\_var = "SOURCEID").', fig.pos="ht"----
knitr::include_graphics("png/llo.png")


## ----results='hide'----------------------------------------------------------------
rsmp_cstf_time_loc = rsmp("sptcv_cstf",
  folds = 5,
  space_var = "SOURCEID", time_var = "Date")
rsmp_cstf_time_loc$instantiate(task_spt)

autoplot(rsmp_cstf_time_loc,
  fold_id = 4, task = task_spt, plot3D = TRUE,
  show_omitted = TRUE)


## ----llto, echo=FALSE, out.width="70%", fig.cap='Perspective plot of "leave-location-and-time-out" CV from package CAST (method "sptcv\\_cstf" and arguments time\\_var = "Date" and space\\_var = "SOURCEID"). The grey points are excluded from both the training and the test set in this fold.', fig.pos="h"----
knitr::include_graphics("png/llto.png")


## ----load-pkgs---------------------------------------------------------------------
library("mlr3")
library("mlr3spatiotempcv")

# be less verbose
lgr::get_logger("bbotk")$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

set.seed(42)


## ----create-task-------------------------------------------------------------------
backend = mlr3::as_data_backend(ecuador)
task = TaskClassifST$new(
  id = "ecuador", backend = backend, target = "slides", positive = "TRUE",
  extra_args = list(
    coordinate_names = c("x", "y"), coords_as_features = FALSE,
    crs = "EPSG:32717")
)


## ----prepare-----------------------------------------------------------------------
library("mlr3learners")

learner = lrn("classif.ranger", predict_type = "prob")


## ----rr-nspcv-1, cache.lazy = FALSE, results='hide'--------------------------------
rsmp_nsp = rsmp("repeated_cv", folds = 4, repeats = 2)
rsmp_nsp
rr_nsp = resample(
  task = task, learner = learner,
  resampling = rsmp_nsp
)


## ----rr-nspcv-2, cache.lazy = FALSE------------------------------------------------
rr_nsp$aggregate(measures = msr("classif.auc"))


## ----rr-spcv-1, cache.lazy = FALSE, results='hide'---------------------------------
rsmp_sp = rsmp("repeated_spcv_coords", folds = 4, repeats = 2)
rsmp_sp
rr_sp = resample(
  task = task, learner = learner,
  resampling = rsmp_sp
)


## ----rr-spcv-2, cache.lazy = FALSE-------------------------------------------------
rr_sp$aggregate(measures = msr("classif.auc"))


## ----vis-spcv-non-eval, eval=FALSE-------------------------------------------------
## autoplot(rsmp_sp, task, fold_id = c(1:2), size = 0.8)


## ----vis-spcv-eval, fig.cap="Spatial leave-one-block-out partitioning using coordinate-based clustering to create roughly equally sized polygonal blocks. Due to space limitations only the first two folds of the first repetition are shown.", fig.pos="ht", fig.height=6, fig.width=8, echo=FALSE----
autoplot(rsmp_sp, task, fold_id = c(1:2), size = 0.8) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


## ----vis-nspcv-non-eval, eval=FALSE------------------------------------------------
## autoplot(rsmp_nsp, task, fold_id = c(1:2), size = 0.8)


## ----vis-nspcv-eval, fig.cap="Random (non-spatial) four-fold CV partitioning. Only the first two folds of the first repetition are shown.", fig.pos="ht", fig.height=6, fig.width=8, echo=FALSE----
autoplot(rsmp_nsp, task, fold_id = c(1:2), size = 0.8) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


## ----echo=FALSE, cache=FALSE-------------------------------------------------------
sessionInfo()
