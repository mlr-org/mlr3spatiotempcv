if (!requireNamespace(c("blockCV"), quietly = TRUE)) install.packages(c("blockCV", "sf", "patchwork", "sperrorest", "ggtext", "plotly"))
dir.create("pdf")

library("mlr3")
library("mlr3spatiotempcv")
task = tsk("ecuador")
rsmp_buffer = rsmp("spcv_buffer", theRange = 1000)
rsmp_buffer

autoplot(rsmp_buffer,
  size = 0.8, task = task, fold_id = 1) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


library("mlr3")
library("mlr3spatiotempcv")
requireNamespace("blockCV")
task = tsk("ecuador")
rsmp_disc = rsmp("spcv_disc", folds = 100, radius = 300L, buffer = 400L)
rsmp_disc

autoplot(rsmp_disc,
  size = 0.8, task = task, fold_id = 1) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


rsmp_coords = rsmp("spcv_coords", folds = 5)

autoplot(rsmp_coords,
  size = 0.8, fold_id = 1, task = task) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


rsmp_tiles = rsmp("spcv_tiles", nsplit = c(3L, 4L))

autoplot(rsmp_tiles,
  size = 0.8, fold_id = 1, task = task) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


breaks = quantile(task$data()$dem, seq(0, 1, length = 6))
zclass = cut(task$data()$dem, breaks, include.lowest = TRUE)

rsmp_custom = rsmp("custom_cv")
rsmp_custom$instantiate(task, f = zclass)

autoplot(rsmp_custom, size = 0.8, task = task, fold_id = 1) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


rsmp_block_random = rsmp("spcv_block", range = 1000, folds = 5)

autoplot(rsmp_block_random,
  size = 0.8, fold_id = 1, task = task,
  show_blocks = TRUE, show_labels = TRUE) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


rsmp_block_systematic = rsmp("spcv_block",
  range = 1000, folds = 5,
  selection = "systematic"
)

autoplot(rsmp_block_systematic,
  size = 0.8, fold_id = 1, task = task,
  show_blocks = TRUE, show_labels = TRUE) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))



task_cv = tsk("ecuador")
group = as.factor(kmeans(task_cv$coordinates(), 8)$cluster)
task_cv$cbind(data.frame("group" = group))
task_cv$set_col_roles("group", roles = "group")

rsmp_cv_group = rsmp("cv", folds = 3)$instantiate(task_cv)

print(rsmp_cv_group$instance)

autoplot(rsmp_cv_group, size = 0.8, task = task_cv, fold_id = 1) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


rsmp_env = rsmp("spcv_env",
  features = "distdeforest", folds = 5)

rsmp_env_multi = rsmp("spcv_env",
  features = c("distdeforest", "slope"), folds = 5)

plot_env_single = autoplot(rsmp_env,
  size = 0.3, fold_id = 1, task = task) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.02)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.02)) +
  ggplot2::theme(plot.title = ggtext::element_textbox(size = 8),
    axis.text = ggplot2::element_text(size = 8))

plot_env_multi = autoplot(rsmp_env_multi,
  size = 0.3, fold_id = 1, task = task) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.02)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.02)) +
  ggplot2::theme(plot.title = ggtext::element_textbox(size = 8),
    axis.text = ggplot2::element_text(size = 8))

library("patchwork")
plot_env_single + plot_env_multi + plot_layout(guides = "collect")

data = cookfarm_mlr3
set.seed(42)
data$Date = sample(rep(c(
  "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
  "2020-05-01"), times = 1, each = 35768))
task_spt = as_task_regr_st(data,
  id = "cookfarm", target = "PHIHOX",
  coordinate_names = c("x", "y"), coords_as_features = FALSE,
  crs = 26911)
task_spt$set_col_roles("Date", roles = "time")

rsmp_cstf_time = rsmp("sptcv_cstf", folds = 5)

p_lto = autoplot(rsmp_cstf_time,
  fold_id = 5, task = task_spt, plot3D = TRUE,
  point_size = 6, axis_label_fontsize = 15,
  sample_fold_n = 3000L
)

p_lto_print = plotly::layout(p_lto,
  scene = list(camera = list(eye = list(z = 0.58))),
  showlegend = FALSE, title = "",
  margin = list(l = 0, b = 0, r = 0, t = 0))

plotly::save_image(p_lto_print, "pdf/lto.pdf",
  scale = 2, width = 1000, height = 800)


knitr::include_graphics("pdf/lto.pdf")

task_spt$col_roles$time = character()
task_spt$set_col_roles("SOURCEID", roles = "space")

rsmp_cstf_loc = rsmp("sptcv_cstf", folds = 5)

p_llo = autoplot(rsmp_cstf_loc,
  fold_id = 5, task = task_spt,
  point_size = 6, axis_label_fontsize = 15,
  plot3D = TRUE, plot_time_var = "Date",
  sample_fold_n = 3000L)

p_llo_print =
  plotly::layout(p_llo,
    scene = list(camera = list(eye = list(z = 2.5, x = -0.1, y = -0.1))),
    showlegend = FALSE, title = "", polar = TRUE,
    margin = list(l = 0, b = 0, r = 0, t = 0))

plotly::save_image(p_llo_print, "pdf/llo.pdf",
  scale = 2, width = 1000, height = 800)

knitr::include_graphics("pdf/llo.pdf")

task_spt$set_col_roles("SOURCEID", roles = "space")
task_spt$set_col_roles("Date", roles = "time")

rsmp_cstf_time_loc = rsmp("sptcv_cstf", folds = 5)

p_lto = autoplot(rsmp_cstf_time_loc, point_size = 6,
  axis_label_fontsize = 15,
  fold_id = 4, task = task_spt, plot3D = TRUE,
  show_omitted = TRUE, sample_fold_n = 3000L)

p_lto_print = plotly::layout(p_lto,
  scene = list(camera = list(eye = list(z = 0.58))),
  showlegend = FALSE, title = "",
  margin = list(l = 0, b = 0, r = 0, t = 0))

plotly::save_image(p_lto_print, "pdf/llto.pdf",
  scale = 2, width = 1000, height = 800)

knitr::include_graphics("pdf/llto.pdf")

library("mlr3")
library("mlr3spatiotempcv")

lgr::get_logger("bbotk")$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

set.seed(42)

data("ecuador", package = "mlr3spatiotempcv")
task = as_task_classif_st(ecuador, target = "slides", positive = "TRUE",
  coordinate_names = c("x", "y"), coords_as_features = FALSE,
  crs = "EPSG:32717")

library("mlr3learners")

learner = lrn("classif.ranger", predict_type = "prob")

rsmp_nsp = rsmp("repeated_cv", folds = 4, repeats = 2)
rsmp_nsp
rr_nsp = resample(
  task = task, learner = learner,
  resampling = rsmp_nsp
)

rr_nsp$aggregate(measures = msr("classif.auc"))

rsmp_sp = rsmp("repeated_spcv_coords", folds = 4, repeats = 2)
rsmp_sp
rr_sp = resample(
  task = task, learner = learner,
  resampling = rsmp_sp
)

rr_sp$aggregate(measures = msr("classif.auc"))


autoplot(rsmp_sp, task, fold_id = 1:2, size = 0.8) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))


autoplot(rsmp_nsp, task, fold_id = 1:2, size = 0.8) *
  ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *
  ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))

sessionInfo()
