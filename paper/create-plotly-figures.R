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

# LTO --------
p_lto = autoplot(rsmp_cstf_time,
  fold_id = 5, task = task_spt, plot3D = TRUE,
  sample_fold_n = 3000L
)

p_lto_print = layout(p_lto, scene = list(camera = list(eye = list(z = 0.58))), showlegend = F, title = "",
  margin = list(l = -150, b = 20, r = 0, t = -350))

tmp = tempfile(fileext = ".pdf")
plotly::save_image(p_lto_print, tmp,
  scale = 2,
  width = 900, height = 750)
fs::file_move(tmp, "~/git/mlr-org/mlr3spatiotempcv/paper/pdf/lto.pdf")

# LLO ------------

task_spt$col_roles$time = character()
task_spt$set_col_roles("SOURCEID", roles = "space")
rsmp_cstf_loc = rsmp("sptcv_cstf", folds = 5)
p_llo = autoplot(rsmp_cstf_loc,
  fold_id = 5, task = task_spt,
  plot3D = TRUE, plot_time_var = "Date",
  sample_fold_n = 3000L)

p_llo_print =
  layout(p_llo,
    scene = list(camera = list(eye = list(z = 2.5, x = -0.1, y = -0.1))),
    showlegend = F, title = "", polar = T,
    margin = list(l = -150, b = 20, r = 0, t = -350))
# p_llo_print

tmp = tempfile(fileext = ".pdf")
plotly::save_image(p_llo_print, tmp,
  scale = 2,
  width = 900, height = 750)
fs::file_move(tmp, "~/git/mlr-org/mlr3spatiotempcv/paper/pdf/llo.pdf")

# llto ------------

task_spt$set_col_roles("SOURCEID", roles = "space")
task_spt$set_col_roles("Date", roles = "time")

rsmp_cstf_space_time = rsmp("sptcv_cstf", folds = 5)

p_llto = autoplot(rsmp_cstf_space_time,
  fold_id = 4, task = task_spt, plot3D = TRUE,
  show_omitted = TRUE, sample_fold_n = 3000L)

p_llto_print =
  layout(p_llto,
    scene = list(camera = list(eye = list(z = 0.58, x = -1.4, y = 1.6))),
    showlegend = F, title = "",
    margin = list(l = -250, b = 20, r = 0, t = -350))
# p_llto_print

tmp = tempfile(fileext = ".pdf")
plotly::save_image(p_llto_print, tmp,
  scale = 2,
  width = 1100, height = 550)
fs::file_move(tmp, "~/git/mlr-org/mlr3spatiotempcv/paper/pdf/llto.pdf")


# NOTE: Crop the resulting images in Finder (or any other PDF tool)
