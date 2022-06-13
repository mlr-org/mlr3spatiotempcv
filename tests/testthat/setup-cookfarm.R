# subset of cookfarm task for smaller vdiffr snaps
# sampling 2 observations per Date+SOURCEID group
set.seed(42)
# b = as_data_backend(as.data.table(cookfarm_mlr3)[, .SD[sample(x = .N, size = 2)], by = .(Date, SOURCEID)])
b = readRDS(test_path("data", "cookfarm_subset.rds"))
b$hash = "_mlr3_tasks_cookfarm_"
tsk_cookfarm_sub = TaskRegrST$new(
  id = "cookfarm", b, target = "PHIHOX",
  coordinate_names = c("x", "y"), coords_as_features = FALSE,
  crs = 26911
)
tsk_cookfarm_sub$set_col_roles("Date", roles = "time")
tsk_cookfarm_sub$set_col_roles("SOURCEID", roles = "space")
