task = mlr_tasks$get("ecuador")

### create custom data for grouping variable
b = as.data.table(readRDS(system.file("extdata", "ecuador.rda",
  package = "mlr3spatiotempcv"
)))
# add grouping variable
data = insert_named(b[1:150, ], list(grp = rep_len(letters[1:10], 150)))
task_grp = TaskClassifST$new("ecuador-grp", as_data_backend(data),
  target = "slides",
  coordinate_names = c("x", "y"))
task_grp$col_roles$group = "grp"
