## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# don't run on mac
os = ifelse(!Sys.info()[["sysname"]] == "Darwin", TRUE, FALSE)

# - don't run on CRAN
is_cran = ifelse(Sys.getenv("NOT_CRAN") != "false", FALSE, TRUE)

# run on ci
pkgdown = Sys.getenv("IN_PKGDOWN") != ""

## ---- fig.align='center', eval=os&&!is_cran&&pkgdown, fig.width=8, fig.height=9----
#  library(mlr3)
#  library(mlr3spatiotempcv)
#  task_st = tsk("cookfarm")
#  resampling = rsmp("sptcv_cluto", folds = 5, time_var = "Date")
#  resampling$instantiate(task_st)
#
#  pl = plot(resampling, task_st, c(1, 2, 3, 4),
#    point_size = 3, axis_label_fontsize = 10)
#
#  # Warnings can be ignored
#  pl_subplot = plotly::subplot(pl)
#
#  plotly::layout(pl_subplot,
#    title = "Individual Folds",
#    scene = list(
#      domain = list(x = c(0, 0.5), y = c(0.5, 1)),
#      aspectmode = "cube",
#      camera = list(eye = list(z = 2.5))
#    ),
#    scene2 = list(
#      domain = list(x = c(0.5, 1), y = c(0.5, 1)),
#      aspectmode = "cube",
#      camera = list(eye = list(z = 2.5))
#    ),
#    scene3 = list(
#      domain = list(x = c(0, 0.5), y = c(0, 0.5)),
#      aspectmode = "cube",
#      camera = list(eye = list(z = 2.5))
#    ),
#    scene4 = list(
#      domain = list(x = c(0.5, 1), y = c(0, 0.5)),
#      aspectmode = "cube",
#      camera = list(eye = list(z = 2.5))
#    )
#  )

## ---- eval=os&&!is_cran&&!pkgdown, echo=FALSE, results='asis'-----------------
#  cat("(The actual graphic here only rendered on the pkgdown site: please visit https://mlr3spatiotempcv.mlr-org.com.)")

## ---- eval=!os, results='asis', echo=FALSE------------------------------------
cat("(The Cluto method does not work on macOS: please visit https://mlr3spatiotempcv.mlr-org.com to see the rendered plot.)")
