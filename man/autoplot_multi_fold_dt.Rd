% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot_multi_fold_dt.R
\name{autoplot_multi_fold_dt}
\alias{autoplot_multi_fold_dt}
\title{Autoplot helper}
\usage{
autoplot_multi_fold_dt(
  task,
  resampling,
  resampling_mod,
  sample_fold_n,
  fold_id,
  repeats_id,
  plot_as_grid,
  show_omitted,
  show_blocks,
  show_labels,
  label_size,
  ...
)
}
\arguments{
\item{resampling}{Actual resampling object (needed for spcv_block with
"show_blocks = TRUE")}

\item{resampling_mod}{Modified resampling object (normal data.table)}
}
\description{
Autoplot helper
}
\keyword{internal}
