## ----setup, include=FALSE-----------------------------------------------------
set.seed(42)
library("mlr3")
library("mlr3spatiotempcv")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
mlr_reflections$task_types

## -----------------------------------------------------------------------------
mlr_reflections$task_col_roles

## ---- warning=FALSE-----------------------------------------------------------
as.data.table(mlr_resamplings)
