# Autoplot helper

Autoplot helper

## Usage

``` r
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
```

## Arguments

- resampling:

  Actual resampling object (needed for spcv_block with "show_blocks =
  TRUE")

- resampling_mod:

  Modified resampling object (normal data.table)
