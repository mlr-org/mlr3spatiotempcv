# Package index

## mlr3spatiotempcv

- [`mlr3spatiotempcv`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr3spatiotempcv-package.md)
  [`mlr3spatiotempcv-package`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr3spatiotempcv-package.md)
  : mlr3spatiotempcv: Spatiotemporal Resampling Methods for 'mlr3'

## Spatiotemporal Blocking Methods

These methods use square/rectangular blocks for partitioning.

- [`mlr_resamplings_spcv_block`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_block.md)
  [`ResamplingSpCVBlock`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_block.md)
  : (blockCV) Spatial block resampling
- [`mlr_resamplings_repeated_spcv_block`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_block.md)
  [`ResamplingRepeatedSpCVBlock`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_block.md)
  : (blockCV) Repeated spatial block resampling
- [`mlr_resamplings_spcv_tiles`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_tiles.md)
  [`ResamplingSpCVTiles`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_tiles.md)
  : (sperrorest) Spatial "Tiles" resampling
- [`mlr_resamplings_repeated_spcv_tiles`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_tiles.md)
  [`ResamplingRepeatedSpCVTiles`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_tiles.md)
  : (sperrorest) Repeated spatial "tiles" resampling

## Spatiotemporal Buffering Methods

These methods support using buffering zones which eventually remove
observations between train and test sets.

- [`mlr_resamplings_spcv_buffer`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_buffer.md)
  [`ResamplingSpCVBuffer`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_buffer.md)
  : (blockCV) Spatial buffering resampling
- [`mlr_resamplings_spcv_disc`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_disc.md)
  [`ResamplingSpCVDisc`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_disc.md)
  : (sperrorest) Spatial "disc" resampling
- [`mlr_resamplings_repeated_spcv_disc`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_disc.md)
  [`ResamplingRepeatedSpCVDisc`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_disc.md)
  : (sperrorest) Repeated spatial "disc" resampling
- [`mlr_resamplings_sptcv_cstf`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_sptcv_cstf.md)
  [`ResamplingSptCVCstf`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_sptcv_cstf.md)
  : (CAST) Spatiotemporal "Leave-location-and-time-out" resampling
- [`mlr_resamplings_repeated_sptcv_cstf`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_sptcv_cstf.md)
  [`ResamplingRepeatedSptCVCstf`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_sptcv_cstf.md)
  : (CAST) Repeated spatiotemporal "leave-location-and-time-out"
  resampling

## Spatiotemporal Clustering Methods

These methods make use of clustering methods (e.g. `k-means`) to create
(equally-sized) partitions.

- [`mlr_resamplings_spcv_coords`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_coords.md)
  [`ResamplingSpCVCoords`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_coords.md)
  : (sperrorest) Coordinate-based k-means clustering
- [`mlr_resamplings_repeated_spcv_coords`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_coords.md)
  [`ResamplingRepeatedSpCVCoords`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_coords.md)
  : (sperrorest) Repeated coordinate-based k-means clustering
- [`mlr_resamplings_spcv_disc`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_disc.md)
  [`ResamplingSpCVDisc`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_disc.md)
  : (sperrorest) Spatial "disc" resampling
- [`mlr_resamplings_repeated_spcv_disc`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_disc.md)
  [`ResamplingRepeatedSpCVDisc`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_disc.md)
  : (sperrorest) Repeated spatial "disc" resampling
- [`mlr_resamplings_spcv_knndm`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_knndm.md)
  [`ResamplingSpCVKnndm`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_knndm.md)
  : (CAST) K-fold Nearest Neighbour Distance Matching
- [`mlr_resamplings_repeated_spcv_knndm`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_knndm.md)
  [`ResamplingRepeatedSpCVKnndm`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_knndm.md)
  : (CAST) Repeated K-fold Nearest Neighbour Distance Matching

## Feature Space Clustering Methods

These methods cluster in the feature space and not (necessarily) in
space or time.

- [`mlr_resamplings_spcv_env`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_env.md)
  [`ResamplingSpCVEnv`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_spcv_env.md)
  : (blockCV) "Environmental blocking" resampling
- [`mlr_resamplings_repeated_spcv_env`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_env.md)
  [`ResamplingRepeatedSpCVEnv`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_spcv_env.md)
  : (blockCV) Repeated "environmental blocking" resampling

## Group-level Partitioning Methods

These methods use (multiple) factor-variables / inherited grouping to
create partitions.

- [`mlr_resamplings_sptcv_cstf`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_sptcv_cstf.md)
  [`ResamplingSptCVCstf`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_sptcv_cstf.md)
  : (CAST) Spatiotemporal "Leave-location-and-time-out" resampling
- [`mlr_resamplings_repeated_sptcv_cstf`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_sptcv_cstf.md)
  [`ResamplingRepeatedSptCVCstf`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_resamplings_repeated_sptcv_cstf.md)
  : (CAST) Repeated spatiotemporal "leave-location-and-time-out"
  resampling

## Spatiotemporal Tasks

Classification and regression tasks.

- [`TaskRegrST`](https://mlr3spatiotempcv.mlr-org.com/reference/TaskRegrST.md)
  : Create a Spatiotemporal Regression Task
- [`as_task_regr_st()`](https://mlr3spatiotempcv.mlr-org.com/reference/as_task_regr_st.md)
  : Convert to a Spatiotemporal Regression Task
- [`TaskClassifST`](https://mlr3spatiotempcv.mlr-org.com/reference/TaskClassifST.md)
  : Create a Spatiotemporal Classification Task
- [`as_task_classif_st()`](https://mlr3spatiotempcv.mlr-org.com/reference/as_task_classif_st.md)
  : Convert to a Spatiotemporal Classification Task

## Spatiotemporal Example Datasets

Tasks appended to the `mlr_tasks` dictionary.

- [`cookfarm_mlr3`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_tasks_cookfarm_mlr3.md)
  : Cookfarm Profiles Regression Task
- [`diplodia`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_tasks_diplodia.md)
  : Diplodia Classification Task
- [`ecuador`](https://mlr3spatiotempcv.mlr-org.com/reference/mlr_tasks_ecuador.md)
  : Ecuador Classification Task

## Visualization of spatiotemporal partitions

- [`autoplot(`*`<ResamplingCV>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingCV.md)
  [`autoplot(`*`<ResamplingRepeatedCV>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingCV.md)
  [`plot(`*`<ResamplingCV>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingCV.md)
  [`plot(`*`<ResamplingRepeatedCV>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingCV.md)
  : Visualization Functions for Non-Spatial CV Methods.
- [`autoplot(`*`<ResamplingCustomCV>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingCustomCV.md)
  [`plot(`*`<ResamplingCustomCV>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingCustomCV.md)
  : Visualization Functions for Non-Spatial CV Methods.
- [`autoplot(`*`<ResamplingSpCVBlock>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVBlock.md)
  [`autoplot(`*`<ResamplingRepeatedSpCVBlock>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVBlock.md)
  [`plot(`*`<ResamplingSpCVBlock>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVBlock.md)
  [`plot(`*`<ResamplingRepeatedSpCVBlock>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVBlock.md)
  : Visualization Functions for SpCV Block Methods.
- [`autoplot(`*`<ResamplingSpCVBuffer>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVBuffer.md)
  [`plot(`*`<ResamplingSpCVBuffer>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVBuffer.md)
  : Visualization Functions for SpCV Buffer Methods.
- [`autoplot(`*`<ResamplingSpCVCoords>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVCoords.md)
  [`autoplot(`*`<ResamplingRepeatedSpCVCoords>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVCoords.md)
  [`plot(`*`<ResamplingSpCVCoords>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVCoords.md)
  [`plot(`*`<ResamplingRepeatedSpCVCoords>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVCoords.md)
  : Visualization Functions for SpCV Coords Methods.
- [`autoplot(`*`<ResamplingSpCVDisc>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVDisc.md)
  [`autoplot(`*`<ResamplingRepeatedSpCVDisc>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVDisc.md)
  [`plot(`*`<ResamplingSpCVDisc>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVDisc.md)
  [`plot(`*`<ResamplingRepeatedSpCVDisc>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVDisc.md)
  : Visualization Functions for SpCV Disc Method.
- [`autoplot(`*`<ResamplingSpCVEnv>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVEnv.md)
  [`autoplot(`*`<ResamplingRepeatedSpCVEnv>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVEnv.md)
  [`plot(`*`<ResamplingSpCVEnv>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVEnv.md)
  [`plot(`*`<ResamplingRepeatedSpCVEnv>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVEnv.md)
  : Visualization Functions for SpCV Env Methods.
- [`autoplot(`*`<ResamplingSpCVKnndm>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVKnndm.md)
  [`autoplot(`*`<ResamplingRepeatedSpCVKnndm>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVKnndm.md)
  [`plot(`*`<ResamplingSpCVKnndm>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVKnndm.md)
  [`plot(`*`<ResamplingRepeatedSpCVKnndm>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVKnndm.md)
  : Visualization Functions for SpCV knndm Method.
- [`autoplot(`*`<ResamplingSpCVTiles>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVTiles.md)
  [`autoplot(`*`<ResamplingRepeatedSpCVTiles>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVTiles.md)
  [`plot(`*`<ResamplingSpCVTiles>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVTiles.md)
  [`plot(`*`<ResamplingRepeatedSpCVTiles>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSpCVTiles.md)
  : Visualization Functions for SpCV Tiles Method.
- [`autoplot(`*`<ResamplingSptCVCstf>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSptCVCstf.md)
  [`autoplot(`*`<ResamplingRepeatedSptCVCstf>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSptCVCstf.md)
  [`plot(`*`<ResamplingSptCVCstf>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSptCVCstf.md)
  [`plot(`*`<ResamplingRepeatedSptCVCstf>`*`)`](https://mlr3spatiotempcv.mlr-org.com/reference/autoplot.ResamplingSptCVCstf.md)
  : Visualization Functions for SptCV Cstf Methods.
