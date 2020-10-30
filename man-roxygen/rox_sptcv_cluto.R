#' @description
#'  Spatiotemporal cluster partitioning via the `vcluster` executable of the
#'  CLUTO clustering application.
#'
#' This partitioning method relies on the external CLUTO library.
#' To use it, CLUTO's executables need to be downloaded and installed into
#' this package.
#'
#' See \url{https://gist.github.com/pat-s/6430470cf817050e27d26c43c0e9be72} for
#' an installation approach that should work on Windows and Linux. macOS is not
#' supported by CLUTO.
#'
#' Before using this method, please check the restrictive copyright shown below.
#'
#' @details
#' By default, `-clmethod='direct'` is passed to the `vcluster` executable in
#' contrast to the upstream default `-clmethod='rb'`.
#' There is no evidence or research that this method is the best among the
#' available ones ("rb", "rbr", "direct", "agglo", "graph", "bagglo").
#' Also, various other parameters can be set via argument `cluto_parameters` to
#' achieve different clustering results.
#'
#' Parameter `-clusterfile` is handled by \CRANpkg{skmeans} and cannot be
#' changed.
#'
#' @section Copyright:
#'
#' CLUTO's copyright is as follows:
#'
#' The CLUTO package is copyrighted by the Regents of the University of
#' Minnesota.
#' It can be freely used for educational and research purposes by non-profit
#' institutions and US government agencies only.
#' Other organizations are allowed to use CLUTO only for evaluation purposes,
#' and any further uses will require prior approval.
#' The software may not be sold or redistributed without prior approval.
#' One may make copies of the software for their use provided that the copies,
#' are not sold or distributed, are used under the same terms and conditions.
#' As unestablished research software, this code is provided on an “as is” basis
#' without warranty of any kind, either expressed or implied.
#' The downloading, or executing any part of this software constitutes an
#' implicit agreement to these terms. These terms and conditions are subject to
#' change at any time without prior notice.
