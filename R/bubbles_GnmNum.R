
#' Bubbles chart Gnm Num
#'
#' @description
#' `lflt_bubbles_GnmNum()` Create a Leaflet bubbles map based on a particular data type.
#' In this case, you can load data with only two columns, where the firts it's a **geoname column**,
#' and the second is a **numeric class column**, or make sure that the first two columns of
#' your data meet this condition
#'
#' @export
#' @family Gnm-Num plots
#' @section Ftype:
#' Gnm-Num
#' @examples
#' data <- sample_data("Gnm-Num", n = 30)
#' lflt_bubbles_GnmNum(data)
#'
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' lflt_bubbles_GnmNum(data,
#'                        agg = "mean")
#'
#'
lflt_bubbles_GnmNum <- function(data = NULL, ...) {

  if(!is.null(data)) data[[1]] <- homodatum::as_Gnm(data[[1]])
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, ftype="Gnm-Num")
  lf <- lflt_basic_bubbles(l) %>%
          lflt_background(l$theme) %>%
              lflt_graticule(l$graticule) %>%
                lflt_titles(l$titles)
  lf
}


#' Bubbles chart Gnm
#'
#' @description
#' `lflt_bubbles_Gnm()` Create a Leaflet bubbles map based on a particular data type.
#' In this case, you can load data with only one column, where it's a **geoname column**,
#' or make sure that the first column of your data meet this condition
#'
#' @export
#' @inheritParams lflt_bubbles_GnmNum
#' @section Ftype:
#' Gnm
#' @examples
#' data <- sample_data("Gnm", n = 30)
#' lflt_bubbles_Gnm(data)
#'
#' # Activate data labels
#' lflt_bubbles_Gnm(data)
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' lflt_bubbles_Gnm(data,
#'                        agg = "mean")
#'
lflt_bubbles_Gnm <- function(data = NULL, ...) {

  if(!is.null(data)) data[[1]] <- as_Gnm(data[[1]])
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, ftype="Gnm")
  lf <- lflt_basic_bubbles(l) %>%
    lflt_background(l$theme) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)
  lf
}

