#' #' Bubbles chart Gln Glt Num
#'
#' @description
#' `lflt_bubbles_GlnGlt()` Create a Leaflet bubbles map based on a particular data type.
#' In this case, you can load data with only three columns, where the firts column contains **longitudes points**,
#' the second **latitudes points** and the third must be a **numeric class column**,
#'  or be sure that three firts columns they meet this condition.
#' @export
#' @family Gln-Glt-Num plots
#' @section Ftype:
#' Gln-Glt-Num
#' @examples
#' data <- sample_data("Gln-Glt-Num", n = 30)
#' lflt_bubbles_GlnGltNum(data)
lflt_bubbles_GlnGltNum <- function(data = NULL, ...) {
  if(!is.null(data)) {
    data[[1]] <- homodatum::as_Gln(data[[1]])
    data[[2]] <- homodatum::as_Glt(data[[2]])
  }
  opts <- dsvizopts::merge_dsviz_options(...)
  l <- lfltmagic_prep(data = data, opts = opts, ftype="Gln-Glt-Num")

  lf <- lflt_basic_points(l) %>%
    lflt_background(l$theme) %>%
    #lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)
  lf
}


#' Bubbles chart Gln Glt
#'
#' @description
#' `lflt_bubbles_GlnGlt()` Create a Leaflet bubbles map based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column contains **longitudes points**,
#' and the second **latitudes points**, or make sure that the first two columns of
#' your data meet this condition
#'
#' @export
#' @inheritParams lflt_bubbles_GlnGlt
#' @section Ftype:
#' Gln-Glt
#' @examples
#' data <- sample_data("Gln-Glt", n = 30)
#' lflt_bubbles_GlnGlt(data)
#'
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' lflt_bubbles_GlnGlt(data,
#'                        agg = "mean")
#'
#'
#'
lflt_bubbles_GlnGlt <- function(data = NULL, ...) {

  if(!is.null(data)) {
    data[[1]] <- homodatum::as_Gln(data[[1]])
    data[[2]] <- homodatum::as_Glt(data[[2]])
  }
  opts <- dsvizopts::merge_dsviz_options(...)
  l <- lfltmagic_prep(data = data, opts = opts, ftype="Gln-Glt")

  lf <- lflt_basic_points(l) %>%
    lflt_background(l$theme) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)
  lf
}
