
#' #' Hexmap chart Gln Glt
#'
#' @description
#' `lflt_hexmap_GlnGlt()` Create a Leaflet hexbin map based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column contains **longitudes points**,
#' and the second **latitudes points**, or make sure that the first two columns of
#' your data meet this condition
#' @export
#' @family Gln-Glt plots
#' @section Ftype:
#' Gln-Glt
#' @examples
#' data <- sample_data("Gln-Glt", n = 30)
#' lflt_hexmap_GlnGlt(data)
lflt_hexmap_GlnGlt <- function(data = NULL, ...) {
  if(!is.null(data)) {
    data[[1]] <- homodatum::as_Gln(data[[1]])
    data[[2]] <- homodatum::as_Glt(data[[2]])
  }

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- lfltmagic_prep(data = data, opts = opts, ftype="Gln-Glt")

  lf <- lflt_basic_hexmap(l) |>
    lflt_titles(l$titles)
  lf
}
