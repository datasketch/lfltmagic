
#' Leaflet bubbless by numerical variable
#'
#' Leaflet bubbless by numerical variable
#'
#' @name lflt_bubbles_GnmNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Num
#' @export
#' @examples
#' lflt_bubbles_GnmNum(sampleData("Gnm-Num", nrow = 10))
lflt_bubbles_GnmNum <- function(data, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts)
  print(l$d@data)
}
