
#' Leaflet bubbles by numerical variable
#'
#'
#' @name lflt_bubbles_GlnGltNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Num
#' @export
#' @examples
#' lflt_bubbles_GlnGltNum(sampleData("Gln-Glt-Num", nrow = 10))
lflt_bubbles_GlnGltNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- lfltmagic_prep(data = data, opts = opts, ftype="Gln-Glt-Num")

  lf <- lflt_basic_points(l) %>%
    lflt_background(l$theme) %>%
    lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)
  lf
}


#' Leaflet bubbles with constant radius
#'
#'
#' @name lflt_bubbles_GlnGlt
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt
#' @export
#' @examples
#' lflt_bubbles_GlnGlt(sampleData("Gln-Glt", nrow = 10))
lflt_bubbles_GlnGlt <- lflt_bubbles_GlnGltNum
