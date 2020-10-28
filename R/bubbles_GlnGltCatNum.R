
#' Leaflet bubbles by categorical variable
#'
#'
#' @name lflt_bubbles_GlnGltCatNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt-Cat-Num
#' @export
#' @examples
#' lflt_bubbles_GlnGltCatNum(sample_data("Gln-Glt-Cat-Num", nrow = 10))
lflt_bubbles_GlnGltCatNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- lfltmagic_prep(data = data, opts = opts, ftype= "Gln-Glt-Cat-Num")

  lf <- lflt_basic_points(l) %>%
    lflt_background(l$theme) %>%
    lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)
  lf
}


#' Leaflet bubbles by categorical variable
#'
#'
#' @name lflt_bubbles_GlnGltCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt-Cat
#' @export
#' @examples
#' lflt_bubbles_GlnGltCat(sample_data("Gln-Glt-Cat", nrow = 10))
lflt_bubbles_GlnGltCat <- lflt_bubbles_GlnGltCatNum
