
#' Leaflet choropleths by categorical variable
#'
#' Leaflet choropleths by categorical variable
#'
#' @name lflt_choropleth_GnmCatNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Cat-Num
#' @export
#' @examples
#' lflt_choropleth_GnmCatNum(sample_data("Gnm-Cat-Num", nrow = 10))
lflt_choropleth_GnmCatNum <- function(data = NULL, ...) {
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, ftype = "Gnm-Cat-Num")

  lf <- lflt_basic_choropleth(l) %>%
    lflt_background(l$theme) %>%
    lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)

  lf
}



#' Leaflet choropleths by categorical variable
#'
#' Leaflet choropleths by categorical variable
#'
#' @name lflt_choropleth_GnmCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Cat
#' @export
#' @examples
#' lflt_choropleth_GnmCat(sample_data("Gnm-Cat", nrow = 10))
lflt_choropleth_GnmCat <- lflt_choropleth_GnmCatNum
