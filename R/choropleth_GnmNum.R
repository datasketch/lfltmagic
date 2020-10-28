
#' Leaflet choropleths by numerical variable
#'
#' @name lflt_choropleth_GnmNum
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Num
#' @export
#' @examples
#' lflt_choropleth_GnmNum(sampleData("Gnm-Num", nrow = 10))
lflt_choropleth_GnmNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, ftype="Gnm-Num")

  lf <- lflt_basic_choropleth(l) %>%
          lflt_background(l$theme) %>%
            lflt_bounds(l$b_box) %>%
              lflt_graticule(l$graticule) %>%
               lflt_titles(l$titles)

  lf
}



#' Leaflet choropleths by numerical variable
#'
#' @name lflt_choropleth_Gnm
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' lflt_choropleth_Gnm(sampleData("Gnm", nrow = 10))
lflt_choropleth_Gnm <- lflt_choropleth_GnmNum
