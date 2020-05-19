
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
lflt_bubbles_GnmNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts)
  lf <- lflt_basic_bubbles(l) %>%
          lflt_background(l$theme) %>%
            lflt_bounds(l$b_box) %>%
              lflt_graticule(l$graticule) %>%
                lflt_titles(l$titles)
  lf
}


#' Leaflet bubbless by numerical variable
#'
#' Leaflet bubbless by numerical variable
#'
#' @name lflt_bubbles_Gnm
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' lflt_bubbles_Gnm(sampleData("Gnm", nrow = 10))
lflt_bubbles_Gnm <- lflt_bubbles_GnmNum
