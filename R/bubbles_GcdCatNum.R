
#' Leaflet bubbles by categorical variable
#'
#' Leaflet bubbles by categorical variable
#'
#' @name lflt_bubbles_GcdCatNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Cat-Num
#' @export
#' @examples
#' lflt_bubbles_GcdCatNum(sample_data("Gcd-Cat-Num", nrow = 10))
lflt_bubbles_GcdCatNum <- function(data = NULL, ...) {
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id")

  lf <- lflt_basic_bubbles(l) %>%
    lflt_background(l$theme) %>%
    lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)

  lf
}



#' Leaflet bubbles by categorical variable
#'
#' Leaflet bubbles by categorical variable
#'
#' @name lflt_bubbles_GcdCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Cat
#' @export
#' @examples
#' lflt_bubbles_GcdCat(sample_data("Gcd-Cat", nrow = 10))
lflt_bubbles_GcdCat <- lflt_bubbles_GcdCatNum
