
#' Leaflet bubbless by numerical variable
#'
#' Leaflet bubbless by numerical variable
#'
#' @name lflt_bubbles_GcdNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' lflt_bubbles_GcdNum(sampleData("Gcd-Num", nrow = 10))
lflt_bubbles_GcdNum <- function(data = NULL, ...) {

  if(!is.null(data)) data[[1]] <- as_Gcd(data[[1]])
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id", ftype="Gcd-Num")
  lf <- lflt_basic_bubbles(l) %>%
    lflt_background(l$theme) %>%
    #lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)
  lf
}


#' Leaflet bubbless by numerical variable
#'
#' Leaflet bubbless by numerical variable
#'
#' @name lflt_bubbles_Gcd
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd
#' @export
#' @examples
#' lflt_bubbles_Gcd(sampleData("Gcd", nrow = 10))
lflt_bubbles_Gcd <- function(data = NULL, ...) {
  if(!is.null(data)) data[[1]] <- as_Gcd(data[[1]])
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id", ftype="Gcd")
  lf <- lflt_basic_bubbles(l) %>%
    lflt_background(l$theme) %>%
    #lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)
  lf
}
