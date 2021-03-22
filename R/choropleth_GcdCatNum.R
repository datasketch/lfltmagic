
#' Leaflet choropleths by categorical variable
#'
#' Leaflet choropleths by categorical variable
#'
#' @name lflt_choropleth_GcdCatNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Cat-Num
#' @export
#' @examples
#' lflt_choropleth_GcdCatNum(sample_data("Gcd-Cat-Num", nrow = 10))
lflt_choropleth_GcdCatNum <- function(data = NULL, ...) {

  data[[1]] <- as_Gcd(data[[1]])
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id", ftype = "Gcd-Cat-Num")

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
#' @name lflt_choropleth_GcdCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Cat
#' @export
#' @examples
#' lflt_choropleth_GcdCat(sample_data("Gcd-Cat", nrow = 10))
lflt_choropleth_GcdCat <- function(data = NULL, ...) {

  data[[1]] <- as_Gcd(data[[1]])
  data[[2]] <- as_Cat(data[[2]])
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id", ftype = "Gcd-Cat")

  lf <- lflt_basic_choropleth(l) %>%
    lflt_background(l$theme) %>%
    lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)

  lf
}
