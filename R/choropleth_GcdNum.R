
#' Leaflet choropleths by numerical variable
#'
#' Leaflet choropleths by numerical variable
#'
#' @name lflt_choropleth_GcdNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' lflt_choropleth_GcdNum(sampleData("Gcd-Num", nrow = 10))
lflt_choropleth_GcdNum <- function(data = NULL, ...) {

  if (!is.null(data)) {
    data[[1]] <- as_Gcd(data[[1]])
    data[[2]] <- as_Num(data[[2]])
  }
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id", ftype = "Gcd-Num")

  lf <- lflt_basic_choropleth(l) %>%
    lflt_background(l$theme) %>%
    #lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)

  lf
}


#' Leaflet choropleths by numerical variable
#'
#' @name lflt_choropleth_Gcd
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd
#' @export
#' @examples
#' lflt_choropleth_Gcd(sampleData("Gcd", nrow = 10))
lflt_choropleth_Gcd <-  function(data = NULL, ...) {

  if (!is.null(data)) data[[1]] <- as_Gcd(data[[1]])
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id", ftype = "Gcd")

  lf <- lflt_basic_choropleth(l) %>%
    lflt_background(l$theme) %>%
    #lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)

  lf
}

