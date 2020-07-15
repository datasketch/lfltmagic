
#' Leaflet choropleths by numerical variable
#'
#' Leaflet choropleths by numerical variable
#'
#' @name lflt_choropleth_GcdCatNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' lflt_choropleth_GcdCatNum(sampleData("Gcd-Num", nrow = 10))
lflt_choropleth_GcdCatNum <- function(data = NULL, ...) {
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id")
  print(l$data)
}
