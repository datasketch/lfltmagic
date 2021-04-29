
#' choropleth chart Gcd Num
#'
#' @description
#' `lflt_choropleth_GcdNum()` Create a Leaflet choropleth map based on a particular data type.
#' In this case, you can load data with only two columns, where the firts it's a **geocode column**,
#' and the second is a **numeric class column**, or make sure that the first two columns of
#' your data meet this condition
#'
#' @export
#' @family Gcd-Num plots
#' @section Ftype:
#' Gcd-Num
#' @examples
#' data <- sample_data("Gcd-Num", n = 30)
#' lflt_choropleth_GcdNum(data)
#'
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' lflt_choropleth_GcdNum(data,
#'                        agg = "mean")
#'
#'
lflt_choropleth_GcdNum <- function(data = NULL, ...) {

  if (!is.null(data)) {
    data[[1]] <- as_Gcd(data[[1]])
    data[[2]] <- as_Num(data[[2]])
  }
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id", ftype = "Gcd-Num")

  lf <- lflt_basic_choropleth(l) %>%
    lflt_background(l$theme) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)

  lf
}


#' choropleth chart Gcd
#'
#' @description
#' `lflt_choropleth_Gcd()` Create a Leaflet choropleth map based on a particular data type.
#' In this case, you can load data with only one column, where it's a **geocode column**,
#' or make sure that the first column of your data meet this condition
#'
#' @export
#' @inheritParams lflt_choropleth_GcdNum
#' @section Ftype:
#' Gcd
#' @examples
#' data <- sample_data("Gcd-Num", n = 30)
#' lflt_choropleth_Gcd(data)
#'
#' # Activate data labels
#' lflt_choropleth_Gcd(data)
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' lflt_choropleth_Gcd(data,
#'                        agg = "mean")
#'
lflt_choropleth_Gcd <-  function(data = NULL, ...) {

  if (!is.null(data)) data[[1]] <- as_Gcd(data[[1]])
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id", ftype = "Gcd")

  lf <- lflt_basic_choropleth(l) %>%
    lflt_background(l$theme) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)

  lf
}

