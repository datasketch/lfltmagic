#' Bubbles chart Gcd Cat Num
#'
#' @description
#' `lflt_bubbles_GcdCatNum()` Create a Leaflet bubbles map based on a particular data type.
#' In this case, you can load data with only three columns, where the firts it's a **geocode column**,
#' the second is a **categorical column** and the third must be  a **numeric class column**, you should be sure that
#' three firts columns they meet this condition
#' @export
#' @family Gcd-Cat-Num plots
#' @section Ftype:
#' Gcd-Cat-Num
#' @examples
#' data <- sample_data("Gcd-Cat-Num", n = 30)
#' lflt_bubbles_GcdCatNum(data)
#'
#' # Activate data labels
#' lflt_bubbles_GcdCatNum(data,
#'                        dataLabels_show = TRUE)
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' lflt_bubbles_GcdCatNum(data,
#'                        agg = "mean",
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Gcd-Cat-Num-Dat-Yea-Cat", n = 30)
#' lflt_bubbles_GcdCatNum(data)
#'
#' # Change variable to color and pallete type
#' lflt_bubbles_GcdCatNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  lflt_bubbles_GcdCatNum(tooltip = info_tool)

lflt_bubbles_GcdCatNum <- function(data = NULL, ...) {

  data[[1]] <- as_Gcd(data[[1]])
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id", ftype= "Gcd-Cat-Num")

  lf <- lflt_basic_bubbles(l) %>%
    lflt_background(l$theme) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)

  lf
}



#' Leaflet bubbles by categorical variable
#'
#' @description
#' `lflt_bubbles_GcdCat()` Create a Leaflet bubbles map based on a particular data type.
#' In this case, you can load data with only two columns, where the firts it's a
#'  **geocode column**, and second is a **categorical column**,
#' or make sure that the firts two columns of you data meet this condition
#' @export
#' @inheritParams lflt_bubbles_GcdCatNum
#' @section Ftype:
#' Gcd-Cat
#' @examples
#' data <- sample_data("Gcd-Cat", n = 30)
#' lflt_bubbles_GcdCat(data)
#'
#' # Activate data labels
#' lflt_bubbles_GcdCat(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Gcd-Cat-Dat-Yea-Cat", n = 30)
#' lflt_bubbles_GcdCat(data)
#'
#' # Change variable to color and pallete type
#' lflt_bubbles_GcdCat(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  lflt_bubbles_GcdCat(tooltip = info_tool)
lflt_bubbles_GcdCat <- function(data = NULL, ...) {

  data[[1]] <- as_Gcd(data[[1]])
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id", ftype= "Gcd-Cat")

  lf <- lflt_basic_bubbles(l) %>%
    lflt_background(l$theme) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)

  lf
}
