#' Bubbles chart Gnm Cat Num
#'
#' @description
#' `lflt_bubbles_GnmCatNum()` Create a Leaflet bubbles map based on a particular data type.
#' In this case, you can load data with only three columns, where the firts it's a **geoname column**,
#' the second is a **categorical column** and the third must be  a **numeric class column**, you should be sure that
#' three firts columns they meet this condition
#' @export
#' @family Gnm-Cat-Num plots
#' @section Ftype:
#' Gnm-Cat-Num
#' @examples
#' data <- sample_data("Gnm-Cat-Num", n = 30)
#' lflt_bubbles_GnmCatNum(data)
#'
#' # Activate data labels
#' lflt_bubbles_GnmCatNum(data)
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' lflt_bubbles_GnmCatNum(data,
#'                        agg = "mean",
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Gnm-Cat-Num-Dat-Yea-Cat", n = 30)
#' lflt_bubbles_GnmCatNum(data)
#'
#' # Change variable to color and pallete type
#' lflt_bubbles_GnmCatNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  lflt_bubbles_GnmCatNum(tooltip = info_tool)
lflt_bubbles_GnmCatNum <- function(data = NULL, ...) {


  if(!is.null(data)) {
    data[[1]] <- homodatum::as_Gnm(data[[1]])
  }

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, ftype = "Gnm-Cat-Num")

  lf <- lflt_basic_bubbles(l) %>%
    lflt_background(l$theme) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)

  lf
}



#' Leaflet bubbles by categorical variable
#'
#' @description
#' `lflt_bubbles_GnmCat()` Create a Leaflet bubbles map based on a particular data type.
#' In this case, you can load data with only two columns, where the firts it's a
#'  **geoname column**, and second is a **categorical column**,
#' or make sure that the firts two columns of you data meet this condition
#' @export
#' @inheritParams lflt_bubbles_GnmCatNum
#' @section Ftype:
#' Gnm-Cat
#' @examples
#' data <- sample_data("Gnm-Cat", n = 30)
#' lflt_bubbles_GnmCat(data)
#'
#' # Activate data labels
#' lflt_bubbles_GnmCat(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Gnm-Cat-Dat-Yea-Cat", n = 30)
#' lflt_bubbles_GnmCat(data)
#'
#' # Change variable to color and pallete type
#' lflt_bubbles_GnmCat(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  lflt_bubbles_GnmCat(tooltip = info_tool)
lflt_bubbles_GnmCat <-  function(data = NULL, ...) {
  if(!is.null(data)) {
    data[[1]] <- homodatum::as_Gnm(data[[1]])
    data[[2]] <- homodatum::as_Cat(data[[2]])
  }
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, ftype = "Gnm-Cat")

  lf <- lflt_basic_bubbles(l) %>%
    lflt_background(l$theme) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)

  lf
}
