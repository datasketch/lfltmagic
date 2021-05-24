
#' Bubbles chart Gln Glt Cat Num
#'
#' @description
#' `lflt_bubbles_GlnGltCatNum()` Create a Leaflet bubbles map based on a particular data type.
#' In this case, you can load data with four columns, where the firts column contains **longitudes points**,
#' the second **latitudes points**, the third is a **categorical column** and the fourth must be a **numeric class column**,
#' or you must be sure that the first four columns of your data meet these rule.
#' @export
#' @family Gln-Glt-Cat-Num plots
#' @section Ftype:
#' Gln-Glt-Cat-Num
#' @examples
#' data <- sample_data("Gln-Glt-Cat-Num", n = 30)
#' lflt_bubbles_GlnGltCatNum(data)
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' lflt_bubbles_GlnGltCatNum(data,
#'                        agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Gln-Glt-Cat-Num-Dat-Yea-Cat", n = 30)
#' lflt_bubbles_GlnGltCatNum(data)
#'
#' # Change variable to color and pallete type
#' lflt_bubbles_GlnGltCatNum(data,
#'                        color_by = names(data)[3],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  lflt_bubbles_GlnGltCatNum(tooltip = info_tool)

lflt_bubbles_GlnGltCatNum <- function(data = NULL, ...) {

  if(!is.null(data)) {
    data[[1]] <- as_Gln(data[[1]])
    data[[2]] <- as_Glt(data[[2]])
  }

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- lfltmagic_prep(data = data, opts = opts, ftype= "Gln-Glt-Cat-Num")

  lf <- lflt_basic_points(l) %>%
    lflt_background(l$theme) %>%
    #lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)
  lf
}


#' Bubbles chart Gln Glt Cat
#'
#' @description
#' `lflt_bubbles_GlnGltCat()` Create a Leaflet bubbles map based on a particular data type.
#' In this case, you can load data with four columns, where the firts column contains **longitudes points**,
#' the second **latitudes points** and the third is a **categorical column**,
#' or you must be sure that the first four columns of your data meet these rule.
#' @export
#' @inheritParams lflt_bubbles_GlnGltCatNum
#' @section Ftype:
#' Gln-Glt-Cat
#' @examples
#' data <- sample_data("Gln-Glt-Cat", n = 30)
#' lflt_bubbles_GlnGltCat(data)
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' lflt_bubbles_GlnGltCat(data,
#'                        agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Gln-Glt-Cat-Dat-Yea-Cat", n = 30)
#' lflt_bubbles_GlnGltCat(data)
#'
#' # Change variable to color and pallete type
#' lflt_bubbles_GlnGltCat(data,
#'                        color_by = names(data)[3],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  lflt_bubbles_GlnGltCat(tooltip = info_tool)
lflt_bubbles_GlnGltCat <-  function(data = NULL, ...) {

  if(!is.null(data)) {
    data[[1]] <- as_Gln(data[[1]])
    data[[2]] <- as_Glt(data[[2]])
  }

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- lfltmagic_prep(data = data, opts = opts, ftype= "Gln-Glt-Cat")

  lf <- lflt_basic_points(l) %>%
    lflt_background(l$theme) %>%
    #lflt_bounds(l$b_box) %>%
    lflt_graticule(l$graticule) %>%
    lflt_titles(l$titles)
  lf
}

