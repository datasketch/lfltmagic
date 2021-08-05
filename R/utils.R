#' Create cutoff points for legends for bubbles maps
create_legend_cuts <- function(x, number=seq(1:10)) {
  x_min <- min(x, na.rm = T)
  x_max <- max(x, na.rm = T)
  cuts_max <- 10^floor(log10(x_max)) * number[[which(x_max <= 10^floor(log10(x_max)) * number)[[1]]]]
  cuts <- seq(0, cuts_max, length.out = 5)
  if(x_min < 0){
    warning("Legend only displays positive numbers.")
  }
  cuts
}

#' Calculate custom intervals when map_color_scale = "Custom"
calculate_custom_intervals <- function(cutoff_points, domain) {
  min_cutoff <- min(cutoff_points, na.rm = T)
  max_cutoff <- max(cutoff_points, na.rm = T)
  min_domain <- min(domain, na.rm = T)
  max_domain <- max(domain, na.rm = T)
  if(min_cutoff < min_domain | max_cutoff > max_domain){
    warning("At least one of the custom cutoff points is outside of the domain.")
  }
  if(min_cutoff > min_domain){cutoff_points <- c(min_domain, cutoff_points)}
  if(max_cutoff < max_domain){cutoff_points <- c(cutoff_points, max_domain)}
  intervals <- cut(domain, breaks=cutoff_points, include.lowest=TRUE, dig.lab = 5)
  intervals
}


#' Get function from string of namespace::function() to pass to do.call
#'
#' @param x String of package namespace and function name, separated by ::
#'
#' @return Function without namespace of package
#'
#' @noRd
#'
#' @examples
#' getfun("shiny::actionButton")
getfun <- function(x) {
  if(length(grep("::", x))>0) {
    parts<-strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    x
  }
}

`%||%` <- function (x, y) {
  suppressWarnings({
    if (is.empty(x))
      return(y)
    else if (is.null(x) || is.na(x))
      return(y)
    else if (class(x) == "character" && all(nchar(x) == 0))
      return(y)
    else x
  })
}

is.empty <- function (x){
  if (length(x) == 0)
    return(TRUE)
  if (length(x) == 1 && nchar(x) == 0)
    return(TRUE)
  !as.logical(length(x))
}
