#' @importFrom dplyr %>%

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
