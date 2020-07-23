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
