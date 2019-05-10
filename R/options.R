getDefaultOpts <- function(...){

  dopts <- list(
    nLevels = 5,
    labelWrap = 12,
    projection_name = NULL,
    projection_ratio = NULL,
    projection_type = NULL,
    projection_orientation = c(90, 0, 0),
    dropNa = FALSE,
    graticule = FALSE,
    graticule_color = '#cccccc',
    graticule_interval = 50,
    graticule_weight = 1,
    border_color = "#2d2d2d",
    borderWidth = 1,
    borderOpacity = 1,
    opacity = 0.7,
    stroke = FALSE,
    radius = 7,
    title =  NULL,
    subtitle =  NULL,
    caption =  NULL,
    fill_opacity = 1,
    colors = NULL,
    marks = c(".", ","),
    nDigits = NULL,
    tiles = NULL,
    legend_show = TRUE,
    legend_size = 13,
    legend_position = 'bottomleft'
  )
  dopts
}




getOpts <- function(opts = NULL){

  userOpts <- opts
  defaultOpts <- getDefaultOpts()

  if(!is.null(opts)){
    opts <- modifyList(defaultOpts, userOpts)
  }else{
    opts <- defaultOpts
  }

  opts
}



