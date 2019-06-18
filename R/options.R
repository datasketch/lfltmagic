getDefaultOpts <- function(...){

  dopts <- list(
    nLevels = 5,
    labelWrap = 12,
    projection_name = NULL,
    projection_ratio = NULL,
    projection_type = NULL,
    projection_orientation = c(90, 0, 0),
    dropNa = FALSE,
    naColor = "#cccccc",
    graticule = FALSE,
    graticule_color = '#cccccc',
    graticule_interval = 50,
    graticule_weight = 1,
    border_color = "#2d2d2d",
    border_width = 1, #cambiar borderWidth
    #border_opacity = 1, #cambiar  borderOpacity
    opacity = 0.7,
    stroke = FALSE,
    radius = 7,
    min_radius = 1,
    max_radius = 10,
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
    legend_position = 'bottomleft',
    percentage = FALSE,
    suffix = NULL,
    prefix = NULL,
    agg = "sum",
    agg_text = NULL,
    scale = "discrete",
    count = TRUE,
    default_color = "transparent",
    zoom = 5
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



