getDefaultOpts <- function(...){

  dopts <- list(
    agg = "sum",
    agg_text = NULL,
    border_color = "#2d2d2d",
    border_width = 1, #cambiar borderWidth
    #border_opacity = 1, #cambiar  borderOpacity
    caption =  NULL,
    colors = NULL,
    count = TRUE,
    default_color = "transparent",
    dropNa = FALSE,
    fill_opacity = 1,
    graticule = FALSE,
    graticule_color = '#cccccc',
    graticule_interval = 50,
    graticule_weight = 1,
    labelWrap = 12,
    legend_show = TRUE,
    legend_size = 13,
    legend_position = 'bottomleft',
    marks = c(".", ","),
    min_radius = 1,
    max_radius = 10,
    na_color = "#cccccc",
    nDigits = NULL,
    nLevels = 5,
    opacity = 0.7,
    percentage = FALSE,
    prefix = NULL,
    radius = 7,
    scale = "discrete",
    stroke = FALSE,
    subtitle =  NULL,
    suffix = NULL,
    tiles = NULL,
    title =  NULL,
    zoom = TRUE,
    zoom_level = 1
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



