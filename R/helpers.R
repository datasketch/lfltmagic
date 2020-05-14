
#'
#'@export
lf_polygons <- function(topoInfo, opts_data, opts_theme) {

  print(opts_theme)

  if ("b" %in% names(topoInfo$d@data)) {
  pal <- colorNumeric(
    palette = c("#FEAFEA", "#000AAA"),
    domain = topoInfo$d@data$b, na.color = "blue" )
  cs <- pal(topoInfo$d@data$b)
  } else {
  cs <- "red"
  }
  # print(pal)
  l <- leaflet(topoInfo$d) %>%
    addPolygons( weight = opts_theme$border_weight,
                 opacity = 1,
                 color = cs,
                 fillOpacity = 0.7,#opts_theme$topo_fill_opacity,
                 label = ~labels
    )

  if (is.null(opts_theme$map_tiles)) {
    l <- l %>% setMapWidgetStyle(list(background = opts_theme$background_color))
  } else {
    l <- l %>%  addProviderTiles(opts_theme$map_tiles)
  }

  legend_position <- opts_theme$legend_verticalAlign
 print(legend_position)
  l #%>%
    # addLegend("bottomright", pal = pal, values = topoInfo@data$b,
    #           title = "",
    #           labFormat = labelFormat(prefix = "$", suffix = "#"),
    #           opacity = 1,
    #           na.label = "bla"#opts_data$na_label
    # )
}
