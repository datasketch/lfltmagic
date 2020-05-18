
#' Leaflet choropleths by numerical variable
#'
#' Leaflet choropleths by numerical variable
#'
#' @name lflt_choropleth_GnmNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Num
#' @export
#' @examples
#' lflt_choropleth_GnmNum(sampleData("Gnm-Num", nrow = 10))
lflt_choropleth_GnmNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts)
  #print(l$d)

  qpal <- colorBin("RdYlBu", l$d@data$b, bins = 5)#colorQuantile("RdYlBu", l$d@data$b, n = 5)

  lf <- leaflet(l$d, option=leafletOptions(zoomControl= l$theme$map_zoom)) %>%
    addPolygons( weight = 2,#opts_theme$border_weight,
                 fillOpacity = 0.7,#opts_theme$topo_fill_opacit
                 opacity = 1,
                 color = ~qpal(b),
                 label = ~labels
    ) %>%
    addLegend(pal = qpal, values = ~b, opacity = 1,
              na.label = l$na_label,
              title = l$titles$legend_title,
              labFormat = labelFormat(
                prefix = l$prefix, suffix = l$suffix
              ))


  # if (is.null(l$theme$map_tiles)) {
  #   lf <- lf %>% setMapWidgetStyle(list(background = l$theme$background_color))
  # } else {
  #   lf <- lf %>%  addProviderTiles(l$theme$map_tiles)
  # }
  #
  # if (l$graticule$map_graticule) {
  #   lf <- lf %>%
  #     addGraticule(interval = l$graticule$map_graticule_interval,
  #                  style = list(color = l$graticule$map_graticule_color, weight = l$graticule$graticule_weight))
  # }
  #
  # lf <- lf  %>%
  #   fitBounds(l$b_box[1], l$b_box[2], l$b_box[3], l$b_box[4]) %>%
  #   addControl(l$caption,
  #              position = "bottomleft",
  #              className="map-caption") %>%
  #   addControl(l$title,
  #              position = "topleft",
  #              className="map-title")
  #
  # if (l$theme$branding_include) {
  #   img <- local_logo_path(logo = l$theme$logo, background = l$theme$background_color)
  #   lf <- lf %>%
  #     leafem::addLogo(img, src = "local", width = 150, height = 40, position = "bottomright")
  # }
 lf
}
