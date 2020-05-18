
#' Leaflet bubbless by numerical variable
#'
#' Leaflet bubbless by numerical variable
#'
#' @name lflt_bubbles_GlnGlt
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Num
#' @export
#' @examples
#' lflt_bubbles_GlnGlt(sampleData("Gnm-Num", nrow = 10))
lflt_bubbles_GlnGlt <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- lfltmagic_prep(data = data, opts = opts, type = "bubbles")
  print(l)#$d@data)
  # pal <- colorNumeric("red", 10)
  # lf <- leaflet(l$d, option=leafletOptions(zoomControl= l$theme$map_zoom))%>%
  #   addPolygons( weight = 2,#opts_theme$border_weight,
  #                fillOpacity = 0.7,#opts_theme$topo_fill_opacit
  #                opacity = 1
  #   ) %>%
  #   addCircleMarkers(
  #     lng = ~a,
  #     lat = ~b,
  #     radius = 5,
  #     color = ~pal(b),
  #     #stroke = opts$stroke,
  #     fillOpacity = 1,#opts$fill_opacity,
  #     #label = ~labels,
  #     layerId = ~a
  #   )
  #lf
}
