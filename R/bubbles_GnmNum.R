
#' Leaflet bubbless by numerical variable
#'
#' Leaflet bubbless by numerical variable
#'
#' @name lflt_bubbles_GnmNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Num
#' @export
#' @examples
#' lflt_bubbles_GnmNum(sampleData("Gnm-Num", nrow = 10))
lflt_bubbles_GnmNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, type = "bubbles")
print(l$d@data)
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px;border-radius: 50%;")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")

  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}

  lf <- leaflet(l$d, option=leafletOptions(zoomControl= l$theme$map_zoom))%>%
    addPolygons( weight = 2,#opts_theme$border_weight,
                 fillOpacity = 0.7,#opts_theme$topo_fill_opacit
                 opacity = 1
    ) %>%
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = ~scales::rescale(b, to =c(1, 10)),
      color = ~..colors,
      #stroke = opts$stroke,
      fillOpacity = 1,#opts$fill_opacity,
      label = ~labels,
      layerId = ~a
    ) %>%
    addLegendCustom(colors = c("yellow", "yellow", "red"), labels = c("Cat1", "Cat2", "Cat3"), sizes = c(10, 20, 40))


  if (is.null(l$theme$map_tiles)) {
    lf <- lf %>% setMapWidgetStyle(list(background = l$theme$background_color))
  } else {
    lf <- lf %>%  addProviderTiles(l$theme$map_tiles)
  }

  if (l$graticule$map_graticule) {
    lf <- lf %>%
      addGraticule(interval = l$graticule$map_graticule_interval,
                   style = list(color = l$graticule$map_graticule_color, weight = l$graticule$graticule_weight))
  }

  lf <- lf  %>%
    fitBounds(l$b_box[1], l$b_box[2], l$b_box[3], l$b_box[4]) %>%
    addControl(l$caption,
               position = "bottomleft",
               className="map-caption") %>%
    addControl(l$title,
               position = "topleft",
               className="map-title")

  if (l$theme$branding_include) {
    img <- local_logo_path(logo = l$theme$logo, background = l$theme$background_color)
    lf <- lf %>%
      leafem::addLogo(img, src = "local", width = 150, height = 40, position = "bottomright")
  }
   lf
}
