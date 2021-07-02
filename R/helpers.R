#' Legend by palette type
lflt_palette <- function(opts) {

  l <- list(
    palette = opts$palette,
    domain = opts$domain,
    na.color = opts$na_color
  )

  if (opts$color_scale %in% c("Category", "Custom")) {
    color_mapping <- "colorFactor"
  } else if (opts$color_scale == "Quantile") {
    color_mapping <- "colorQuantile"
    l <- modifyList(l, list(n = opts$n_quantile))
  } else if (opts$color_scale == 'Bins') {
    color_mapping <- "colorBin"
    l <- modifyList(l, list(bins = opts$n_bins,
                            pretty = opts$pretty))
  } else {
    color_mapping <- "colorNumeric"
  }

  # Add leaflet namespace
  color_mapping <- paste0("leaflet::",color_mapping)

  do.call(getfun(color_mapping), l)
}


agg_tooltip <- function(data, label_by, nms, label_ftype, tooltip) {
  if (is.null(data)) stop("There is not a data")
  data_format <- data %>%
    dplyr::mutate(labels = ifelse(is.na(a),
                                  glue::glue(paste0("<span style='font-size:13px;'><strong>{", label_by,"_label}</strong></span>")) %>% lapply(htmltools::HTML),
                                  glue::glue(
                                    dsvizprep::tooltip_map(nms = nms,
                                                 label_ftype = label_ftype,
                                                 tooltip = tooltip)) %>%
                                    lapply(htmltools::HTML))
    )
  data_format
}


lflt_legend_bubbles <- function(map, colors, labels, sizes,
                                title, na.label, position, opacity){
  colorAdditions <- paste0(colors, ";border-radius: 50%; width:", sizes, "px; height:", sizes, "px;")
  labelAdditions <- paste0("<div style='display: inline-block; height: ",
                           max(sizes), "px; margin-bottom: 3px; line-height: ", max(sizes), "px; font-size: 13px; '>",
                           makeup::makeup_num(labels), "</div>")

  return(leaflet::addLegend(map, colors = colorAdditions, labels = labelAdditions,
                            opacity = opacity, title = title, na.label = na.label,
                            position = position))
}


#'
lflt_legend_format <- function (prefix = "",
                                suffix = "",
                                between = " &ndash; ",
                                sample = "1,324.2",
                                locale = NULL,
                                transform = identity)
{
  formatNum <- function(x) {
    makeup::makeup_num(transform(x), sample)#, locale = locale)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      paste0(prefix, formatNum(cuts), suffix)
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]),
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n],
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}


lflt_base_map <- function(topoinfo, opts, ...) {

  lf <- leaflet::leaflet(topoinfo,
                         option = leaflet::leafletOptions(zoomControl = opts$map_zoom, minZoom = opts$min_zoom, maxZoom = 18)) %>%
    # addTopoJSON(geoinfo,
    #             weight = opts$border_weight,
    #             fillOpacity = opts$topo_fill_opacity,
    #             opacity = 1,
    #             color = opts$border_color,
    #             fillColor = opts$color_map) %>%
    leaflet::addPolygons(weight = opts$border_weight,
                         label = ~labels,
                         color =  opts$border_color,
                         fillColor = opts$color_map,
                         fillOpacity = opts$topo_fill_opacity,
                         opacity = 1)
  lf
}

#' Basic layer choroplets
lflt_basic_choropleth <- function(l) {

  color_map <- l$theme$na_color

  lf <- lflt_base_map(l$topoInfo,
                      opts = list(
                        map_zoom = l$theme$map_zoom,
                        min_zoom = l$min_zoom,
                        border_weight = l$theme$border_weight,
                        topo_fill_opacity = l$theme$topo_fill_opacity,
                        border_color = l$border_color,
                        color_map = color_map
                      ))
  #print("value" %in% names(l$topoInfo))
  if ("value" %in% names(l$topoInfo)) {
    domain <- l$topoInfo[["value"]]#l$d %>% drop_na(value) %>% .$value
    if(l$color_scale == "Custom"){
      intervals <- calculate_custom_intervals(cutoff_points = l$cutoff_points, domain = domain)
      domain <- intervals
    }


    opts_pal <- list(color_scale = l$color_scale,
                     palette = l$palette_colors,
                     # sequential = l$palette_colors_sequential,
                     # divergent = l$palette_colors_divergent,
                     na_color = l$theme$na_color,
                     domain = domain,
                     n_bins = l$n_bins,
                     n_quantile = l$n_quantile,
                     pretty = l$bins_pretty)

    pal <- lflt_palette(opts_pal)

    color_map <- pal(domain)

    fill_opacity <- l$theme$topo_fill_opacity
    if (is(l$topoInfo[["value"]], "numeric")){
      fill_opacity <- scales::rescale(l$topoInfo[["value"]], to = c(0.5, 1))
    }


    lf <- lf %>%
      leaflet::addPolygons( weight = l$theme$border_weight,
                            fillOpacity = fill_opacity,
                            opacity = 1,
                            color = l$border_color,
                            fillColor = color_map,
                            layerId = ~a,
                            label = ~labels,
                            labelOptions = leaflet::labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                  textsize = "13px", direction = "auto")#,
                            # highlight = highlightOptions(
                            #   color= 'white',
                            #   opacity = 0.8,
                            #   weight= 3,
                            #   bringToFront = TRUE)
      )
    if ( l$theme$legend_show) {
    legend_position <- l$theme$legend_position %||% "topright"
    if (legend_position == "bottom") legend_position <- "bottomright"
    if (legend_position == "top") legend_position <- "topright"

      lf <- lf %>% leaflet::addLegend(pal = pal, values = domain, opacity = 1,
                                      position = legend_position,
                                      na.label = l$na_label,
                                      title = l$legend_title,
                                      labFormat = lflt_legend_format(
                                        sample =l$format_num, locale = l$locale,
                                        prefix = l$prefix, suffix = l$suffix,
                                        between = paste0(l$suffix, " - ", l$prefix),
                                      ))
    }
  }
  lf
}

# If map_legend_bubble if TRUE the legend will be represented with circles
legend_format <- function(map, map_legend_bubble = FALSE, opts, ...) {

  if (is.null(map)) stop("There is no map")

  legend_position <- opts$legend_position %||% "topright"
  if (legend_position == "bottom") legend_position <- "bottomright"
  if (legend_position == "top") legend_position <- "topright"

  if (map_legend_bubble){
    lf <- map %>% lflt_legend_bubbles(sizes = 2*scales::rescale(opts$cuts, to = c(opts$min_size, opts$max_size)),
                                      labels = opts$cuts,
                                      color = legend_color,
                                      opacity = 1,
                                      position = opts$legend_position,
                                      na.label = opts$na_label,
                                      title = opts$legend_title)
  } else {
    lf <- map %>% leaflet::addLegend(pal = opts$pal, values = ~value, opacity = 1,
                                     position = legend_position,
                                     na.label = opts$na_label,
                                     title = opts$legend_title,
                                     labFormat = lflt_legend_format(
                                       sample = opts$format_num, locale = opts$locale,
                                       prefix = opts$prefix, suffix = opts$suffix,
                                       between = paste0(opts$suffix, " - ",opts$prefix),
                                     ))
  }
  lf
}


#' Basic layer bubbles
lflt_basic_bubbles <- function(l) {

  color_map <- l$theme$na_color

  # base map
  lf <- lflt_base_map(l$topoInfo,
                      opts = list(
                        map_zoom = l$theme$map_zoom,
                        min_zoom = l$min_zoom,
                        border_weight = l$theme$border_weight,
                        topo_fill_opacity = l$theme$topo_fill_opacity,
                        border_color = l$border_color,
                        color_map = color_map
                      ))

  if ("value" %in% names(l$topoInfo)) {

    radius <- 0
    color <- l$palette_colors[1]
    legend_color <- color

    if (is(l$topoInfo$value, "numeric")){
      radius <- scales::rescale(l$topoInfo$value, to = c(l$min_size, l$max_size))
      opts_pal <- list(color_scale = l$color_scale,
                       palette = l$palette_colors,
                       na_color = l$theme$na_color,
                       domain = l$topoInfo$value,
                       n_bins = l$n_bins,
                       n_quantile = l$n_quantile)
      pal <- lflt_palette(opts_pal)
      color <- pal(l$topoInfo[["value"]])
      legend_color <- l$palette_colors[1]
      cuts <- create_legend_cuts(l$topoInfo$value)
    } else if (is(l$topoInfo$value, "character")){
      radius <- ifelse(!is.na(l$topoInfo$value), 5, 0)
      opts_pal <- list(color_scale = l$color_scale,
                       palette = l$palette_colors,
                       na_color = l$theme$na_color,
                       domain = l$topoInfo$value,
                       n_bins = l$n_bins,
                       n_quantile = l$n_quantile)
      pal <- lflt_palette(opts_pal)
      color <- pal(l$topoInfo$value)
    }

    lon <- as.numeric(l$topoInfo$lon)
    lat <- as.numeric(l$topoInfo$lat)

    lon[is.na(radius)] <- NA
    lat[is.na(radius)] <- NA

    lf <- lf %>%
      leaflet::addCircleMarkers(
        lng = lon,
        lat = lat,
        radius = radius,
        color = color,
        stroke = l$map_stroke,
        fillOpacity = l$bubble_opacity,
        label = ~labels,
        labelOptions = leaflet::labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
        layerId = ~a
      )

    if (l$theme$legend_show){
      lf <- legend_format(map = lf, opts = list(min_size = l$min_size,
                                                max_size = l$max_size,
                                                cuts = cuts,
                                                legend_color = unique(color)[1],
                                                legend_position = l$theme$legend_position,
                                                na_label = l$na_label,
                                                legend_title = l$legend_title,
                                                pal = pal,
                                                legend_position = l$theme$legend_position,
                                                na_label = l$na_label,
                                                legend_title = l$legend_title,
                                                format_num = l$format_num,
                                                locale = l$locale,
                                                prefix = l$prefix,
                                                suffix = l$suffix))
    }
  }

  lf
}


#' Basic layer points
lflt_basic_points <- function(l) {

  color_map <- l$theme$na_color
  # base map
  lf <- lflt_base_map(l$topoInfo$topoInfo %||% l$topoInfo,
                      opts = list(
                        map_zoom = l$theme$map_zoom,
                        min_zoom = l$min_zoom,
                        border_weight = l$theme$border_weight,
                        topo_fill_opacity = l$theme$topo_fill_opacity,
                        border_color = l$border_color,
                        color_map = color_map
                      ))

  if (is.null(l$topoInfo$topoInfo)) return(lf)
  if ("value" %in% names(l$topoInfo$data)) {

    radius <- 0
    color <- l$palette_colors[1]
    legend_color <- color

    if (is(l$topoInfo$data$value, "numeric")){
      radius <- scales::rescale(l$topoInfo$data$value, to = c(l$min_size, l$max_size))
      opts_pal <- list(color_scale = l$color_scale,
                       palette = l$palette_colors,
                       na_color = l$theme$na_color,
                       domain = l$topoInfo$data$value,
                       n_bins = l$n_bins,
                       n_quantile = l$n_quantile)
      pal <- lflt_palette(opts_pal)
      color <- pal(l$topoInfo$data[["value"]])
      #legend_color <- "#505050"
      cuts <- suppressWarnings(create_legend_cuts(l$topoInfo$data$value))
    } else { #if (is(l$data$value, "character")){
      radius <- ifelse(!is.na(l$topoInfo$data$value), 5, 0)
      opts_pal <- list(color_scale = "Custom",
                       palette = l$palette_colors,
                       na_color = l$theme$na_color,
                       domain = unique(l$topoInfo$data$value),
                       n_bins = l$n_bins,
                       n_quantile = l$n_quantile)
      pal <- lflt_palette(opts_pal)
      color <- pal(l$topoInfo$data[["value"]])
    }

    lf <- lf %>%
      leaflet::addCircleMarkers(
        lng = l$topoInfo$data$a,
        lat = l$topoInfo$data$b,
        radius = radius,
        color = color,
        stroke = l$map_stroke,
        fillOpacity = l$bubble_opacity,
        label = l$topoInfo$data$labels,
        layerId = l$topoInfo$data$a
      )


    if (l$theme$legend_show){
      lf <- legend_format(map = lf,
                          map_legend_bubble = ifelse(is(l$topoInfo$data$value, "character"), FALSE, TRUE),
                          opts = list(min_size = l$min_size,
                                      max_size = l$max_size,
                                      cuts = cuts,
                                      legend_color = unique(color)[1],
                                      legend_position = l$theme$legend_position,
                                      na_label = l$na_label,
                                      legend_title = l$legend_title,
                                      pal = pal,
                                      legend_position = l$theme$legend_position,
                                      na_label = l$na_label,
                                      legend_title = l$legend_title,
                                      format_num = l$format_num,
                                      locale = l$locale,
                                      prefix = l$prefix,
                                      suffix = l$suffix))
    }
  }

  lf
}





url_logo <- function(logo, background_color) {
  if (grepl("http", logo)) logo_url <- logo
  logo_path <- dsvizopts::local_logo_path(logo, background_color)
  logo_url <- knitr::image_uri(f = logo_path)
  logo_url
}

#' Background and branding Map
lflt_background <- function(map, theme) {
  #print(theme$map_provider_tile)
  if (is.null(theme$map_tiles) & theme$map_provider_tile == "leaflet") {
    lf <- map %>% leaflet.extras::setMapWidgetStyle(list(background = theme$background_color))
  } else {
    if (theme$map_provider_tile == "leaflet") {
      lf <- map %>% leaflet::addProviderTiles(theme$map_tiles)
    } else {
      lf <- map %>% leaflet.esri::addEsriBasemapLayer(leaflet.esri::esriBasemapLayers[[theme$map_tiles_esri]])
      if (!is.null(theme$map_extra_layout)) {
        lf <- lf %>%
          leaflet.esri::addEsriFeatureLayer(
            url = theme$map_extra_layout,
            labelProperty = theme$map_name_layout)
      }
    }
  }
  if (theme$branding_include) {
    img <- url_logo(logo = theme$logo, background = theme$background_color)
    lf <- lf %>%
      leafem::addLogo(img, width = theme$logo_width, height = theme$logo_height, position = "bottomright")
  }
  lf
}

#' Set the bounds of map
lflt_bounds <- function(map, b_box) {

  map %>%
    leaflet::fitBounds(b_box[1], b_box[2], b_box[3], b_box[4])
}

#' Graticule map
lflt_graticule <- function(map, graticule) {

  if (graticule$map_graticule) {
    map <- map %>%
      leaflet::addGraticule(interval = graticule$map_graticule_interval,
                            style = list(color = graticule$map_graticule_color, weight = graticule$map_graticule_weight))
  }
  map
}

#' titles map
lflt_titles <- function(map, titles) {

  map %>%
    leaflet::addControl(titles$caption,
                        position = "bottomleft",
                        className="map-caption") %>%
    leaflet::addControl(titles$title,
                        position = "topleft",
                        className="map-title") %>%
    leaflet::addControl(titles$subtitle,
                        position = "topleft",
                        className="map-subtitle")

}

