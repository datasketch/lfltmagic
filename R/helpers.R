#' Legend by palette type
lflt_palette <- function(opts) {
  if (opts$color_scale == "Category") {
    color_mapping <- "colorFactor"
    # l <- list(levels = opts$levels,
    #           ordered = opts$ordered)
    l <- list()
  } else if (opts$color_scale == "Quantile") {
    color_mapping <- "colorQuantile"
    l <- list(n = opts$n_quantile)
  } else if (opts$color_scale == 'Bins') {
    color_mapping <- "colorBin"
    l <- list(bins = opts$n_bins,
              pretty = FALSE)
  } else {
    color_mapping <- "colorNumeric"
    l <- list()
  }

  l$palette <- opts$palette
  l$domain <- opts$domain
  l$na.color <- opts$na_color
  do.call(color_mapping, l)
}

#' labels
lflt_tooltip <- function(nms, tooltip, style) {
  if (is.null(nms)) stop("Enter names")
  nms_names <- names(nms)
  if (is.null(tooltip) | tooltip == "") {
    l <- map(seq_along(nms), function(i){
      paste0("<span style='font-size:15px;'><strong>", nms[[i]], ":</strong> {", nms_names[i], "_label}</span>")
    }) %>% unlist()
    tooltip <- paste0(l, collapse = "<br/>")
  } else {
    points <- gsub("\\{|\\}", "",
                   stringr::str_extract_all(tooltip, "\\{.*?\\}")[[1]])
    if (identical(points, character())) {
      tooltip <- tooltip
    } else {
      l <- purrr::map(1:length(points), function(i){
        true_points <-  paste0("{",names(nms[match(points[i], nms)]),"_label}")
        tooltip <<- gsub(paste0("\\{",points[i], "\\}"), true_points, tooltip)
      })[[length(points)]]}
  }
  tooltip
}

#' Format
lflt_format <- function(d, dic, nms, opts) {

  var_nums <- dic$id[dic$hdType %in% c("Num", "Gln", "Glt")]
  l_num <- map(var_nums, function(v) {
    d[[paste0(v, "_label")]] <<- ifelse(is.na(d[[v]]), NA,
                                        paste0(opts$prefix,
                                               makeup::makeup_num(v = d[[v]],
                                                                  opts$format_num_sample#,
                                                                  #locale = opts$locale
                                               ),
                                               opts$suffix))
  })
  var_cats <- dic$id[dic$hdType %in% c("Cat", "Gnm", "Gcd")]
  l_cat <- map(var_cats, function(v) {
    d[[paste0(v, "_label")]] <<- ifelse(is.na(d[[v]]), NA,
                                        makeup::makeup_chr(v = d[[v]],
                                                           opts$format_cat_sample))
  })
  d
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

#' Basic layer choroplets
lflt_basic_choropleth <- function(l) {

  color_map <- l$theme$na_color

  lf <- leaflet(l$d,
                option = leafletOptions(zoomControl= l$theme$map_zoom, minZoom = l$min_zoom, maxZoom = 18)) %>%
    addPolygons( weight = l$theme$border_weight,
                 fillOpacity = l$theme$topo_fill_opacity,
                 opacity = 1,
                 label = ~name,
                 color = l$border_color,
                 fillColor = color_map)

  if (!is.null(l$data)) {
    if(sum(is.na(l$d@data$b)) == nrow(l$d@data)) {
      lf <- lf
    } else {
    opts_pal <- list(color_scale = l$color_scale,
                     palette = l$theme$palette_colors,
                     na_color = l$theme$na_color,
                     domain = l$d@data[["b"]],
                     n_bins = l$n_bins,
                     n_quantile = l$n_quantile)
    pal <- lflt_palette(opts_pal)
    color_map <- pal(l$d@data[["b"]])

    fill_opacity <- l$theme$topo_fill_opacity
    if (is(l$d$b, "character")){
      fill_opacity <- scales::rescale(l$d$c, to = c(0.5, 1))
    }


    lf <- leaflet(l$d,
                  option = leafletOptions(zoomControl= l$theme$map_zoom, minZoom = l$min_zoom, maxZoom = 18)) %>%
      addPolygons( weight = l$theme$border_weight,
                   fillOpacity = fill_opacity,
                   opacity = 1,
                   color = l$border_color,
                   fillColor = color_map,
                   layerId = ~a,
                   label = ~labels
      )
    if (!is.null(l$data) & l$theme$legend_show) {
      lf <- lf %>% addLegend(pal = pal, values = ~b, opacity = 1,
                             position = l$theme$legend_position,
                             na.label = l$na_label,
                             title = l$legend_title,
                             labFormat = lflt_legend_format(
                               sample =l$format_num, locale = l$locale,
                               prefix = l$prefix, suffix = l$suffix,
                               between = paste0(l$suffix, " - ", l$prefix),
                             ))
    }
  }}
  lf
}


#' Basic layer points
lflt_basic_points <- function(l) {

  color_map <- l$theme$na_color
  lf <- leaflet(l$d,
                option = leafletOptions(zoomControl= l$theme$map_zoom, minZoom = l$min_zoom, maxZoom = 18)) %>%
    addPolygons( weight = l$theme$border_weight,
                 fillOpacity = l$theme$topo_fill_opacity,
                 opacity = 1,
                 label = ~labels,
                 color = l$border_color,
                 fillColor = color_map)

  if (!is.null(l$data)) {

    if (is(l$d$c, "character")){
      radius <- scales::rescale(l$d$d, to = c(l$min_size, l$max_size))
      opts_pal <- list(color_scale = l$color_scale,
                       palette = l$theme$palette_colors,
                       na_color = l$theme$na_color,
                       domain = l$d@data[["c"]],
                       n_bins = l$n_bins,
                       n_quantile = l$n_quantile)
      pal <- lflt_palette(opts_pal)
      color <- pal(l$d@data[["c"]])
    } else {
      radius <- scales::rescale(l$d$c, to = c(l$min_size, l$max_size))
      color <- l$theme$palette_colors[1]
    }

    lf <- leaflet(l$d,
                  option = leafletOptions(zoomControl= l$theme$map_zoom, minZoom = l$min_zoom, maxZoom = 18)) %>%
      addPolygons( weight = l$theme$border_weight,
                   fillOpacity = l$theme$topo_fill_opacity,
                   opacity = 1,
                   color = l$border_color,
                   fillColor = color_map) %>%
      addCircleMarkers(
        lng = ~a,
        lat = ~b,
        radius = radius,
        color = color,
        stroke = l$map_stroke,
        fillOpacity = l$bubble_opacity,
        label = ~labels,
        layerId = ~a
      )

    if (is(l$d$c, "character") & l$theme$legend_show) {
      lf <- lf %>% addLegend(pal = pal, values = ~c, opacity = 1,
                             position = l$theme$legend_position,
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



#' Basic layer bubbles
lflt_basic_bubbles <- function(l) {

  color_map <- l$theme$na_color

  lf <- leaflet(l$d,
                option = leafletOptions(zoomControl= l$theme$map_zoom, minZoom = l$min_zoom, maxZoom = 18)) %>%
    addPolygons( weight = l$theme$border_weight,
                 fillOpacity = l$theme$topo_fill_opacity,
                 opacity = 1,
                 label = ~name,
                 color = l$border_color,
                 fillColor = color_map)
  if (!is.null(l$data)) {

    if (is(l$d$b, "character")){
      radius <- scales::rescale(l$d$c, to = c(l$min_size, l$max_size))
      opts_pal <- list(color_scale = l$color_scale,
                       palette = l$theme$palette_colors,
                       na_color = l$theme$na_color,
                       domain = l$d@data[["b"]],
                       n_bins = l$n_bins,
                       n_quantile = l$n_quantile)
      pal <- lflt_palette(opts_pal)
      color <- pal(l$d@data[["b"]])
    } else {
      radius <- scales::rescale(l$d$b, to = c(l$min_size, l$max_size))
      color <- l$theme$palette_colors[1]
    }


    lf <- lf %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = radius,
        color = color,
        stroke = l$map_stroke,
        fillOpacity = l$bubble_opacity,
        label = ~labels,
        layerId = ~a
      )

  if (is(l$d$b, "character") & l$theme$legend_show) {
    lf <- lf %>% addLegend(pal = pal, values = ~b, opacity = 1,
                           position = l$theme$legend_position,
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


url_logo <- function(logo, background_color) {
  if (grepl("http", logo)) logo_url <- logo
  logo_path <- local_logo_path(logo, background_color)
  logo_url <- knitr::image_uri(f = logo_path)
  logo_url
}

#' Background and branding Map
lflt_background <- function(map, theme) {

  if (is.null(theme$map_tiles)) {
    lf <- map %>% setMapWidgetStyle(list(background = theme$background_color))
  } else {
    lf <- map %>%  addProviderTiles(theme$map_tiles)
  }
  if (theme$branding_include) {
    img <- url_logo(logo = theme$logo, background = theme$background_color)
    lf <- lf %>%
      leafem::addLogo(img,  width = theme$logo_width, height = theme$logo_height, position = "bottomright")
  }
  lf
}

#' Set the bounds of map
lflt_bounds <- function(map, b_box) {
  map %>%
    fitBounds(b_box[1], b_box[2], b_box[3], b_box[4])
}

#' Graticule map
lflt_graticule <- function(map, graticule) {

  if (graticule$map_graticule) {
    map <- map %>%
      addGraticule(interval = graticule$map_graticule_interval,
                   style = list(color = graticule$map_graticule_color, weight = graticule$map_graticule_weight))
  }
  map
}

#' titles map
lflt_titles <- function(map, titles) {

  map %>%
    addControl(titles$caption,
               position = "bottomleft",
               className="map-caption") %>%
    addControl(titles$title,
               position = "topleft",
               className="map-title") %>%
    addControl(titles$subtitle,
               position = "topleft",
               className="map-subtitle")

}


# Find name or id
#' @export
geoType <- function(data, map_name) {

  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  d <- homodatum::fringe_d(f)

  lfmap <- geodataMeta(map_name)
  centroides <- data_centroid(lfmap$geoname, lfmap$basename)
  vs <- NULL
  values <- intersect(d[["a"]], centroides[["id"]])

  if (identical(values, character(0))) {
    values <- intersect(d[["a"]], centroides[["name"]])
    if(!identical(values, character())) vs <- "Gnm"
  } else {
    vs <- "Gcd"
  }
  vs
}


# fake data
#' @export
fakeData <- function(map_name = NULL, ...) {
  if (is.null(map_name)) return()
  lfmap <- geodataMeta(map_name)
  centroides <- data_centroid(lfmap$geoname, lfmap$basename)
  d <- data.frame(name =sample(centroides$name, 11), fake_value = rnorm(11, 33, 333))
  d
}

