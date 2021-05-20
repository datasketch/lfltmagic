

#' Legend by palette type
lflt_palette <- function(opts) {
  if (opts$color_scale %in% c("Category", "Custom")) {
    color_mapping <- "colorFactor"
    # l <- list(levels = opts$levels, ordered = opts$ordered)
    l <- list()
  } else if (opts$color_scale == "Quantile") {
    color_mapping <- "colorQuantile"
    l <- list(n = opts$n_quantile)
  } else if (opts$color_scale == 'Bins') {
    color_mapping <- "colorBin"
    l <- list(bins = opts$n_bins,
              pretty = opts$pretty)
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
lflt_tooltip <- function(nms,label_ftype, tooltip) {
  if (is.null(nms)) stop("Enter names")
  nms <- nms
  nms <- gsub("[][!()*`|]", "",nms)
  nms <- gsub("[\r\n]", " ", nms)
  label_ftype_clean <- gsub("[][!()*`|{}]", "", label_ftype)
  label_ftype_clean <- gsub("[\r\n]", " ", label_ftype_clean)
  nms_names <- names(nms)

  if (is.null(tooltip)) tooltip <- ""
  if (tooltip == "") {
    nms <-  nms[names(nms) %in% label_ftype]
    nms_names <- names(nms)
    l <- map(seq_along(nms), function(i){
      paste0("<span style='font-size:15px;'><strong>", nms[[i]], ":</strong> {", nms_names[i], "_label}</span>")
    }) %>% unlist()
    tooltip <- paste0(l, collapse = "<br/>")
  } else {
    tt <- gsub("[][()*`|]", "", tooltip)#gsub("[][!#$()*,.:;<=>@^`|~.", "", tooltip)
  points <- gsub("\\{|\\}", "",
                 stringr::str_extract_all(tt, "\\{.*?\\}")[[1]])
  if (identical(points, character())) {
    tooltip <- tooltip
  } else {
    tooltip <- tt
    l <- purrr::map(seq_along(points), function(i){

      true_points <-  paste0("{",names(nms[match(points[i], nms)]),"_label}")
      tooltip <<- gsub(paste0("\\{",points[i], "\\}"), true_points, tooltip)
    })}
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


lflt_legend_bubbles <- function(map, colors, labels, sizes,
                                title, na.label, position, opacity){
  colorAdditions <- paste0(colors, ";border-radius: 50%; width:", sizes, "px; height:", sizes, "px;")
  labelAdditions <- paste0("<div style='display: inline-block; height: ",
                           max(sizes), "px; margin-bottom: 5px; line-height: ", max(sizes), "px; font-size: 15px; '>",
                           makeup::makeup_num(labels), "</div>")

  return(addLegend(map, colors = colorAdditions, labels = labelAdditions,
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

#' Basic layer choroplets
lflt_basic_choropleth <- function(l) {

  color_map <- l$theme$na_color

  lm <- leaflet(l$topoInfo,
                option = leafletOptions(zoomControl= l$theme$map_zoom, minZoom = l$min_zoom, maxZoom = 18)) %>%
    addTopoJSON(l$geoInfo,
                weight = l$theme$border_weight,
                fillOpacity = l$theme$topo_fill_opacity,
                opacity = 1,
                color = l$border_color,
                fillColor = color_map)
  print("value" %in% names(l$topoInfo))
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
    if (is(l$topoInfo[[3]], "numeric")){
      fill_opacity <- scales::rescale(l$topoInfo[["value"]], to = c(0.5, 1))
    }


    lf <- lm %>%
      addPolygons( weight = l$theme$border_weight,
                   fillOpacity = fill_opacity,
                   opacity = 1,
                   color = l$border_color,
                   fillColor = color_map,
                   layerId = ~a,
                   label = ~labels,
                   highlight = highlightOptions(
                     color= 'white',
                     opacity = 0.8,
                     weight= 3,
                     bringToFront = TRUE)
      )
    if ( l$theme$legend_show) {

      lf <- lf %>% addLegend(pal = pal, values = domain, opacity = 1,
                             position = l$theme$legend_position,
                             na.label = l$na_label,
                             title = l$legend_title,
                             labFormat = lflt_legend_format(
                               sample =l$format_num, locale = l$locale,
                               prefix = l$prefix, suffix = l$suffix,
                               between = paste0(l$suffix, " - ", l$prefix),
                             ))
    }
  } else {
    lf <- lm %>%
      addPolygons(weight = 0.5,
                  label = ~labels,
                  fillColor = color_map)
  }
  lf
}

#' Basic layer bubbles
lflt_basic_bubbles <- function(l) {

  color_map <- l$theme$na_color

  lf <- leaflet(l$topoInfo,
                option = leafletOptions(zoomControl= l$theme$map_zoom, minZoom = l$min_zoom, maxZoom = 18)) %>%
    addTopoJSON(l$geoInfo,
                weight = l$theme$border_weight,
                fillOpacity = l$theme$topo_fill_opacity,
                opacity = 1,
                color = color_map) %>%
    addPolygons(weight = 0.5,
                label = ~labels,
                color = color_map)

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
      addCircleMarkers(
        lng = lon,
        lat = lat,
        radius = radius,
        color = color,
        stroke = l$map_stroke,
        fillOpacity = l$bubble_opacity,
        label = ~labels,
        layerId = ~a
      )

    if (l$theme$legend_show){
      if (is(l$topoInfo$value, "numeric")){
        lf <- lf %>% lflt_legend_bubbles(sizes = 2*scales::rescale(cuts, to = c(l$min_size, l$max_size)),
                                         labels = cuts,
                                         color = legend_color,
                                         opacity = 1,
                                         position = l$theme$legend_position,
                                         na.label = l$na_label,
                                         title = l$legend_title)
      }

      if (is(l$topoInfo$value, "character")) {
        lf <- lf %>% addLegend(pal = pal, values = ~value, opacity = 1,
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
  lf <-  leaflet(l$topoInfo,
                 option = leafletOptions(zoomControl= l$theme$map_zoom, minZoom = l$min_zoom, maxZoom = 18)) %>%
    addTopoJSON(l$geoInfo,
                weight = l$theme$border_weight,
                fillOpacity = l$theme$topo_fill_opacity,
                opacity = 1,
                color = color_map) %>%
    addPolygons(weight = 0.5,
                label = ~labels,
                color = color_map)

  if ("value" %in% names(l$data)) {

    radius <- 0
    color <- l$palette_colors[1]
    legend_color <- color

    if (is(l$data$value, "numeric")){
      radius <- scales::rescale(l$data$value, to = c(l$min_size, l$max_size))
      opts_pal <- list(color_scale = l$color_scale,
                       palette = l$palette_colors,
                       na_color = l$theme$na_color,
                       domain = l$data$value,
                       n_bins = l$n_bins,
                       n_quantile = l$n_quantile)
      pal <- lflt_palette(opts_pal)
      color <- pal(l$data[["value"]])
      legend_color <- "#505050"
      cuts <- create_legend_cuts(l$data$value)
    } else { #if (is(l$data$value, "character")){
      radius <- ifelse(!is.na(l$data$value), 5, 0)
      opts_pal <- list(color_scale = "Custom",
                       palette = l$palette_colors,
                       na_color = l$theme$na_color,
                       domain = unique(l$data$value),
                       n_bins = l$n_bins,
                       n_quantile = l$n_quantile)
      pal <- lflt_palette(opts_pal)
      color <- pal(l$data[["value"]])
    }

    lf <- lf %>%
      addCircleMarkers(
        lng = l$data$a,
        lat = l$data$b,
        radius = radius,
        color = color,
        stroke = l$map_stroke,
        fillOpacity = l$bubble_opacity,
        label = l$data$labels,
        layerId = l$data$a
      )


    if (l$theme$legend_show){

      if (is(l$data$value, "numeric")){
        lf <- lf %>% lflt_legend_bubbles(sizes = 2*scales::rescale(cuts, to = c(l$min_size, l$max_size)),
                                         labels = cuts,
                                         color = legend_color,
                                         opacity = 1,
                                         position = l$theme$legend_position,
                                         na.label = l$na_label,
                                         title = l$legend_title)
      }

      if (is(l$data$value, "character")) {
        lf <- lf %>% addLegend(pal = pal, values = l$data$value, opacity = 1,
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
  #print(theme$map_provider_tile)
  if (is.null(theme$map_tiles) & theme$map_provider_tile == "leaflet") {
    lf <- map %>% setMapWidgetStyle(list(background = theme$background_color))
  } else {
    if (theme$map_provider_tile == "leaflet") {
      lf <- map %>% addProviderTiles(theme$map_tiles)
    } else {
      lf <- map %>% leaflet.esri::addEsriBasemapLayer(esriBasemapLayers[[theme$map_tiles_esri]])
      if (!is.null(theme$map_extra_layout)) {
        lf <- lf %>%
          addEsriFeatureLayer(
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

  if (identical(values, character(0))||identical(values, numeric(0))) {
    values <- intersect(d[["a"]], centroides[["name"]])
    if(!identical(values, character())) vs <- "Gnm"
  } else {
    vs <- "Gcd"
  }
  vs
}


# fake data
#' @export
fakeData <- function(map_name = NULL, by = "name", ...) {
  if (is.null(map_name)) return()
  centroides <- suppressWarnings(geodataMeta(map_name)$codes)
  names_centroides <- names(centroides)
  diff_names <- setdiff(names_centroides, c("id", "name", "lat", "lon"))
  nsample <- nrow(centroides)
  if (nsample > 30) nsample <- 30
  centroides <- centroides[sample(1:nrow(centroides), nsample),]
  if (by == "name" & !(identical(diff_names, character()))) {
    d <- data.frame(name = centroides[[by]],
                    name_addition = centroides[[diff_names]], sample_value = runif(nsample, 33, 333))
  } else {
    d <- data.frame(name = sample(centroides[[by]], nsample), sample_value = runif(nsample, 33, 333))
  }
  d
}


# fake data points
#' @export
fakepoints <- function(map_name = NULL, ...) {
  if (is.null(map_name)) return()
  lfmap <- geodataMeta(map_name)
  centroides <- data_centroid(lfmap$geoname, lfmap$basename)

  nsample <- nrow(centroides)
  if (nsample > 30) nsample <- 30
  d <- data.frame(lon = sample(centroides$lon, nsample),
                  lat = sample(centroides$lat, nsample),
                  dim_sample = abs(rnorm(nsample, 33, 333)))
  d
}

#
#' standar dataset
#' @export
standar_values <- function(data) {
  l <- map(colnames(data), function(i) iconv(tolower(data[[i]]), to = "ASCII//TRANSLIT"))
  names(l) <- names(data)
  l <- l %>% bind_rows()
  l
}

# find geo code or geo name
#' @export
find_geoinfo <- function(data, centroides) {


  centroides <- centroides %>% select(-lat, -lon)
  centroides <- standar_values(centroides)
  dic_info <- data.frame(names_centroides = c("id", "name", "name_addition", "code_addition"),
                         ftype = c("Gcd", "Gnm", "Gnm", "Gcd"))

  info_data <- paste0("^", map(colnames(centroides),
                               function (i) {unique(centroides[[i]])
                               }) %>% unlist(), "$", collapse = "|")

  data <- standar_values(data)


  l <- sapply(colnames(data), function(x) {
    search_info <- !identical(grep(info_data, as.matrix(data[,x])), integer(0)) == TRUE
  })
  r <- names(l)[l == TRUE]
  if (identical(r, character(0))) {
    return() # if is null is not finding consciousness
  } else{
    r # return names of date with geo code o geoname
  }

}


# guess ftypes changed cat by Gnm or Gcd
#' @export
guess_ftypes <- function(data, map_name) {
  #data <- sample_data("Glt-Gln-Num-Cat-Num-Num-Cat")
  if (is.null(map_name))
    stop("Please type a map name")
  if (is.null(data)) return()

  f <- fringe(data)
  d <- homodatum::fringe_d(f)
  dic <- homodatum::fringe_dic(f)
  dic$id <- names(d)

  centroides <- suppressWarnings(geodataMeta(map_name)$codes)
  centroides$id <- iconv(tolower(centroides$id), to = "ASCII//TRANSLIT")
  dif_names <- setdiff(names(centroides), c("id", "name", "lat", "lon"))


  if (!identical(dif_names, character())) {
    col_names <- c("name", dif_names)
    centroides$name <- iconv(tolower(centroides$name), to = "ASCII//TRANSLIT")
    centroides[[dif_names]] <- iconv(tolower(centroides[[dif_names]]), to = "ASCII//TRANSLIT")
  } else {
    col_names <- c("name")
    centroides$name <- iconv(tolower(centroides$name), to = "ASCII//TRANSLIT")
  }
  var_geo <- find_geoinfo(as.data.frame(data), centroides)
  d <- data[var_geo]
  d <- standar_values(d)

  info_gcd <- paste0("^", centroides$id, "$", collapse = "|")
  l <- sapply(colnames(d), function(x) {
    search_info <- !identical(grep(info_gcd, as.matrix(d[,x])), integer(0)) == TRUE
  })
  r <- names(l)[l == TRUE]

  if (!identical(r, character(0))) {
    if(!all(is.na(suppressWarnings(as.numeric(centroides$id))))) {
      max_gcd <- max(centroides$id)
      d_gcd <- r %>%
        map(function(i){max(d[[i]], na.rm = TRUE) <= max(centroides$id)}) %>% unlist()
      r <- r[d_gcd]
    }
    ld<- map(r, function(i) {
      dic$hdType[dic$label == i] <<- "Gcd"
    })
  }


  info_gnm <- paste0("^", map(col_names,
                              function (i) {unique(centroides[[i]])
                              }) %>% unlist(), "$", collapse = "|")
  l <- sapply(colnames(d), function(x) {
    search_info <- !identical(grep(info_gnm, as.matrix(d[,x])), integer(0)) == TRUE
  })
  r <- names(l)[l == TRUE]

  if (!identical(r, character(0))) {
    ld<- map(r, function(i) {
      dic$hdType[dic$label == i] <<- "Gnm"
    })
  }

  dic

}
