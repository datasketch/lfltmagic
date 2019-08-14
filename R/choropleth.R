
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
lflt_choropleth_GnmNum <- function(data = NULL,
                                   mapName = "world_countries",
                                   opts = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  opts <- getOpts(opts = opts)

  title <-  opts$title %||% ""
  caption <- opts$caption %||% ""

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)

  if (opts$scale == 'discrete') {
    colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
  } else {
    colorDefault <- c("#f39ecb", "#53255E")
  }

  if (!is.null(opts$colors)) {
    opts$colors <- opts$colors
  } else {
    opts$colors <- colorDefault
  }

  lfmap <- geodataMeta(mapName)
  #topoInfo <- geojsonio::topojson_read(geodataTopojsonPath(mapName))
  topoInfo <- rgdal::readOGR(geodataTopojsonPath(mapName))

  centroides <- file.path("geodata",lfmap$geoname,paste0(lfmap$basename,".csv"))
  centroides <- read_csv(system.file(centroides,package = "geodata"))

  topoData <- suppressWarnings(
    readLines(geodataTopojsonPath(mapName))) %>% paste(collapse = "\n")
  b_box <- geojson::bbox_get(topoData)

  lf <- leaflet() %>%
    leaflet(options = leafletOptions(zoomControl = opts$zoom
    )) %>%
    addTopoJSON(topoData,
                weight = opts$border_width,
                color = opts$border_color,
                fill = FALSE)


  nDig <- opts$nDigits
  if (is.null(nDig)) nDig <- 0

  if (!is.null(data)) {

    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d


    d <- d  %>%
      tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                             b =  NA)) %>%
      tidyr::drop_na(a) %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = agg(opts$agg, b))

    if (opts$percentage) {
      d$b <- (d[['b']] * 100) / sum(d[['b']], na.rm = TRUE)
    }

    d$b <- round(d$b, nDig)


    pale <- opts$colors
    nBins <-  opts$nLevels

    d$name_alt <- iconv(tolower(d$a), to = "ASCII//TRANSLIT")
    topoInfo@data$name_alt <- iconv(tolower(topoInfo@data$name), to = "ASCII//TRANSLIT")
    topoInfo@data  <- left_join(topoInfo@data, d, by = "name_alt")

    if (opts$scale == "discrete") {
      pal <- colorBin(pale,
                      domain = topoInfo@data$b, na.color = opts$na_color,  bins = nBins)
    } else {
      pal <- colorNumeric(pale, domain = topoInfo@data$b, na.color = opts$na_color)
    }

    if (is.null(opts$prefix)) opts$prefix <- ""
    if (is.null(opts$suffix)) opts$suffix <- ""

    if (opts$percentage & opts$suffix == "") {
      opts$suffix <- "%"
    }

    dato <- ifelse(is.na(topoInfo@data$b), "",
                   paste0(prefix_agg, ' ', nms[2], ': ', opts$prefix , format(topoInfo@data$b, big.mark = opts$marks[1], decimal.mark = opts$marks[2], small.mark = opts$marks[2]), opts$suffix))

    if (opts$count) {
      labels <-
        paste0('<p><b>', topoInfo@data$name, '</b></br>',dato ,'</p>'
        ) %>% lapply(htmltools::HTML)
    } else {
      d$b <- 1
      labels <-
        paste0('<p><b>', topoInfo@data$name, '</b></br></p>'
        ) %>% lapply(htmltools::HTML)
    }

    lf  <-  leaflet(topoInfo, options = leafletOptions(zoomControl = opts$zoom)) %>%
      addPolygons(
        weight = opts$border_width,
        opacity = 1,
        color = opts$border_color,
        fillOpacity = opts$fill_opacity,
        fillColor = pal(topoInfo@data$b),
        layerId =  as.character(topoInfo@data$name),
        label = labels
      )

    if (opts$count) {
      if (opts$legend_show) {
      lf <- lf %>%
        addLegend(pal = pal,
                  values = ~b,
                  position = opts$legend_position,
                  opacity = 1.0,
                  bins = nBins,
                  title = opts$legend_title,
                  labFormat = labelFormat0(prefix = opts$prefix,
                                           suffix = opts$suffix,
                                           big.mark = opts$marks[1],
                                           decimal.mark = opts$marks[2],
                                           digits = nDig))
      }
    }

    lf

  }


  lf <- lf %>%
    setView(lng = mean(c(b_box[1],b_box[3])), lat = mean(c(b_box[2], b_box[4])), zoom = opts$zoom_level)


  if (!is.null(opts$tiles)) {
    lf <- lf %>%
      addProviderTiles(opts$tiles)
  }

  if (opts$graticule) {
    lf <- lf %>%
      addGraticule(interval = opts$graticule_interval,
                   style = list(color = opts$graticule_color, weight = opts$graticule_weight))
  }


  lf %>%
    addControl(caption,
               position = "bottomright",
               className="map-caption") %>%
    addControl(title,
               position = "topleft",
               className="map-title")

}

#' Leaflet choropleths by geo name
#'
#' Leaflet choropleths by geo name
#'
#' @name lflt_choropleth_Gnm
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' lflt_choropleth_Gnm(sampleData("Gnm", nrow = 10))
lflt_choropleth_Gnm <- function(data = NULL,
                                mapName = "world_countries",
                                opts = NULL) {

  if (is.null(data)) {
    d <- NULL
  } else {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d


    d <- d %>%
      dplyr::group_by_all() %>%
      dplyr::summarise(b = n())

    prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)

    names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label))
    opts$agg_text <- " "
  }
  lflt_choropleth_GnmNum(d, mapName = mapName, opts = opts)
}


#' Leaflet choropleths by code variable
#'
#' Leaflet choropleths by code variable
#'
#' @name lflt_choropleth_GcdNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' lflt_choropleth_GcdNum(sampleData("Gcd-Num", nrow = 10))
lflt_choropleth_GcdNum <- function(data = NULL,
                                   mapName = "world_countries",
                                   opts = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  opts <- getOpts(opts = opts)

  title <-  opts$title %||% ""
  caption <- opts$caption %||% ""

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)

  if (opts$scale == 'discrete') {
    colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
  } else {
    colorDefault <- c("#f39ecb", "#53255E")
  }

  if (!is.null(opts$colors)) {
    opts$colors <- opts$colors
  } else {
    opts$colors <- colorDefault
  }

  lfmap <- geodataMeta(mapName)
  #topoInfo <- geojsonio::topojson_read(geodataTopojsonPath(mapName))
  topoInfo <- rgdal::readOGR(geodataTopojsonPath(mapName))

  centroides <- file.path("geodata",lfmap$geoname,paste0(lfmap$basename,".csv"))
  centroides <- read_csv(system.file(centroides,package = "geodata"))

  topoData <- suppressWarnings(
    readLines(geodataTopojsonPath(mapName))) %>% paste(collapse = "\n")
  b_box <- geojson::bbox_get(topoData)

  lf <- leaflet() %>%
    leaflet(options = leafletOptions(zoomControl = opts$zoom
    )) %>%
    addTopoJSON(topoData,
                weight = opts$border_width,
                color = opts$border_color,
                fill = FALSE)


  nDig <- opts$nDigits
  if (is.null(nDig)) nDig <- 0

  if (!is.null(data)) {

    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d


    d <- d  %>%
      tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                             b =  NA)) %>%
      tidyr::drop_na(a) %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = agg(opts$agg, b))

    if (opts$percentage) {
      d$b <- (d[['b']] * 100) / sum(d[['b']], na.rm = TRUE)
    }

    d$b <- round(d$b, nDig)


    pale <- opts$colors
    nBins <-  opts$nLevels

    d$id_alt <- iconv(tolower(d$a), to = "ASCII//TRANSLIT")
    topoInfo@data$id_alt <- iconv(tolower(topoInfo@data$id), to = "ASCII//TRANSLIT")
    topoInfo@data  <- left_join(topoInfo@data, d, by = "id_alt")


    if (opts$scale == "discrete") {
      pal <- colorBin(pale,
                      domain = topoInfo@data$b, na.color = opts$na_color,  bins = nBins)
    } else {
      pal <- colorNumeric(pale, domain = topoInfo@data$b, na.color = opts$na_color)
    }

    if (is.null(opts$prefix)) opts$prefix <- ""
    if (is.null(opts$suffix)) opts$suffix <- ""

    if (opts$percentage & opts$suffix == "") {
      opts$suffix <- "%"
    }

    dato <- ifelse(is.na(topoInfo@data$b), "",
                   paste0(prefix_agg, ' ', nms[2], ': ', opts$prefix , format(topoInfo@data$b, big.mark = opts$marks[1], decimal.mark = opts$marks[2], small.mark = opts$marks[2]), opts$suffix))

    if (opts$count) {
      labels <-
        paste0('<p><b>', topoInfo@data$id, '</b></br>',dato ,'</p>'
        ) %>% lapply(htmltools::HTML)
    } else {
      d$b <- 1
      labels <-
        paste0('<p><b>', topoInfo@data$id, '</b></br></p>'
        ) %>% lapply(htmltools::HTML)
    }

    lf  <-  leaflet(topoInfo, options = leafletOptions(zoomControl = opts$zoom)) %>%
      addPolygons(
        weight = opts$border_width,
        opacity = 1,
        color = opts$border_color,
        fillOpacity = opts$fill_opacity,
        fillColor = pal(topoInfo@data$b),
        layerId =  as.character(topoInfo@data$id),
        label = labels
      )

    if (opts$count) {
      if (opts$legend_show) {
        lf <- lf %>%
          addLegend(pal = pal,
                    values = ~b,
                    position = opts$legend_position,
                    opacity = 1.0,
                    bins = nBins,
                    title = opts$legend_title,
                    labFormat = labelFormat0(prefix = opts$prefix,
                                             suffix = opts$suffix,
                                             big.mark = opts$marks[1],
                                             decimal.mark = opts$marks[2],
                                             digits = nDig))
      }
    }

    lf

  }


  lf <- lf %>%
    setView(lng = mean(c(b_box[1],b_box[3])), lat = mean(c(b_box[2], b_box[4])), zoom = opts$zoom_level)


  if (!is.null(opts$tiles)) {
    lf <- lf %>%
      addProviderTiles(opts$tiles)
  }

  if (opts$graticule) {
    lf <- lf %>%
      addGraticule(interval = opts$graticule_interval,
                   style = list(color = opts$graticule_color, weight = opts$graticule_weight))
  }


  lf %>%
    addControl(caption,
               position = "bottomright",
               className="map-caption") %>%
    addControl(title,
               position = "topleft",
               className="map-title")

}

#' Leaflet choropleths by geo code
#'
#' Leaflet choropleths by geo code
#'
#' @name lflt_choropleth_Gcd
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd
#' @export
#' @examples
#' lflt_choropleth_Gcd(sampleData("Gcd", nrow = 10))
lflt_choropleth_Gcd <- function(data = NULL,
                                mapName = "world_countries",
                                opts = NULL) {

  if (is.null(data)) {
    d <- NULL
  } else {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d


    d <- d %>%
      dplyr::group_by_all() %>%
      dplyr::summarise(b = n())

    prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)

    names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label))
    opts$agg_text <- " "
  }
  lflt_choropleth_GcdNum(d, mapName = mapName, opts = opts)
}



