#' Leaflet bubbles geo code num
#'
#' Leaflet bubbles
#'
#' @name lflt_bubbles_GcdNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' lflt_bubbles_GcdNum(sampleData("Gcd-Num", nrow = 10))
lflt_bubbles_GcdNum <- function(data = NULL,
                                mapName = "world_countries",
                                opts = NULL, ...) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }
  opts <- getOpts(opts = opts)

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""


  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    d <- d %>% drop_na()
    d <- d %>% filter(b >= 0)

    d <- d  %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = agg(opts$agg, b))

    d$code_alt <- iconv(tolower(d$a), to = "ASCII//TRANSLIT")

    lfmap <- geodataMeta(mapName)
    centroides <- file.path("geodata",lfmap$geoname,paste0(lfmap$basename,".csv"))
    centroides <- read_csv(system.file(centroides,package = "geodata"))
    centroides$code_alt <- iconv(tolower(centroides$id), to = "ASCII//TRANSLIT")

    d <- left_join(d, centroides)

    if (is.null(opts$nDigits)) opts$nDigits <- 2
    d$b <- round(d$b, opts$nDigits)

    if (is.null(opts$colors)) {
      colorDefault <- c("#3DB26F")
    } else {
      colorDefault <- opts$colors
    }


    topoData <- readLines(geodataTopojsonPath(mapName)) %>% paste(collapse = "\n")

    lf <-  leaflet(data = d) %>%
      addTopoJSON(topoData,
                  weight = opts$borderWidth,
                  color = opts$border_color,
                  fill = FALSE)

    if (is.null(opts$prefix)) opts$prefix <- ""
    if (is.null(opts$suffix)) opts$suffix <- ""

    if (opts$percentage & opts$suffix == "") {
      d$b <- (d$b/sum(d$b))*100
      opts$suffix <- "%"
    }

    prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)

    labels <- sprintf(
      paste0('<b>', d$a, '</b></br>','<b>', prefix_agg, ' ', nms[2],': </b>', opts$prefix ,format(d$b, big.mark = opts$marks[1],small.mark = opts$marks[2]), opts$suffix
      )) %>% lapply(htmltools::HTML)

    lf <- lf %>%
      addCircleMarkers(
        lng = d$lon,
        lat = d$lat,
        radius = scales::rescale(d$b, to =c(opts$min_radius, opts$max_radius)),
        color = colorDefault,
        stroke = opts$stroke,
        fillOpacity = opts$fill_opacity,
        label = labels,
        layerId = opts$shinyId
      )
  } else {
    topoData <- readLines(geodataTopojsonPath(mapName)) %>% paste(collapse = "\n")

    lf <-  leaflet() %>%
      addTopoJSON(topoData,
                  weight = opts$borderWidth,
                  color = opts$border_color,
                  fill = FALSE)
  }


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
               className="map-caption")
}


#' Leaflet bubbles geo code
#'
#' Leaflet bubbles
#'
#' @name lflt_bubbles_Gcd
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd
#' @export
#' @examples
#' lflt_bubbles_Gcd(sampleData("Gcd", nrow = 10))
lflt_bubbles_Gcd <- function(data = NULL,
                             mapName = "world_countries",
                             opts = NULL, ...) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d

    d <- d %>%
      dplyr::group_by_all() %>%
      dplyr::summarise(b = n())

    prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)

    names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label))
  }

  lflt_bubbles_GcdNum(data = data, mapName = mapName, opts = opts)
}



#' Leaflet bubbles geo name num
#'
#' Leaflet bubbles
#'
#' @name lflt_bubbles_GnmNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Num
#' @export
#' @examples
#' lflt_bubbles_GnmNum(sampleData("Gnm-Num", nrow = 10))
lflt_bubbles_GnmNum <- function(data = NULL,
                                mapName = "world_countries",
                                opts = NULL, ...) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }
  opts <- getOpts(opts = opts)

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""


  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    d <- d %>% drop_na()
    d <- d %>% filter(b >= 0)

    d <- d %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = agg(opts$agg, b))

    d$name_alt <- iconv(tolower(d$a), to = "ASCII//TRANSLIT")

    lfmap <- geodataMeta(mapName)
    centroides <- file.path("geodata",lfmap$geoname,paste0(lfmap$basename,".csv"))
    centroides <- read_csv(system.file(centroides,package = "geodata"))
    centroides$name_alt <- iconv(tolower(centroides$name), to = "ASCII//TRANSLIT")

    d <- left_join(d, centroides)

    if (is.null(opts$nDigits)) opts$nDigits <- 2
    d$b <- round(d$b, opts$nDigits)

    if (is.null(opts$colors)) {
      colorDefault <- c("#3DB26F")
    } else {
      colorDefault <- opts$colors
    }


    topoData <- readLines(geodataTopojsonPath(mapName)) %>% paste(collapse = "\n")

    lf <-  leaflet(data = d) %>%
      addTopoJSON(topoData,
                  weight = opts$borderWidth,
                  color = opts$border_color,
                  fill = FALSE)

    if (is.null(opts$prefix)) opts$prefix <- ""
    if (is.null(opts$suffix)) opts$suffix <- ""

    if (opts$percentage & opts$suffix == "") {
      d$b <- (d$b/sum(d$b))*100
      opts$suffix <- "%"
    }
    prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)

    labels <- sprintf(
      paste0('<b>', d$a, '</b></br>','<b>', prefix_agg, ' ', nms[2],': </b>', opts$prefix ,format(d$b, big.mark = opts$marks[1],small.mark = opts$marks[2]), opts$suffix
      )) %>% lapply(htmltools::HTML)

    lf <- lf %>%
      addCircleMarkers(
        lng = d$lon,
        lat = d$lat,
        radius = scales::rescale(d$b, to =c(opts$min_radius, opts$max_radius)),
        color = colorDefault,
        stroke = opts$stroke,
        fillOpacity = opts$fill_opacity,
        label = labels,
        layerId = opts$shinyId
      )
  } else {
    topoData <- readLines(geodataTopojsonPath(mapName)) %>% paste(collapse = "\n")

    lf <-  leaflet() %>%
      addTopoJSON(topoData,
                  weight = opts$borderWidth,
                  color = opts$border_color,
                  fill = FALSE)
  }


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
               className="map-caption")
}

#' Leaflet bubbles geo name
#'
#' Leaflet bubbles
#'
#' @name lflt_bubbles_Gnm
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' lflt_bubbles_Gnm(sampleData("Gnm", nrow = 10))
lflt_bubbles_Gnm <- function(data = NULL,
                             mapName = "world_countries",
                             opts = NULL, ...) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d

    d <- d %>%
      dplyr::group_by_all() %>%
      dplyr::summarise(b = n())

    prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)

    names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label))
  }

  lflt_bubbles_GnmNum(data = data, mapName = mapName, opts = opts)
}



#' Leaflet bubbles by category
#'
#' Leaflet bubbles by category
#'
#' @name lflt_bubbles_CatGlnGlt
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' lflt_bubbles_CatGlnGlt(sampleData("Cat-Gln-Glt", nrow = 10))
lflt_bubbles_CatGlnGlt <- function(data = NULL,
                                   mapName = "world_countries",
                                   opts = NULL, ...) {

  if (is.null(data) & is.null(mapName)) return("You must call a data or mapName argument")

  opts <- getOpts(opts = opts)

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""


  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    d <- d %>% drop_na(b, c)

    if (opts$dropNa) {
      d <- d %>%drop_na()
    } else {
      d$a[is.na(d$a)] <- 'N.A'
    }

    if (is.null(opts$colors)) {
      colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
    } else {
      colorDefault <- opts$colors
    }
    colorDefault <- discreteColor(colorDefault, d)
    categorias <- unique(d$a)

    d <- sp::SpatialPointsDataFrame(
      cbind(
        d$b,  # lng
        d$c  # lat
      ),
      data.frame(type = factor(
        d$a
      ))
    )


    if (!is.null(mapName)) {

      if (!mapName %in% availableGeodata()) {
        stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
      }

      topoData <- readLines(geodataTopojsonPath(mapName)) %>% paste(collapse = "\n")
      lf <-  leaflet(data = d) %>%
        addTopoJSON(topoData,
                    weight = opts$borderWidth,
                    color = opts$border_color,
                    fill = FALSE)
    } else {
      lf <- leaflet(data = d) %>% addTiles()
    }

    pal <- colorFactor(colorDefault, domain = categorias)

    lf <- lf %>%
      addCircleMarkers(
        radius = opts$radius,
        color = ~pal(type),
        stroke = opts$stroke,
        fillOpacity = opts$fill_opacity,
        label = ~type,
        layerId = opts$shinyId
      )

    if(opts$legend_show) {
      lf <- lf %>%
        addLegendCustom(colors = colorDefault,
                        labels = sort(categorias),
                        sizes = rep(opts$legend_size, length(colorDefault)),
                        position = opts$legend_position)
    }
  } else {
    if (!is.null(mapName)) {
      if (!mapName %in% availableGeodata()) {
        stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
      }

      topoData <- readLines(geodataTopojsonPath(mapName)) %>% paste(collapse = "\n")

      lf <-  leaflet() %>%
        addTopoJSON(topoData,
                    weight = opts$borderWidth,
                    color = opts$border_color,
                    fill = FALSE)

    }
  }


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
               className="map-caption")
}

#' Leaflet bubbles
#'
#' Leaflet bubbles
#'
#' @name lflt_bubbles_GlnGlt
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt
#' @export
#' @examples
#' lflt_bubbles_GlnGlt(sampleData("Gln-Glt", nrow = 10))
lflt_bubbles_GlnGlt <- function(data = NULL,
                                mapName = "world_countries",
                                opts = NULL, ...) {

  if (is.null(data) & is.null(mapName)) return("You must call a data or mapName argument")

  opts <- getOpts(opts = opts)

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""


  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    d <- d %>% drop_na()

    if (is.null(opts$nDigits)) opts$nDigits <- 2
    d$a <- round(d$a, opts$nDigits)
    d$b <- round(d$b, opts$nDigits)

    if (is.null(opts$colors)) {
      colorDefault <- c("#3DB26F")
    } else {
      colorDefault <- opts$colors
    }

    if (!is.null(mapName)) {

      if (!mapName %in% availableGeodata()) {
        stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
      }

      topoData <- readLines(geodataTopojsonPath(mapName)) %>% paste(collapse = "\n")
      lf <-  leaflet(data = d) %>%
        addTopoJSON(topoData,
                    weight = opts$borderWidth,
                    color = opts$border_color,
                    fill = FALSE)
    } else {
      lf <- leaflet(data = d) %>% addTiles()
    }



    labels <- sprintf(
      paste0('<p><b>lng: </b>', format(d$a, big.mark = opts$marks[1],small.mark = opts$marks[2]),'</p><p><b>lat: </b>', format(d$b, big.mark = opts$marks[1], small.mark = opts$marks[2]),'</p>'
      )) %>% lapply(htmltools::HTML)

    lf <- lf %>%
      addCircleMarkers(
        lng = d$a,
        lat = d$b,
        radius = opts$radius,
        color = colorDefault,
        stroke = opts$stroke,
        fillOpacity = opts$fill_opacity,
        label = labels,
        layerId = opts$shinyId
      )
  } else {
    if (!is.null(mapName)) {
      if (!mapName %in% availableGeodata()) {
        stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
      }

      topoData <- readLines(geodataTopojsonPath(mapName)) %>% paste(collapse = "\n")

      lf <-  leaflet() %>%
        addTopoJSON(topoData,
                    weight = opts$borderWidth,
                    color = opts$border_color,
                    fill = FALSE)

    }
  }


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
               className="map-caption")
}


