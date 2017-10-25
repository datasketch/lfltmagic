#' Leaflet choropleths by geographical code
#'
#' Leaflet choropleths by geographical code
#'
#' @name lflt_choropleth_Gcd
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd
#' @export
#' @examples
#' lflt_choropleth_Gcd(sampleData("Gcd", nrow = 10))
lflt_choropleth_Gcd <- function(data,
                                palette = c("#009EE3", "#9B71AF", "#FFFF99"),
                                fillOpacity = 0.5,
                                #infoVar = NULL,
                                label = NULL,
                                popup = NULL,
                                scope = "world_countries",
                                tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    dgeo@data <- dgeo@data %>%
      dplyr::left_join(dd, by = c(id = "a"))
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    #lab <- paste0(dgeo@data$id, ": ", dgeo@data$b)
    lab <- map(as.list(1:nrow(dgeo@data)), function(r) {
      htmltools::HTML(paste0("<b>", dgeo@data$id[r], "</b><br/>", dgeo@data$b[r]))
    })
  } else {
    lab <- dgeo@data[[label]]
  }
  if (popup %in% nms) {
    popup <- dgeo@data[[popup]]
  }

  col <- colorNumeric(palette = palette, domain = NULL)

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addPolygons(fillColor = col(dgeo@data$b),
                label = lab,
                popup = popup,
                fillOpacity = fillOpacity,
                stroke = FALSE)
  l
}


#' Leaflet choropleths by categorical variable
#'
#' Leaflet choropleths by categorical variable
#'
#' @name lflt_choropleth_GcdCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Cat
#' @export
#' @examples
#' lflt_choropleth_GcdCat(sampleData("Gcd-Cat", nrow = 10))
lflt_choropleth_GcdCat <- function(data,
                                   palette = c("#009EE3", "#9B71AF", "#FFFF99"),
                                   fillOpacity = 0.5,
                                   #infoVar = NULL,
                                   label = NULL,
                                   popup = NULL,
                                   scope = "world_countries",
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::arrange(desc(c)) %>%
    dplyr::slice(1)


  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    dgeo@data <- dgeo@data %>%
      dplyr::left_join(dd, by = c(id = "a"))
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    #lab <- paste0(dgeo@data$id, ": ", dgeo@data$b)
    lab <- map(as.list(1:nrow(dgeo@data)), function(r) {
      htmltools::HTML(paste0("<b>", dgeo@data$id[r], "</b><br/>", nms[2], ": ", dgeo@data$b[r], "<br/>", dgeo@data$c[r]))
    })
  } else {
    lab <- dgeo@data[[label]]
  }
  if (popup %in% nms) {
    popup <- dgeo@data[[popup]]
  }

  col <- colorNumeric(palette = palette, domain = NULL)

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addPolygons(fillColor = col(dgeo@data$c),
                label = lab,
                popup = popup,
                fillOpacity = fillOpacity,
                stroke = FALSE)
  l
}


#' Leaflet choropleths by numerical variable
#'
#' Leaflet choropleths by numerical variable
#'
#' @name lflt_choropleth_GcdNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' lflt_choropleth_GcdNum(sampleData("Gcd-Num", nrow = 10))
lflt_choropleth_GcdNum <- function(data,
                                   palette = c("#009EE3", "#9B71AF", "#FFFF99"),
                                   fillOpacity = 0.5,
                                   #infoVar = NULL,
                                   label = NULL,
                                   popup = NULL,
                                   agg = "sum",
                                   scope = "world_countries",
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = do.call(agg, list(b, na.rm = TRUE)))


  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    dgeo@data <- dgeo@data %>%
      dplyr::left_join(dd, by = c(id = "a"))
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    #lab <- paste0(dgeo@data$id, ": ", dgeo@data$b)
    lab <- map(as.list(1:nrow(dgeo@data)), function(r) {
      htmltools::HTML(paste0("<b>", dgeo@data$id[r], "</b><br/>", nms[2], ": ", dgeo@data$b[r]))
    })
  } else {
    lab <- dgeo@data[[label]]
  }
  if (popup %in% nms) {
    popup <- dgeo@data[[popup]]
  }

  col <- colorNumeric(palette = palette, domain = NULL)

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addPolygons(fillColor = col(dgeo@data$b),
                label = lab,
                popup = popup,
                fillOpacity = fillOpacity,
                stroke = FALSE)
  l
}

#' Leaflet choropleths by geographical name
#'
#' Leaflet choropleths by geographical name
#'
#' @name lflt_choropleth_Gnm
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' lflt_choropleth_Gnm(sampleData("Gnm", nrow = 10))
lflt_choropleth_Gnm <- function(data,
                                palette = c("#009EE3", "#9B71AF", "#FFFF99"),
                                fillOpacity = 0.5,
                                #infoVar = NULL,
                                label = NULL,
                                popup = NULL,
                                scope = "world_countries",
                                tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    if ("altnames" %in% names(geodata::geodataMeta(scope))) {
      alt <- geodata::geodataMeta(scope)$altnames %>%
        na.omit()
      dgeo@data <- dd %>%
        fuzzyjoin::stringdist_left_join(alt,
                                        by = c(a = "altname"),
                                        ignore_case = TRUE,
                                        method = "jw",
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::right_join(dgeo@data, by = "id")
    } else {
      dgeo@data <- dd %>%
        fuzzyjoin::stringdist_right_join(dgeo@data,
                                         by = c(a = "name"),
                                         ignore_case = TRUE,
                                         method = "jw",
                                         max_dist = 0.5,
                                         distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::right_join(dgeo@data, by = c("name", "id"))
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
  }

  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    #lab <- paste0(dgeo@data$id, ": ", dgeo@data$b)
    lab <- map(as.list(1:nrow(dgeo@data)), function(r) {
      htmltools::HTML(paste0("<b>", dgeo@data$name[r], "</b><br/>", dgeo@data$b[r]))
    })
  } else {
    lab <- dgeo@data[[label]]
  }
  if (popup %in% nms) {
    popup <- dgeo@data[[popup]]
  }

  col <- colorNumeric(palette = palette, domain = NULL)

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addPolygons(fillColor = col(dgeo@data$b),
                label = lab,
                popup = popup,
                fillOpacity = fillOpacity,
                stroke = FALSE)
  l
}


#' Leaflet choropleths by categorical variable
#'
#' Leaflet choropleths by categorical variable
#'
#' @name lflt_choropleth_GnmCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Cat
#' @export
#' @examples
#' lflt_choropleth_GnmCat(sampleData("Gnm-Cat", nrow = 10))
lflt_choropleth_GnmCat <- function(data,
                                   palette = c("#009EE3", "#9B71AF", "#FFFF99"),
                                   fillOpacity = 0.5,
                                   #infoVar = NULL,
                                   label = NULL,
                                   popup = NULL,
                                   scope = "world_countries",
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::arrange(desc(c)) %>%
    dplyr::slice(1)


  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    if ("altnames" %in% names(geodata::geodataMeta(scope))) {
      alt <- geodata::geodataMeta(scope)$altnames %>%
        na.omit()
      dgeo@data <- dd %>%
        fuzzyjoin::stringdist_left_join(alt,
                                        by = c(a = "altname"),
                                        ignore_case = TRUE,
                                        method = "jw",
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::right_join(dgeo@data, by = "id")
    } else {
      dgeo@data <- dd %>%
        fuzzyjoin::stringdist_right_join(dgeo@data,
                                         by = c(a = "name"),
                                         ignore_case = TRUE,
                                         method = "jw",
                                         max_dist = 0.5,
                                         distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::right_join(dgeo@data, by = c("name", "id"))
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
  }


  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    #lab <- paste0(dgeo@data$id, ": ", dgeo@data$b)
    lab <- map(as.list(1:nrow(dgeo@data)), function(r) {
      htmltools::HTML(paste0("<b>", dgeo@data$name[r], "</b><br/>", nms[2], ": ", dgeo@data$b[r], "<br/>", dgeo@data$c[r]))
    })
  } else {
    lab <- dgeo@data[[label]]
  }
  if (popup %in% nms) {
    popup <- dgeo@data[[popup]]
  }

  col <- colorNumeric(palette = palette, domain = NULL)

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addPolygons(fillColor = col(dgeo@data$c),
                label = lab,
                popup = popup,
                fillOpacity = fillOpacity,
                stroke = FALSE)
  l
}


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
lflt_choropleth_GnmNum <- function(data,
                                   palette = c("#009EE3", "#9B71AF", "#FFFF99"),
                                   fillOpacity = 0.5,
                                   #infoVar = NULL,
                                   label = NULL,
                                   popup = NULL,
                                   agg = "sum",
                                   scope = "world_countries",
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = do.call(agg, list(b, na.rm = TRUE)))


  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    if ("altnames" %in% names(geodata::geodataMeta(scope))) {
      alt <- geodata::geodataMeta(scope)$altnames %>%
        na.omit()
      dgeo@data <- dd %>%
        fuzzyjoin::stringdist_left_join(alt,
                                        by = c(a = "altname"),
                                        ignore_case = TRUE,
                                        method = "jw",
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::right_join(dgeo@data, by = "id")
    } else {
      dgeo@data <- dd %>%
        fuzzyjoin::stringdist_right_join(dgeo@data,
                                         by = c(a = "name"),
                                         ignore_case = TRUE,
                                         method = "jw",
                                         max_dist = 0.5,
                                         distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::right_join(dgeo@data, by = c("name", "id"))
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
  }

  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    #lab <- paste0(dgeo@data$id, ": ", dgeo@data$b)
    lab <- map(as.list(1:nrow(dgeo@data)), function(r) {
      htmltools::HTML(paste0("<b>", dgeo@data$name[r], "</b><br/>", nms[2], ": ", dgeo@data$b[r]))
    })
  } else {
    lab <- dgeo@data[[label]]
  }
  if (popup %in% nms) {
    popup <- dgeo@data[[popup]]
  }

  col <- colorNumeric(palette = palette, domain = NULL)

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addPolygons(fillColor = col(dgeo@data$b),
                label = lab,
                popup = popup,
                fillOpacity = fillOpacity,
                stroke = FALSE)
  l
}
