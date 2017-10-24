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
    dplyr::summarise(c = n())
  dd <- dd %>%
    dplyr::group_by(a) %>%


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
