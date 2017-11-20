#' Leaflet heatmap by geographical code
#'
#' Leaflet heatmap by geographical code
#'
#' @name lflt_heatmap_Gcd
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd
#' @export
#' @examples
#' lflt_heatmap_Gcd(sampleData("Gcd", nrow = 10))
lflt_heatmap_Gcd <- function(data,
                             radius = NULL,
                             blur = NULL,
                             gradient = NULL,
                             tiles = "CartoDB.Positron",
                             scope = "world_countries") {
  f <- fringe(data)
  nms <- getClabels(f)

  radius <- radius %||% 10
  blur <- blur %||% 10
  gradient <- gradient %||% c("#009EE3", "#9B71AF", "#F72872", "#48239D")

  dd <- f$d %>%
    na.omit()

  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    cent <- geodata::geodataMeta(scope)$codes
    dgeo <- dd %>%
      dplyr::left_join(cent, by = c(a = "id")) %>%
      na.omit()
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    leaflet.extras::addHeatmap(lat = ~as.numeric(lat), lng = ~as.numeric(lon),
                               radius = radius,
                               blur = blur,
                               gradient = gradient)
  l
}


#' Leaflet heatmap by geographical name
#'
#' Leaflet heatmap by geographical name
#'
#' lflt_heatmap_Gnm
#' Heatmap
#' @name lflt_heatmap_Gnm
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' lflt_heatmap_Gnm(sampleData("Gnm", nrow = 10))
lflt_heatmap_Gnm <- function(data,
                             radius = NULL,
                             blur = NULL,
                             gradient = NULL,
                             tiles = "CartoDB.Positron",
                             scope = "world_countries") {

  f <- fringe(data)
  nms <- getClabels(f)

  radius <- radius %||% 10
  blur <- blur%||% 10
  gradient <- gradient %||% c("#009EE3", "#9B71AF", "#F72872", "#48239D")

  dd <- f$d %>%
    na.omit()

  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    cent <- geodata::geodataMeta(scope)$codes
    if ("altnames" %in% names(geodata::geodataMeta(scope))) {
      alt <- geodata::geodataMeta(scope)$altnames %>%
        na.omit()
      dgeo <- dd %>%
        fuzzyjoin::stringdist_left_join(alt,
                                        by = c(a = "altname"),
                                        ignore_case = TRUE,
                                        method = "jw",
                                        max_dist = 4,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::left_join(cent, by = "id") %>%
        na.omit()
    } else {
      dgeo <- dd %>%
        dplyr::left_join(cent, by = c(a = "id")) %>%
        na.omit()
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    leaflet.extras::addHeatmap(lat = ~as.numeric(lat), lng = ~as.numeric(lon),
                               radius = radius,
                               blur = blur,
                               gradient = gradient)
  l
}


#' Leaflet heatmap by longitud and latitud
#'
#' Leaflet heatmap by longitud and latitud
#'
#' lflt_heatmap_GlnGlt
#' Heatmap
#' @name lflt_heatmap_GlnGlt
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt
#' @export
#' @examples
#' lflt_heatmap_GlnGlt(sampleData("Gln-Glt", nrow = 10))
lflt_heatmap_GlnGlt <- function(data,
                                radius = NULL,
                                blur = NULL,
                                gradient = NULL,
                                tiles = "CartoDB.Positron") {

  f <- fringe(data)
  nms <- getClabels(f)

  radius <- radius %||% 10
  blur <- blur %||% 10
  gradient <- gradient %||% c("#009EE3", "#9B71AF", "#F72872", "#48239D")

  dgeo <- f$d %>%
    na.omit()

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addHeatmap(lat = ~as.numeric(a), lng = ~as.numeric(b),
               radius = radius,
               blur = blur,
               gradient = gradient)
  l
}



