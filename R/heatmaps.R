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
                             gradient = NULL,
                             tiles = "CartoDB.Positron") {

  f <- fringe(data)
  nms <- getClabels(f)

  radius <- radius %||% 10
  gradient <- gradient %||% c("#009EE3", "#9B71AF", "#F72872", "#48239D")

  d <- f$d %>%
    na.omit()

  leaflet(d) %>%
    addProviderTiles(tiles) %>%
    addHeatmap(lat = ~a, lng = ~b,
               radius = radius,
               gradient = gradient)

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
#' lflt_heatmap_size_Gnm(sampleData("Gnm",nrow = 10))
lflt_heatmap_Gnm <- function(data,
                             radius = NULL,
                             gradient = NULL,
                             tiles = "CartoDB.Positron") {

  f <- fringe(data)
  nms <- getClabels(f)

  radius <- radius %||% 10
  gradient <- gradient %||% c("#009EE3", "#9B71AF", "#F72872", "#48239D")

  d <- f$d %>%
    na.omit()

  leaflet(d) %>%
    addProviderTiles(tiles) %>%
    addHeatmap(lat = ~a, lng = ~b,
               radius = radius,
               gradient = gradient)

}

####

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
#' lflt_heatmap_size_GlnGlt(sampleData("Gln-Glt",nrow = 10))
lflt_heatmap_GltGln <- function(data,
                                radius = NULL,
                                gradient = NULL,
                                tiles = "CartoDB.Positron") {

  f <- fringe(data)
  nms <- getClabels(f)

  radius <- radius %||% 10
  gradient <- gradient %||% c("#009EE3", "#9B71AF", "#F72872", "#48239D")

  d <- f$d %>%
    na.omit()

  leaflet(d) %>%
    addProviderTiles(tiles) %>%
    addHeatmap(lat = ~a, lng = ~b,
               radius = radius,
               gradient = gradient)

}



