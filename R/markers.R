#' Leaflet markers by latitud and longitud
#'
#' Leaflet markers by latitud and longitud
#'
#' @name lflt_markers_GlnGlt
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt
#' @export
#' @examples
#' lflt_markers_GlnGlt(sampleData("Gln-Glt", nrow = 10))
lflt_markers_GlnGlt <- function(data,
                                #color = "navy",
                                #fillOpacity = 0.5,
                                #infoVar = NULL,
                                label = NULL,
                                popup = NULL,
                                icon = NULL,
                                iconWidth = 36,
                                iconHeight = 24,
                                tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dgeo <- f$d %>%
    na.omit()

  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dgeo[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dgeo[[label]]
  }
  if (popup %in% nms) {
    popup <- dgeo[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addMarkers(lat = ~b, lng = ~a,
               icon =  icons(iconUrl = icon, iconWidth = iconWidth, iconHeight = iconHeight),
               popup = popup,
               label = lab)
  l
}

#' Leaflet markers by latitud and longitud and icons
#'
#' Leaflet markers by latitud and longitud and icons
#'
#' @name lflt_markers_GlnGltImg
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt-Img
#' @export
#' @examples
#' lflt_markers_GlnGltImg(sampleData("Gln-Glt-Img", nrow = 10))
lflt_markers_GlnGltImg <- function(data,
                                   #color = "navy",
                                   #fillOpacity = 0.5,
                                   #infoVar = NULL,
                                   label = NULL,
                                   popup = NULL,
                                   iconWidth = 36,
                                   iconHeight = 24,
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dgeo <- f$d %>%
    na.omit()

  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dgeo[r, 1:(length(nms) - 1)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dgeo[[label]]
  }
  if (popup %in% nms) {
    popup <- dgeo[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addMarkers(lat = ~b, lng = ~a,
               icon =  icons(iconUrl = ~c, iconWidth = iconWidth, iconHeight = iconHeight),
               popup = popup,
               label = lab)
  l
}


#' Leaflet markers by latitud and longitud and icons by categorical variable
#'
#' Leaflet markers by latitud and longitud and icons by categorical variable
#'
#' @name lflt_markers_GlnGltCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt-Cat
#' @export
#' @examples
#' lflt_markers_GlnGltCat(sampleData("Gln-Glt-Cat", nrow = 10))
lflt_markers_GlnGltCat <- function(data,
                                   label = NULL,
                                   popup = NULL,
                                   icon = NULL,
                                   iconWidth = 36,
                                   iconHeight = 24,
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dgeo <- f$d %>%
    na.omit()
  if (!is.null(icon)) {
    n0 <- unique(dgeo$c)
    n1 <- data.frame("c" = n0, "d" = c(icon, rep("", length(n0) - length(icon))))
    dgeo <- dgeo %>%
      dplyr::left_join(n1, by = "c")
  } else {
    dgeo$d <- NA
  }
  names(dgeo)[4] <- "d"

##### FALTA HACER LSO DE LAS IMÁGENES POR CATEGORIA
  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dgeo[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dgeo[[label]]
  }
  if (popup %in% nms) {
    popup <- dgeo[[popup]]
  }

  View(dgeo)
  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addMarkers(lat = ~b, lng = ~a,
               icon =  icons(iconUrl = ~d, iconWidth = iconWidth, iconHeight = iconHeight),
               popup = popup,
               label = lab)
  l
}
