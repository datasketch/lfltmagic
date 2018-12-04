#' Leaflet bubbles size by geographical code
#'
#' Leaflet bubbles size by geographical code
#'
#' @name lflt_bubbles_size_Gcd
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd
#' @export
#' @examples
#' lflt_bubbles_size_Gcd(sampleData("Gcd", nrow = 10))
lflt_bubbles_size_Gcd <- function(data,
                                  color = "navy",
                                  fillOpacity = 0.5,
                                  #infoVar = NULL,
                                  label = NULL,
                                  popup = NULL,
                                  minSize = 3,
                                  maxSize = 20,
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
    cent <- geodata::geodataMeta(scope)$codes
    dgeo <- dd %>%
      dplyr::left_join(cent, by = c(a = "id"))
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  #dgeo$info <- str_tpl_format(tpl, dgeo)

  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dd)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dd[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dd[[label]]
  }
  if (popup %in% nms) {
    popup <- dd[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~as.numeric(lat), lng = ~as.numeric(lon), weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(minSize, maxSize)),
                     popup = popup,
                     label = lab,
                     fillOpacity = fillOpacity,
                     color = color,
                     stroke = FALSE)
  l
}

#' Leaflet bubbles grouped by categorical variable
#'
#' Leaflet bubbles grouped by categorical variable
#'
#' @name lflt_bubbles_grouped_GcdCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Cat
#' @export
#' @examples
#' lflt_bubbles_grouped_GcdCat(sampleData("Gcd-Cat", nrow = 10))
lflt_bubbles_grouped_GcdCat <- function(data,
                                        palette = c("#009EE3", "#9B71AF", "#FFFF99"),
                                        fillOpacity = 0.5,
                                        #infoVar = NULL,
                                        label = NULL,
                                        popup = NULL,
                                        size = 5,
                                        scope = "world_countries",
                                        tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit()

  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    cent <- geodata::geodataMeta(scope)$codes
    dgeo <- dd %>%
      dplyr::left_join(cent, by = c(a = "id"))
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  col <- colorFactor(palette = palette, domain = NULL)

  # dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  # dd$info <- str_tpl_format(tpl,dd)

  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dd)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dd[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dd[[label]]
  }
  if (popup %in% nms) {
    popup <- dd[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~as.numeric(lat), lng = ~as.numeric(lon), weight = 3,
                     radius = size,
                     popup = popup,
                     label = lab,
                     fillOpacity = fillOpacity,
                     color = ~col(b),
                     stroke = FALSE)
  l
}


#' Leaflet bubbles size by categorical variable
#'
#' Leaflet bubbles size by categorical variable
#'
#' @name lflt_bubbles_size_GcdCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Cat
#' @export
#' @examples
#' lflt_bubbles_size_GcdCat(sampleData("Gcd-Cat", nrow = 10))
lflt_bubbles_size_GcdCat <- function(data,
                                     color = "navy",
                                     fillOpacity = 0.5,
                                     #infoVar = NULL,
                                     label = NULL,
                                     popup = NULL,
                                     minSize = 3,
                                     maxSize = 20,
                                     scope = "world_countries",
                                     tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n())

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    cent <- geodata::geodataMeta(scope)$codes
    dgeo <- dd %>%
      dplyr::left_join(cent, by = c(a = "id"))
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  #dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  #dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dd)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dd[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dd[[label]]
  }
  if (popup %in% nms) {
    popup <- dd[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~as.numeric(lat), lng = ~as.numeric(lon), weight = 3,
                     radius = ~scales::rescale(sqrt(c), to = c(minSize, maxSize)),
                     label = lab,
                     popup = popup,
                     fillOpacity = fillOpacity,
                     color = color,
                     stroke = FALSE)
  l
}


#' Leaflet bubbles size by numeric variable
#'
#' Leaflet bubbles size by numeric variable
#'
#' @name lflt_bubbles_size_GcdNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' lflt_bubbles_size_GcdNum(sampleData("Gcd-Num", nrow = 10))
lflt_bubbles_size_GcdNum <- function(data,
                                     color = "navy",
                                     #infoVar = NULL,
                                     fillOpacity = 0.5,
                                     label = NULL,
                                     popup = NULL,
                                     minSize = 3,
                                     maxSize = 20,
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
    # depende de lo que se decida con las advertencias
    #dplyr::filter(b >= 0)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    cent <- geodata::geodataMeta(scope)$codes
    dgeo <- dd %>%
      dplyr::left_join(cent, by = c(a = "id"))
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  #dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  #dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dd)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dd[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dd[[label]]
  }
  if (popup %in% nms) {
    popup <- dd[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~as.numeric(lat), lng = ~as.numeric(lon), weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(minSize, maxSize)),
                     popup = popup,
                     label = lab,
                     color = color,
                     fillOpacity = fillOpacity,
                     stroke = FALSE)
  l
}



#' Leaflet bubbles grouped by categorical variable size by numerical
#'
#' Leaflet bubbles grouped by categorical variable size by numerical
#'
#' @name lflt_bubbles_GcdCatNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Cat-Num
#' @export
#' @examples
#' lflt_bubbles_GcdCatNum(sampleData("Gcd-Cat-Num", nrow = 10))
lflt_bubbles_GcdCatNum <- function(data,
                                   palette = c("#009EE3", "#9B71AF", "#FFFF99"),
                                   fillOpacity = 0.5,
                                   #infoVar = NULL,
                                   label = NULL,
                                   popup = NULL,
                                   minSize = 3,
                                   maxSize = 20,
                                   agg = "sum",
                                   scope = "world_countries",
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = do.call(agg, list(c, na.rm = TRUE)))
    # falta decidir con las advertencias
    # dplyr::filter(c >= 0)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  if (!is.null(scope) && scope %in% geodata::availableGeodata()) {
    cent <- geodata::geodataMeta(scope)$codes
    dgeo <- dd %>%
      dplyr::left_join(cent, by = c(a = "id"))
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  col <- colorFactor(palette = palette, domain = NULL)

  #dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  #dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dd)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dd[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dd[[label]]
  }
  if (popup %in% nms) {
    popup <- dd[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~as.numeric(lat), lng = ~as.numeric(lon), weight = 3,
                     radius = ~scales::rescale(sqrt(c), to = c(minSize, maxSize)),
                     popup = popup,
                     label = lab,
                     color = ~col(b),
                     fillOpacity = fillOpacity,
                     stroke = FALSE)
  l
}


#' Leaflet bubbles size by geographical name
#'
#' Leaflet bubbles size by geographical name
#'
#' @name lflt_bubbles_size_Gnm
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' lflt_bubbles_size_Gnm(sampleData("Gnm", nrow = 10))
lflt_bubbles_size_Gnm <- function(data,
                                  color = "navy",
                                  fillOpacity = 0.5,
                                  #infoVar = NULL,
                                  label = NULL,
                                  popup = NULL,
                                  minSize = 3,
                                  maxSize = 20,
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
    cent <- geodata::geodataMeta(scope)$codes
    if ("altnames" %in% names(geodata::geodataMeta(scope))) {
      alt <- geodata::geodataMeta(scope)$altnames %>%
        na.omit()
      dgeo <- dd %>%
        fuzzyjoin::stringdist_left_join(alt,
                                        by = c(a = "altname"),
                                        ignore_case = TRUE,
                                        method = "jw",
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::left_join(cent, by = "id")
    } else {
      dgeo <- dd %>%
        fuzzyjoin::stringdist_left_join(cent,
                                        by = c(a = "name"),
                                        ignore_case = TRUE,
                                        method = "jw",
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist))
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
  }

  #dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  #dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dd)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dd[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dd[[label]]
  }
  if (popup %in% nms) {
    popup <- dd[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~as.numeric(lat), lng = ~as.numeric(lon), weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(minSize, maxSize)),
                     popup = popup,
                     fillOpacity = fillOpacity,
                     label = lab,
                     color = color,
                     stroke = FALSE)
  l
}


#' Leaflet bubbles grouped by categorical variable
#'
#' Leaflet bubbles grouped by categorical variable
#'
#' @name lflt_bubbles_grouped_GnmCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Cat
#' @export
#' @examples
#' lflt_bubbles_grouped_GnmCat(sampleData("Gnm-Cat", nrow = 10))
lflt_bubbles_grouped_GnmCat <- function(data,
                                        palette = c("#009EE3", "#9B71AF", "#FFFF99"),
                                        fillOpacity = 0.5,
                                        #infoVar = NULL,
                                        label = NULL,
                                        popup = NULL,
                                        size = 5,
                                        scope = "world_countries",
                                        tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit()

  col <- colorFactor(palette = palette, domain = NULL)

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
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::left_join(cent, by = "id")
    } else {
      dgeo <- dd %>%
        fuzzyjoin::stringdist_left_join(cent,
                                        by = c(a = "name"),
                                        ignore_case = TRUE,
                                        method = "jw",
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist))
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
  }
  #dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  #dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dd)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dd[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dd[[label]]
  }
  if (popup %in% nms) {
    popup <- dd[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~as.numeric(lat), lng = ~as.numeric(lon), weight = 3,
                     radius = size,
                     popup = popup,
                     fillOpacity = fillOpacity,
                     label = lab,
                     color = ~col(b),
                     stroke = FALSE)
  l
}

#' Leaflet bubbles size by categorical variable
#'
#' Leaflet bubbles size by categorical variable
#'
#' @name lflt_bubbles_size_GnmCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Cat
#' @export
#' @examples
#' lflt_bubbles_size_GnmCat(sampleData("Gnm-Cat", nrow = 10))
lflt_bubbles_size_GnmCat <- function(data,
                                     color = "navy",
                                     fillOpacity = 0.5,
                                     #infoVar = NULL,
                                     label = NULL,
                                     popup = NULL,
                                     minSize = 3,
                                     maxSize = 20,
                                     scope = "world_countries",
                                     tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n())

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
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::left_join(cent, by = "id")
    } else {
      dgeo <- dd %>%
        fuzzyjoin::stringdist_left_join(cent,
                                        by = c(a = "name"),
                                        ignore_case = TRUE,
                                        method = "jw",
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist))
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
  }

  #dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  #dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dd)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dd[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dd[[label]]
  }
  if (popup %in% nms) {
    popup <- dd[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~as.numeric(lat), lng = ~as.numeric(lon), weight = 3,
                     radius = ~scales::rescale(sqrt(c), to = c(minSize, maxSize)),
                     popup = popup,
                     fillOpacity = fillOpacity,
                     label = lab,
                     color = color,
                     stroke = FALSE)
  l
}


#' Leaflet bubbles size by numeric variable
#'
#' Leaflet bubbles size by numeric variable
#'
#' @name lflt_bubbles_size_GnmNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Num
#' @export
#' @examples
#' lflt_bubbles_size_GnmNum(sampleData("Gnm-Num", nrow = 10))
lflt_bubbles_size_GnmNum <- function(data,
                                     color = "navy",
                                     fillOpacity = 0.5,
                                     #infoVar = NULL,
                                     label = NULL,
                                     popup = NULL,
                                     minSize = 3,
                                     maxSize = 20,
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
    # falta decidir las advertencias
    #dplyr::filter(b >= 0)

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
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::left_join(cent, by = "id")
    } else {
      dgeo <- dd %>%
        fuzzyjoin::stringdist_left_join(cent,
                                        by = c(a = "name"),
                                        ignore_case = TRUE,
                                        method = "jw",
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist))
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
  }

  #dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  #dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dd)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dd[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dd[[label]]
  }
  if (popup %in% nms) {
    popup <- dd[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~as.numeric(lat), lng = ~as.numeric(lon), weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(minSize, maxSize)),
                     popup = popup,
                     fillOpacity = fillOpacity,
                     label = lab,
                     color = color,
                     stroke = FALSE)
  l
}


#' Leaflet bubbles grouped by categorical variable size by numerical
#'
#' Leaflet bubbles grouped by categorical variable size by numerical
#'
#' @name lflt_bubbles_GnmCatNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Cat-Num
#' @export
#' @examples
#' lflt_bubbles_GnmCatNum(sampleData("Gnm-Cat-Num", nrow = 10))
lflt_bubbles_GnmCatNum <- function(data,
                                   palette = c("#009EE3", "#9B71AF", "#FFFF99"),
                                   fillOpacity = 0.5,
                                   #infoVar = NULL,
                                   label = NULL,
                                   popup = NULL,
                                   minSize = 3,
                                   maxSize = 20,
                                   agg = "sum",
                                   scope = "world_countries",
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  popup <- popup %||% ""

  dd <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = do.call(agg, list(c, na.rm = TRUE)))
    # falta decidir las advertencias
    #dplyr::filter(c >= 0)

  col <- colorFactor(palette = palette, domain = NULL)

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
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::left_join(cent, by = "id")
    } else {
      dgeo <- dd %>%
        fuzzyjoin::stringdist_left_join(cent,
                                        by = c(a = "name"),
                                        ignore_case = TRUE,
                                        method = "jw",
                                        max_dist = 0.5,
                                        distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist))
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
  }

  #dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  #dgeo$info <- str_tpl_format(tpl,dgeo)
  # los labels y popups
  if (is.null(label) || !label %in% nms) {
    lab <- map(as.list(1:nrow(dd)), function(r) {
      htmltools::HTML(paste0("<b>", nms, ": </b>", dd[r, 1:length(nms)], "<br/>", collapse = ""))
    })
  } else {
    lab <- dd[[label]]
  }
  if (popup %in% nms) {
    popup <- dd[[popup]]
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~as.numeric(lat), lng = ~as.numeric(lon), weight = 3,
                     radius = ~scales::rescale(sqrt(c), to = c(minSize, maxSize)),
                     popup = popup,
                     fillOpacity = fillOpacity,
                     label = lab,
                     color = ~col(b),
                     stroke = FALSE)
  l
}


#' Leaflet bubbles by latitud and longitud
#'
#' Leaflet bubbles by latitud and longitud
#'
#' @name lflt_bubbles_GlnGlt
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt
#' @export
#' @examples
#' lflt_bubbles_GlnGlt(sampleData("Gln-Glt", nrow = 10))
lflt_bubbles_GlnGlt <- function(data,
                                caption = NULL,
                                count = TRUE,
                                border = list(),
                                fill = list(),
                                format = c("", ""),
                                labelWrap = 12,
                                marks = c(",", "."),
                                nDigits = 2,
                                label = NULL,
                                percentage = FALSE,
                                popup = NULL,
                                size = c(3, 20),
                                tiles = NULL) {
  fillDefault <- list(color = NULL,
                      opacity = 0.5,
                      scale = "discrete",
                      nullColor = "#dddddd")
  fill <- modifyList(fillDefault, fill)

  borderDefault <- list(weight = 1.3,
                        color = "black",
                        opacity = 1,
                        stroke = TRUE)
  border <- modifyList(borderDefault, border)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  if (count) {
    d <- d  %>%
      tidyr::replace_na(list(a = NA,
                             b = NA)) %>%
      # tidyr::drop_na() %>%
      dplyr::group_by(a, b) %>%
      dplyr::summarise(c = n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(d = seq_along(c),
                    percent = c * 100 / sum(c, na.rm = TRUE))

    d <- fillColors(d, "d", fill$color, fill$scale, NULL, NULL,
                    labelWrap = labelWrap, numeric = TRUE)
  } else {
    d <- d %>%
      tidyr::replace_na(list(a = NA,
                             b = NA)) %>%
      dplyr::group_by(a, b) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(c = 1,
                    d = seq_along(c))
    d <- fillColors(d, "d", fill$color, fill$scale, NULL, NULL,
                    labelWrap = labelWrap, numeric = TRUE)
  }

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }
  # los labels y popups
  if (is.null(label)) {
    label <- paste0("Lng: <b> {point.a} </b><br/> Lat: <b> {point.b} <br/>",
                    ifelse(count,
                           paste0(format[1], "{point.",
                                  ifelse(percentage, "percent}", "c}"),
                                  format[2], "<br/>"),
                           "</b>"))
  }
  if (is.null(popup)) {
    popup <- paste0("Lng: <b> {point.a} </b><br/> Lat: <b> {point.b} <br/>",
                    ifelse(count,
                           paste0(format[1], "{point.",
                                  ifelse(percentage, "percent}", "c}"),
                                  format[2], "<br/>"),
                           "</b>"))
  }
  d$label <- labelPopup(d, label, marks, nDigits, labelWrap)
  d$popup <- labelPopup(d, popup, marks, nDigits, labelWrap)

  lf <- leaflet(d)
  if (!is.null(tiles)) {
    lf <- lf %>%
      addProviderTiles(tiles)
  }
  lf <- lf %>%
    addCircleMarkers(lng = ~as.numeric(a),
                     lat = ~as.numeric(b),
                     color = ~ifelse(border$color == "color", color, border$color),
                     fillColor = ~color,
                     fillOpacity = fill$opacity,
                     opacity = border$opacity,
                     radius = ~scales::rescale(sqrt(c), to = c(size[1], size[2])),
                     label = ~map(label, ~shiny::HTML(.x)),
                     popup = ~popup,
                     stroke = border$stroke,
                     weight = border$weight)

  lf
}


#' Leaflet bubbles by latitud and longitud and category
#'
#' Leaflet bubbles by latitud and longitud and category
#'
#' @name lflt_bubbles_GlnGltCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt-Cat
#' @export
#' @examples
#' lflt_bubbles_GlnGltCat(sampleData("Gln-Glt-Cat", nrow = 10))
lflt_bubbles_GlnGltCat <- function(data,
                                   caption = NULL,
                                   count = TRUE,
                                   border = list(),
                                   fill = list(),
                                   format = c("", ""),
                                   labelWrap = 12,
                                   marks = c(",", "."),
                                   nDigits = 2,
                                   label = NULL,
                                   percentage = FALSE,
                                   popup = NULL,
                                   size = c(3, 20),
                                   tiles = NULL) {
  fillDefault <- list(color = NULL,
                      opacity = 0.5,
                      scale = "discrete",
                      nullColor = "#dddddd")
  fill <- modifyList(fillDefault, fill)

  borderDefault <- list(weight = 1.3,
                        color = "black",
                        opacity = 1,
                        stroke = TRUE)
  border <- modifyList(borderDefault, border)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  if (count) {
    d <- d  %>%
      tidyr::replace_na(list(a = NA,
                             b = NA,
                             c = ifelse(is.character(d$c), "NA", NA))) %>%
      # tidyr::drop_na() %>%
      dplyr::group_by(a, b, c) %>%
      dplyr::summarise(d = n()) %>%
      dplyr::arrange(desc(d)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(e = seq_along(d),
                    percent = d * 100 / sum(d, na.rm = TRUE))

    d <- fillColors(d, "e", fill$color, fill$scale, NULL, NULL,
                    labelWrap = labelWrap, numeric = TRUE)
  } else {
    d <- d %>%
      tidyr::replace_na(list(a = NA,
                             b = NA,
                             c = ifelse(is.character(d$c), "NA", NA))) %>%
      dplyr::group_by(a, b, c) %>%
      dplyr::summarise(d = n()) %>%
      dplyr::arrange(desc(d)) %>%
      dplyr::slice(1) %>%
      dplyr::select(a, b, c) %>%
      dplyr::mutate(d = 1)

    d <- fillColors(d, "c", fill$color, fill$scale, NULL, NULL,
                    labelWrap = labelWrap, numeric = FALSE)
    assign("d", d, envir = globalenv())
  }

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }
  # los labels y popups
  if (is.null(label)) {
    label <- paste0("Lng: <b> {point.a} </b><br/> Lat: <b> {point.b} <br/> {point.c}",
                    ifelse(count,
                           paste0(": ", format[1], "{point.",
                                  ifelse(percentage, "percent}", "d}"),
                                  format[2], "<br/>"),
                           "</b>"))
  }
  if (is.null(popup)) {
    popup <- paste0("Lng: <b> {point.a} </b><br/> Lat: <b> {point.b} <br/> {point.c}",
                    ifelse(count,
                           paste0(": ", format[1], "{point.",
                                  ifelse(percentage, "percent}", "d}"),
                                  format[2], "<br/>"),
                           "</b>"))
  }
  d$label <- labelPopup(d, label, marks, nDigits, labelWrap)
  d$popup <- labelPopup(d, popup, marks, nDigits, labelWrap)

  lf <- leaflet(d)
  if (!is.null(tiles)) {
    lf <- lf %>%
      addProviderTiles(tiles)
  }
  lf <- lf %>%
    addCircleMarkers(lng = ~as.numeric(a),
                     lat = ~as.numeric(b),
                     color = ~ifelse(border$color == "color", color, border$color),
                     fillColor = ~color,
                     fillOpacity = fill$opacity,
                     opacity = border$opacity,
                     radius = ~scales::rescale(sqrt(d), to = c(size[1], size[2])),
                     label = ~map(label, ~shiny::HTML(.x)),
                     popup = ~popup,
                     stroke = border$stroke,
                     weight = border$weight)
  lf
}



#' Leaflet bubbles by latitud and longitud and numeric variable
#'
#' Leaflet bubbles by latitud and longitud and numeric variable
#'
#' @name lflt_bubbles_GlnGltNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt-Num
#' @export
#' @examples
#' lflt_bubbles_GlnGltNum(sampleData("Gln-Glt-Num", nrow = 10))
lflt_bubbles_GlnGltNum <- function(data,
                                   caption = NULL,
                                   agg = "sum",
                                   border = list(),
                                   fill = list(),
                                   format = c("", ""),
                                   labelWrap = 12,
                                   marks = c(",", "."),
                                   nDigits = 2,
                                   label = NULL,
                                   percentage = FALSE,
                                   popup = NULL,
                                   size = c(3, 20),
                                   tiles = NULL) {
  fillDefault <- list(color = NULL,
                      opacity = 0.5,
                      scale = "discrete",
                      nullColor = "#dddddd")
  fill <- modifyList(fillDefault, fill)

  borderDefault <- list(weight = 1.3,
                        color = "black",
                        opacity = 1,
                        stroke = TRUE)
  border <- modifyList(borderDefault, border)


  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d  %>%
    tidyr::replace_na(list(a = NA,
                           b = NA,
                           c = NA)) %>%
    # tidyr::drop_na() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c)) %>%
    dplyr::mutate(percent = c * 100 / sum(c, na.rm = TRUE))

  d <- fillColors(d, "c", fill$color, fill$scale, NULL, NULL,
                  labelWrap = labelWrap, numeric = TRUE)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }
  # los labels y popups
  if (is.null(label)) {
    label <- paste0("Lng: <b> {point.a} </b><br/> Lat: <b> {point.b} <br/>",
                    format[1], "{point.",
                    ifelse(percentage, "percent}", "c}"),
                    format[2], "<br/>",
                    "</b>")
  }
  if (is.null(popup)) {
    popup <- paste0("Lng: <b> {point.a} </b><br/> Lat: <b> {point.b} <br/>",
                    format[1], "{point.",
                    ifelse(percentage, "percent}", "c}"),
                    format[2], "<br/>",
                    "</b>")
  }
  d$label <- labelPopup(d, label, marks, nDigits, labelWrap)
  d$popup <- labelPopup(d, popup, marks, nDigits, labelWrap)

  lf <- leaflet(d)
  if (!is.null(tiles)) {
    lf <- lf %>%
      addProviderTiles(tiles)
  }
  lf <- lf %>%
    addCircleMarkers(lng = ~as.numeric(a),
                     lat = ~as.numeric(b),
                     color = ~ifelse(border$color == "color", color, border$color),
                     fillColor = ~color,
                     fillOpacity = fill$opacity,
                     opacity = border$opacity,
                     radius = ~scales::rescale(sqrt(c), to = c(size[1], size[2])),
                     label = ~map(label, ~shiny::HTML(.x)),
                     popup = ~popup,
                     stroke = border$stroke,
                     weight = border$weight)

  lf
}


#' Leaflet bubbles grouped by categorical variable size by numerical
#'
#' Leaflet bubbles grouped by categorical variable size by numerical
#'
#' @name lflt_bubbles_GlnGltCatNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt-Cat-Num
#' @export
#' @examples
#' lflt_bubbles_GlnGltCatNum(sampleData("Gln-Glt-Cat-Num", nrow = 10))
lflt_bubbles_GlnGltCatNum <- function(data,
                                      caption = NULL,
                                      agg = "sum",
                                      border = list(),
                                      fill = list(),
                                      format = c("", ""),
                                      labelWrap = 12,
                                      marks = c(",", "."),
                                      nDigits = 2,
                                      label = NULL,
                                      percentage = FALSE,
                                      popup = NULL,
                                      size = c(3, 20),
                                      tiles = NULL) {
  fillDefault <- list(color = NULL,
                      opacity = 0.5,
                      scale = "discrete",
                      nullColor = "#dddddd")
  fill <- modifyList(fillDefault, fill)

  borderDefault <- list(weight = 1.3,
                        color = "black",
                        opacity = 1,
                        stroke = TRUE)
  border <- modifyList(borderDefault, border)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d  %>%
    tidyr::replace_na(list(a = NA,
                           b = NA,
                           c = ifelse(is.character(d$c), "NA", NA),
                           d = NA)) %>%
    # tidyr::drop_na() %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(d = agg(agg, d)) %>%
    dplyr::mutate(percent = d * 100 / sum(d, na.rm = TRUE))

  d <- fillColors(d, "c", fill$color, fill$scale, NULL, NULL,
                  labelWrap = labelWrap, numeric = FALSE)

    if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }
  # los labels y popups
  if (is.null(label)) {
    label <- paste0("Lng: <b> {point.a} </b><br/> Lat: <b> {point.b} <br/> {point.c}",
                    format[1], "{point.",
                    ifelse(percentage, "percent}", "d}"),
                    format[2], "<br/>",
                    "</b>")
  }
  if (is.null(popup)) {
    popup <- paste0("Lng: <b> {point.a} </b><br/> Lat: <b> {point.b} <br/> {point.c}",
                    format[1], "{point.",
                    ifelse(percentage, "percent}", "d}"),
                    format[2], "<br/>",
                    "</b>")
  }
  d$label <- labelPopup(d, label, marks, nDigits, labelWrap)
  d$popup <- labelPopup(d, popup, marks, nDigits, labelWrap)

  lf <- leaflet(d)
  if (!is.null(tiles)) {
    lf <- lf %>%
      addProviderTiles(tiles)
  }
  lf <- lf %>%
    addCircleMarkers(lng = ~as.numeric(a),
                     lat = ~as.numeric(b),
                     color = ~ifelse(border$color == "color", color, border$color),
                     fillColor = ~color,
                     fillOpacity = fill$opacity,
                     opacity = border$opacity,
                     radius = ~scales::rescale(sqrt(d), to = c(size[1], size[2])),
                     label = ~map(label, ~shiny::HTML(.x)),
                     popup = ~popup,
                     stroke = border$stroke,
                     weight = border$weight)

  lf
}
