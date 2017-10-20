lftLabels <- function()

#' Ordered horizontal bar
#'
#' Ordered horizontal bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat
#' @examples
#' hgch_bar_hor_top_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_bar_hor_top_Cat




#' Leaflet bubbles size by geocode
#'
#' Leaflet bubbles size by geocode
#'
#' @name lflt_bubbles_size_Gcd
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd
#' @export
#' @examples
#' lflt_bubbles_Gcd(sampleData("Gcd", nrow = 10))
lflt_bubbles_Gcd <- function(data,
                             color = "navy",
                             #infoVar = NULL,
                             label = NULL,
                             popup = "",
                             minSize = 3,
                             maxSize = 20,
                             tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")


  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", nms, ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }

  l <- leaflet(dd) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(minSize, maxSize)),
                     #popup = ~info,
                     label = lab,
                     color = color,
                     stroke = FALSE)
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
                                        palette = c("#009EE3", "#9B71AF"),
                                        #infoVar = NULL,
                                        label = NULL,
                                        popup = "",
                                        size = 5,
                                        tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")
  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    na.omit() %>%
    dplyr::group_by(b)

  col <- colorRampPalette(palette)


  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }


  l <- leaflet(dd) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = size,
                     #popup = ~info,
                     label = lab,
                     color = col(nrow(dd)),
                     stroke = FALSE)
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
                                     palette = c("#009EE3", "#9B71AF"),
                                     #infoVar = NULL,
                                     label = NULL,
                                     popup = "",
                                     minSize = 3,
                                     maxSize = 20,
                                     tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")
  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n())

  col <- colorRampPalette(palette)

  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }


  l <- leaflet(dd) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = ~scales::rescale(sqrt(c), to = c(minSize, maxSize)),
                     #popup = ~info,
                     label = lab,
                     color = col(nrow(dd)),
                     stroke = FALSE)
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
                                     label = NULL,
                                     popup = "",
                                     minSize = 3,
                                     maxSize = 20,
                                     scope = "world",
                                     agg = "sum",
                                     tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")
  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    dplyr::na.omit() %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = do.call(agg, list(b, na.rm = TRUE))) %>%
    dplyr::filter(b >= 0)


  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }

  l <- leaflet(dd) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(minSize, maxSize)),
                     #popup = ~info,
                     label = lab,
                     color = color,
                     stroke = FALSE)
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
                                   palette = c("#009EE3", "#9B71AF"),
                                   #infoVar = NULL,
                                   label = NULL,
                                   popup = "",
                                   minSize = 3,
                                   maxSize = 20,
                                   agg = "sum",
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")
  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = do.call(agg, list(c, na.rm = TRUE))) %>%
    dplyr::filter(c >= 0)

  col <- colorRampPalette(palette)


  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }


  l <- leaflet(dd) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = ~scales::rescale(sqrt(c), to = c(minSize, maxSize)),
                     #popup = ~info,
                     label = lab,
                     color = col(nrow(dd)),
                     stroke = FALSE)
}


#' Leaflet bubbles size by geoname
#'
#' Leaflet bubbles size by geoname
#'
#' @name lflt_bubbles_size_Gnm
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' lflt_bubbles_Gnm(sampleData("Gnm", nrow = 10))
lflt_bubbles_Gnm <- function(data,
                             color = "navy",
                             #infoVar = NULL,
                             label = NULL,
                             popup = "",
                             minSize = 3,
                             maxSize = 20,
                             tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")


  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", nms, ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }

  l <- leaflet(dd) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(minSize, maxSize)),
                     #popup = ~info,
                     label = lab,
                     color = color,
                     stroke = FALSE)
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
                                        palette = c("#009EE3", "#9B71AF"),
                                        #infoVar = NULL,
                                        label = NULL,
                                        popup = "",
                                        size = 5,
                                        tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")
  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    na.omit() %>%
    dplyr::group_by(b)

  col <- colorRampPalette(palette)


  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }


  l <- leaflet(dd) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = size,
                     #popup = ~info,
                     label = lab,
                     color = col(nrow(dd)),
                     stroke = FALSE)
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
                                     palette = c("#009EE3", "#9B71AF"),
                                     #infoVar = NULL,
                                     label = NULL,
                                     popup = "",
                                     minSize = 3,
                                     maxSize = 20,
                                     tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")
  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n())

  col <- colorRampPalette(palette)

  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }


  l <- leaflet(dd) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(minSize, maxSize)),
                     #popup = ~info,
                     label = lab,
                     color = col(nrow(dd)),
                     stroke = FALSE)
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
                                     #infoVar = NULL,
                                     label = NULL,
                                     popup = "",
                                     minSize = 3,
                                     maxSize = 20,
                                     scope = "world",
                                     agg = "sum",
                                     tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")
  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    dplyr::na.omit() %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = do.call(agg, list(b, na.rm = TRUE))) %>%
    dplyr::filter(b >= 0)


  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }

  l <- leaflet(dd) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(minSize, maxSize)),
                     #popup = ~info,
                     label = lab,
                     color = color,
                     stroke = FALSE)
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
                                   palette = c("#009EE3", "#9B71AF"),
                                   #infoVar = NULL,
                                   label = NULL,
                                   popup = "",
                                   minSize = 3,
                                   maxSize = 20,
                                   agg = "sum",
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")
  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = do.call(agg, list(c, na.rm = TRUE))) %>%
    dplyr::filter(c >= 0)

  col <- colorRampPalette(palette)


  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }


  l <- leaflet(dd) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = ~scales::rescale(sqrt(c), to = c(minSize, maxSize)),
                     #popup = ~info,
                     label = lab,
                     color = col(nrow(dd)),
                     stroke = FALSE)
}



############


#' Leaflet bubbles size by latitud and longitud
#'
#' Leaflet bubbles size by latitud and longitud
#'
#' @name lflt_bubbles_size_GlnGlt
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: GlnGlt
#' @export
#' @examples
#' lflt_bubbles_GlnGlt(sampleData("GlnGlt", nrow = 10))
lflt_bubbles_GlnGlt <- function(data,
                                color = "navy",
                                #infoVar = NULL,
                                label = NULL,
                                popup = "",
                                size = 5,
                                tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  dgeo <- f$d %>%
    na.omit()

  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", nms, ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~b, lng = ~a, weight = 3,
                     radius = size,
                     #popup = ~info,
                     label = lab,
                     color = color,
                     stroke = FALSE)
}


#' Leaflet bubbles grouped by categorical variable
#'
#' Leaflet bubbles grouped by categorical variable
#'
#' @name lflt_bubbles_grouped_GlnGltCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt-Cat
#' @export
#' @examples
#' lflt_bubbles_grouped_GlnGltCat(sampleData("Gln-Glt-Cat", nrow = 10))
lflt_bubbles_grouped_GlnGltCat <- function(data,
                                           palette = c("#009EE3", "#9B71AF"),
                                           #infoVar = NULL,
                                           label = NULL,
                                           popup = "",
                                           size = 5,
                                           tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  dgeo <- f$d %>%
    na.omit()

  col <- colorRampPalette(palette)

  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }
  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = size,
                     #popup = ~info,
                     label = lab,
                     color = col(nrow(dd)),
                     stroke = FALSE)
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
                                     palette = c("#009EE3", "#9B71AF"),
                                     #infoVar = NULL,
                                     label = NULL,
                                     popup = "",
                                     minSize = 3,
                                     maxSize = 20,
                                     tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")
  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n())

  col <- colorRampPalette(palette)

  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }


  l <- leaflet(dd) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = ~scales::rescale(sqrt(c), to = c(minSize, maxSize)),
                     #popup = ~info,
                     label = lab,
                     color = col(nrow(dd)),
                     stroke = FALSE)
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
                                     label = NULL,
                                     popup = "",
                                     minSize = 3,
                                     maxSize = 20,
                                     scope = "world",
                                     agg = "sum",
                                     tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")
  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    dplyr::na.omit() %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = do.call(agg, list(b, na.rm = TRUE))) %>%
    dplyr::filter(b >= 0)


  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }

  l <- leaflet(dd) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(minSize, maxSize)),
                     #popup = ~info,
                     label = lab,
                     color = color,
                     stroke = FALSE)
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
                                   palette = c("#009EE3", "#9B71AF"),
                                   #infoVar = NULL,
                                   label = NULL,
                                   popup = "",
                                   minSize = 3,
                                   maxSize = 20,
                                   agg = "sum",
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)

  # primero que se carguen la tabla de sinónimos y de las equivalencias oficiales
  gsin <- geodata::geodataCsv("")
  gofi <- geodata::geodataCsv("TODO")
  #geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>%
    na.omit() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = do.call(agg, list(c, na.rm = TRUE))) %>%
    dplyr::filter(c >= 0)

  col <- colorRampPalette(palette)


  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)
  # los labels y popups
  if (is.null(label)) {
    lab <- map(as.list(1:nrow(dgeo)), function(r) {
      shiny::HTML(paste0("<b>", names(dgeo), ": </b>", dgeo[r, ], "<br/>", collapse = ""))
    })
  } else {
    lab <- label
  }


  l <- leaflet(dd) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                     radius = ~scales::rescale(sqrt(c), to = c(minSize, maxSize)),
                     #popup = ~info,
                     label = lab,
                     color = col(nrow(dd)),
                     stroke = FALSE)
}



##############

#' lflt_bubbles_size_GltLn
#' Multilines
#' @name lflt_bubbles_size_GltLn
#' @param x A data.frame
#' @export
#' @return leaflet viz
#' @section ftypes: Ye-Nu*
#' @examples
#' lflt_bubbles_size_GltLn(sampleData("Gcd-Num",nrow = 10))
lflt_bubbles_size_GltLn <- function(data,
                                    #infoVar = NULL,
                                    radius = NULL,
                                    bounds =  NULL){

  radius <- radius %||% 5
  f <- fringe(data)
  nms <- getClabels(f)
  dd <- f$d %>% na.omit()
  tpl <- str_tpl_format("<strong>{GltName}: {a}</strong><br>{GlnName}: {b}",
                        list(GltName = nms[1], GlnName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)

  l <- leaflet(dd) %>%
    addProviderTiles("CartoDB.Positron")
  if(!is.null(bounds))
    l <- l %>%  fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  l  %>%  addCircleMarkers(lat = ~a, lng = ~b, weight = 3,
                           #radius = ~scales::rescale(sqrt(b), to = c(3,20)),
                           radius = radius,
                           popup = ~info,
                           color = "navy",
                           stroke = FALSE)
}



