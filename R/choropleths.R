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
                                # dropNa = FALSE, ¿NO HACE FALTA? ¿SIEMPRE SE QUITAN??
                                fillOpacity = 0.5,
                                format = c("", ""),
                                legendColor = "discrete",
                                legendPosition = "bottomleft",
                                legendTitle = NULL,
                                label = NULL,
                                marks = c(",", "."),
                                nBins = 4,
                                nDigits = 2,
                                palette = c("#009EE3", "#E5007D", "#95C11E"),
                                percentage = FALSE,
                                # popup = NULL,
                                #infoVar = NULL,
                                scope = "world_countries",
                                tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  legendTitle <- legendTitle %||% ""

  d <- d  %>%
    tidyr::drop_na() %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  d <- percentColumn(d, "b", percentage, nDigits)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  if (scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    dgeo@data <- dgeo@data %>%
      dplyr::left_join(d, by = c(id = "a"))
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  # los labels y popups
  if (is.null(label)) {
    #lab <- paste0(dgeo@data$id, ": ", dgeo@data$b)
    lab <- map(1:nrow(dgeo@data), function(r) {
      htmltools::HTML(paste0(dgeo@data$id[r],
                             ": <b>",
                             format[1],
                             format(dgeo@data$b[r], big.mark = marks[1], decimal.mark = marks[2]),
                             ifelse(is.na(dgeo@data$b[r]), "", format[2]),
                             "<br/>"))
    })
  }

  # colorBin en vez de colorNumeric

  # col <- colorBin(palette = palette,
  #                 domain = c(unique(dgeo@data$b), unique(dgeo@data$b) + 1),
  #                 # bins = nBins)
  #                 bins = as.vector(quantile(unique(dgeo@data$b),
  #                                           probs = seq(0, 1, 1/nBins),
  #                                           na.rm = TRUE)))
  # reverse = TRUE)

  col <- colorNumeric(palette = palette,
                      domain = dgeo@data$b)
  if (legendColor == "discrete") {
  col <- colorBin(palette = palette,
                  domain = dgeo@data$b,
                  bins = nBins,
                  pretty = FALSE)
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addPolygons(fillColor = col(dgeo@data$b),
    # addPolygons(fillColor = col(unique(dgeo@data$b)),
                fillOpacity = fillOpacity,
                color = "black",
                label = lab,
                weight = 0.5)
  if (legendColor %in% c("discrete", "continuous")) {
    l <- l %>%
      addLegend(pal = col,
                title = legendTitle,
                values = dgeo@data$b,
                labFormat = labFor(prefix = format[1],
                                   suffix = format[2],
                                   big.mark = marks[1],
                                   decimal.mark = marks[2],
                                   digits = nDigits),
                opacity = 2,
                position = legendPosition)
  }
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
                                   dropNa = FALSE,
                                   fillOpacity = 0.5,
                                   #format = c("", ""),
                                   legendColor = "discrete",
                                   legendPosition = "bottomleft",
                                   legendTitle = NULL,
                                   label = NULL,
                                   #marks = c(",", "."),
                                   #nDigits = 2,
                                   palette = c("#009EE3", "#9B71AF", "#FFFF99"),
                                   #percentage = FALSE,
                                   #infoVar = NULL,
                                   # popup = NULL,
                                   scope = "world_countries",
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  legendTitle <- legendTitle %||% ""


  if (dropNa)
    d <- d %>%
    tidyr::drop_na(2)

  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::arrange(desc(c)) %>%
    dplyr::slice(1)

  # d <- percentColumn(d, "b", percentage, nDigits)
  #
  # if (percentage & nchar(format[2]) == 0) {
  #   format[2] <- "%"
  # }

  if (scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    dgeo@data <- dgeo@data %>%
      dplyr::left_join(d, by = c(id = "a"))
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  # los labels y popups
  if (is.null(label)) {
    #lab <- paste0(dgeo@data$id, ": ", dgeo@data$b)
    lab <- map(1:nrow(dgeo@data), function(r) {
      htmltools::HTML(paste0("<b>",
                             dgeo@data$id[r],
                             "</b><br/>",
                             nms[2],
                             ": <b>",
                             dgeo@data$b[r],
                             "</b>"))
    })
  }

  col <- colorFactor(palette = palette, domain = unique(dgeo@data$b))
  # col <- colorNumeric(palette = palette, domain = NULL)

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addPolygons(fillColor = col(dgeo@data$b),
                fillOpacity = fillOpacity,
                color = "black",
                label = lab,
                weight = 0.5)

  if (legendColor == "discrete") {
    l <- l %>%
      addLegend(pal = col,
                title = legendTitle,
                values = dgeo@data$b,
                # labFormat = labFor(prefix = format[1],
                #                    suffix = format[2],
                #                    big.mark = marks[1],
                #                    decimal.mark = marks[2],
                #                    digits = nDigits),
                opacity = 2,
                position = legendPosition)

  }
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
                                   agg = "sum",
                                   dropNa = FALSE,
                                   fillOpacity = 0.5,
                                   format = c("", ""),
                                   legendColor = "discrete",
                                   legendPosition = "bottomleft",
                                   legendTitle = NULL,
                                   label = NULL,
                                   marks = c(",", "."),
                                   nBins = 4,
                                   nDigits = 2,
                                   palette = c("#009EE3", "#E5007D", "#95C11E"),
                                   percentage = FALSE,
                                   # popup = NULL,
                                   #infoVar = NULL,,
                                   scope = "world_countries",
                                   tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  legendTitle <- legendTitle %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na(2)

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = do.call(agg, list(b, na.rm = TRUE)))

  d <- percentColumn(d, "b", percentage, nDigits)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  if (scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    dgeo@data <- dgeo@data %>%
      dplyr::left_join(d, by = c(id = "a"))
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata())")
  }

  # los labels y popups
  if (is.null(label)) {
    #lab <- paste0(dgeo@data$id, ": ", dgeo@data$b)
    lab <- map(1:nrow(dgeo@data), function(r) {
      htmltools::HTML(paste0(dgeo@data$id[r],
                             ": <b>",
                             format[1],
                             format(dgeo@data$b[r], big.mark = marks[1], decimal.mark = marks[2]),
                             ifelse(is.na(dgeo@data$b[r]), "", format[2]),
                             "<br/>"))
    })
  }

  # colorBin en vez de colorNumeric
  # col <- colorQuantile(palette = palette,
  #                      domain = dgeo@data$b,
  #                      probs = seq(0, 1, 1/nBins))

  col <- colorNumeric(palette = palette,
                      domain = dgeo@data$b)
  if (legendColor == "discrete") {
    col <- colorBin(palette = palette,
                    domain = dgeo@data$b,
                    bins = nBins,
                    pretty = FALSE)
  }

  # col <- colorNumeric(palette = palette, domain = NULL)

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addPolygons(fillColor = col(dgeo@data$b),
                # addPolygons(fillColor = col(unique(dgeo@data$b)),
                fillOpacity = fillOpacity,
                color = "black",
                label = lab,
                weight = 0.5)
  # stroke = FALSE)

  if (legendColor %in% c("discrete", "continuous")) {
    l <- l %>%
      addLegend(pal = col,
                title = legendTitle,
                values = dgeo@data$b,
                labFormat = labFor(prefix = format[1],
                                   suffix = format[2],
                                   big.mark = marks[1],
                                   decimal.mark = marks[2],
                                   digits = nDigits),
                opacity = 2,
                position = legendPosition)
  }
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
                                # dropNa = FALSE, AQUI NO HACE FALTA.. SIEMPE SE QUITAN...
                                fillOpacity = 0.5,
                                format = c("", ""),
                                legendColor = "discrete",
                                legendPosition = "bottomleft",
                                legendTitle = NULL,
                                label = NULL,
                                marks = c(",", "."),
                                matchMethod = "lv",
                                maxDist = 2,
                                nBins = 8,
                                nDigits = 2,
                                palette = c("#009EE3", "#E5007D", "#95C11E"),
                                percentage = FALSE,
                                # popup = NULL,
                                #infoVar = NULL,
                                scope = "world_countries",
                                tiles = "CartoDB.Positron") {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  legendTitle <- legendTitle %||% ""

  d <- d  %>%
    tidyr::drop_na() %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  d <- percentColumn(d, "b", percentage, nDigits)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  if (scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    if ("altnames" %in% names(geodata::geodataMeta(scope))) {
      alt <- geodata::geodataMeta(scope)$altnames %>%
        na.omit()
      a0 <- adist(d$a, alt$altname)
      map(1:nrow(a0), function(u) {
        s0 <- which(a0[u, ] == min(a0[u, ], na.rm = TRUE))

      })



      # i0 <- map2_df(d$a, 1:length(d$a), function(u, e) {
      #   j0 <- agrep(u, alt$altname, max.distance = maxDist, ignore.case = TRUE)[1]
      #   j1 <- grep(alt$id[j0], dgeo@data$id)
      #   if (!is.na(j0) > 0 & sum(j1) > 0) {
      #     cbind(dgeo@data[j1, ], alt[j0, "altname"], d[e, ])
      #   }
      # })
      # dgeo@data <- left_join(dgeo@data, i0, by = "id")
      # dgeo@data <- d %>%
      #   fuzzyjoin::stringdist_left_join(alt,
      #                                   by = c(a = "altname"),
      #                                   ignore_case = TRUE,
      #                                   method = matchMethod,
      #                                   max_dist = maxDist,
      #                                   distance_col = "dist") %>%
      #   dplyr::group_by(a) %>%
      #   dplyr::filter(dist == min(dist)) %>%
      #   dplyr::right_join(dgeo@data, by = "id")
      # View(dgeo@data)
    } else {
      dgeo@data <- d %>%
        fuzzyjoin::stringdist_right_join(dgeo@data,
                                         by = c(a = "name"),
                                         ignore_case = TRUE,
                                         method = matchMethod,
                                         max_dist = maxdist,
                                         distance_col = "dist") %>%
        dplyr::group_by(a) %>%
        dplyr::filter(dist == min(dist)) %>%
        dplyr::right_join(dgeo@data, by = c("name", "id"))
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
  }

  # los labels y popups
  #lab <- paste0(dgeo@data$id, ": ", dgeo@data$b)
  if (is.null(label)) {
    lab <- map(1:nrow(dgeo@data), function(r) {
      htmltools::HTML(paste0(dgeo@data$name[r],
                             ": <b>",
                             format[1],
                             format(dgeo@data$b[r], big.mark = marks[1], decimal.mark = marks[2]),
                             ifelse(is.na(dgeo@data$b[r]), "", format[2]),
                             "<br/>"))
    })
  }

  col <- colorNumeric(palette = palette,
                      domain = dgeo@data$b)
  if (legendColor == "discrete") {
    col <- colorBin(palette = palette,
                    domain = dgeo@data$b,
                    bins = nBins,
                    pretty = FALSE)
  }

  l <- leaflet(dgeo) %>%
    addProviderTiles(tiles) %>%
    addPolygons(fillColor = col(dgeo@data$b),
                # addPolygons(fillColor = col(unique(dgeo@data$b)),
                fillOpacity = fillOpacity,
                color = "black",
                label = lab,
                weight = 0.5)
  if (legendColor %in% c("discrete", "continuous")) {
    l <- l %>%
      addLegend(pal = col,
                title = legendTitle,
                values = dgeo@data$b,
                labFormat = labFor(prefix = format[1],
                                   suffix = format[2],
                                   big.mark = marks[1],
                                   decimal.mark = marks[2],
                                   digits = nDigits),
                opacity = 2,
                position = legendPosition)
  }
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
