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
lflt_choropleth_Gcd <- function(data = NULL,
                                caption = NULL,
                                count = TRUE,
                                border = list(weight = 1,
                                              color = "black",
                                              opacity = 1),
                                fill = list(color = NULL,
                                            opacity = 0.5,
                                            scale = "no",
                                            nullColor = "#cccccc"),
                                # dropNa = FALSE, ¿NO HACE FALTA? ¿SIEMPRE SE QUITAN??
                                format = c("", ""),
                                highlightValue = NULL,
                                highlightValueColor = NULL,
                                labelWrap = 12,
                                legend = list(bins = 6,
                                              position = "bottomleft",
                                              title = NULL),
                                mapName = "world_countries",
                                marks = c(",", "."),
                                nDigits = 2,
                                label = NULL,
                                percentage = FALSE,
                                popup = NULL,
                                tiles = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }
  dt <- geojsonio::topojson_read(geodataTopojsonPath(mapName))
  dt@data$color <- fill$nullColor
  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    if (count) {
      d <- d  %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
        # tidyr::drop_na() %>%
        dplyr::group_by(a) %>%
        dplyr::summarise(b = n()) %>%
        dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))

      d <- fillColors(d, "b", fill$color, fill$scale, highlightValue, highlightValueColor,
                      labelWrap = labelWrap, bins = legend$bins, numeric = TRUE)
    } else {
      d <- d %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
        dplyr::group_by(a) %>%
        dplyr::slice(1)

      d <- fillColors(d, "a", fill$color, fill$scale, highlightValue, highlightValueColor,
                      labelWrap = labelWrap, numeric = FALSE)
    }
    dt@data <- dt@data %>%
      dplyr::select(-color) %>%
      dplyr::left_join(d, by = c(id = "a"))

    dt@data$color <- as.character(dt@data$color)
    dt@data$color[is.na(dt@data$color)] <- fill$nullColor
  }

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }
  # los labels y popups
  if (is.null(label)) {
    label <- "{point.id}: <b> {point.b} </b>"
  }
  if (is.null(popup)) {
    popup <- "{point.id}: <b> {point.b} </b>"
  }
  dt@data$label <- labelPopup(dt@data, label, marks, nDigits, labelWrap)
  dt@data$popup <- labelPopup(dt@data, popup, marks, nDigits, labelWrap)

  lf <- leaflet(dt)
  if (!is.null(tiles)) {
    lf <- lf %>%
      addProviderTiles(tiles)
  }
  lf <- lf %>%
    addPolygons(fillColor = ~color,
                label = ~map(label, ~shiny::HTML(.x)),
                popup = ~map(popup, ~shiny::HTML(.x)),
                color = border$color,
                fillOpacity = fill$opacity,
                opacity = border$opacity,
                weight = border$weight)
  if (!legend$position %in% "no") {
    lf <- lf %>%
      addLegend(bins = legend$bins,
                colors = ~unique(color),
                labels = ~unique(ifelse(count, b, id)),
                # labFormat = labFor(prefix = format[1],
                #                    suffix = format[2],
                #                    big.mark = marks[1],
                #                    decimal.mark = marks[2],
                #                    digits = nDigits),
                opacity = fill$opacity,
                position = legend$position,
                title = legend$title)
  }
  lf
}



#' Leaflet choropleths by geographical variable
#'
#' Leaflet choropleths by geographical variable
#'
#' @name lflt_choropleth_GcdCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Cat
#' @export
#' @examples
#' lflt_choropleth_GcdCat(sampleData("Gcd-Cat", nrow = 10))
lflt_choropleth_GcdCat <- function(data = NULL,
                                   caption = NULL,
                                   count = TRUE,
                                   border = list(weight = 1,
                                                 color = "black",
                                                 opacity = 1),
                                   fill = list(color = NULL,
                                               opacity = 0.5,
                                               scale = "no",
                                               nullColor = "#cccccc"),
                                   # dropNa = FALSE, ¿NO HACE FALTA? ¿SIEMPRE SE QUITAN??
                                   format = c("", ""),
                                   highlightValue = NULL,
                                   highlightValueColor = NULL,
                                   labelWrap = 12,
                                   legend = list(bins = 6,
                                                 position = "bottomleft",
                                                 title = NULL),
                                   mapName = "world_countries",
                                   marks = c(",", "."),
                                   nDigits = 2,
                                   label = NULL,
                                   percentage = FALSE,
                                   popup = NULL,
                                   tiles = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }
  dt <- geojsonio::topojson_read(geodataTopojsonPath(mapName))
  dt@data$color <- fill$nullColor
  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    if (count) {
      d <- d  %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                               b = ifelse(is.character(d$b), "NA", NA))) %>%
        # tidyr::drop_na() %>%
        dplyr::group_by(a, b) %>%
        dplyr::summarise(c = n()) %>%
        dplyr::arrange(desc(c)) %>%
        dplyr::slice(1) %>%
        dplyr::mutate(percent = c * 100 / sum(c, na.rm = TRUE))

      d <- fillColors(d, "c", fill$color, fill$scale, highlightValue, highlightValueColor,
                      labelWrap = labelWrap, bins = legend$bins, numeric = TRUE)
    } else {
      d <- d %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                               b = ifelse(is.character(d$b), "NA", NA))) %>%
        dplyr::group_by(a, b) %>%
        dplyr::summarise(c = n()) %>%
        dplyr::arrange(desc(c)) %>%
        dplyr::slice(1) %>%
        dplyr::selec(a, b)

      d <- fillColors(d, "b", fill$color, fill$scale, highlightValue, highlightValueColor,
                      labelWrap = labelWrap, numeric = FALSE)
    }
    dt@data <- dt@data %>%
      dplyr::select(-color) %>%
      dplyr::left_join(d, by = c(id = "a"))

    dt@data$color <- as.character(dt@data$color)
    dt@data$color[is.na(dt@data$color)] <- fill$nullColor
  }

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }
  # los labels y popups
  if (is.null(label)) {
    label <- "{point.id} <br/><b> {point.b}: </b> {point.c}"
  }
  if (is.null(popup)) {
    popup <- "{point.id} <br/><b> {point.b}: </b> {point.c}"
  }
  dt@data$label <- labelPopup(dt@data, label, marks, nDigits, labelWrap)
  dt@data$popup <- labelPopup(dt@data, popup, marks, nDigits, labelWrap)

  lf <- leaflet(dt)
  if (!is.null(tiles)) {
    lf <- lf %>%
      addProviderTiles(tiles)
  }
  lf <- lf %>%
    addPolygons(fillColor = ~color,
                label = ~map(label, ~shiny::HTML(.x)),
                popup = ~map(popup, ~shiny::HTML(.x)),
                color = border$color,
                fillOpacity = fill$opacity,
                opacity = border$opacity,
                weight = border$weight)
  if (!legend$position %in% "no") {
    lf <- lf %>%
      addLegend(bins = legend$bins,
                colors = ~unique(color),
                labels = ~unique(ifelse(count, b, id)),
                # labFormat = labFor(prefix = format[1],
                #                    suffix = format[2],
                #                    big.mark = marks[1],
                #                    decimal.mark = marks[2],
                #                    digits = nDigits),
                opacity = fill$opacity,
                position = legend$position,
                title = legend$title)
  }
  lf
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
lflt_choropleth_GcdNum <- function(data = NULL,
                                   caption = NULL,
                                   agg = "sum",
                                   border = list(weight = 1,
                                                 color = "black",
                                                 opacity = 1),
                                   fill = list(color = NULL,
                                               opacity = 0.5,
                                               scale = "no",
                                               nullColor = "#cccccc"),
                                   # dropNa = FALSE, ¿NO HACE FALTA? ¿SIEMPRE SE QUITAN??
                                   format = c("", ""),
                                   highlightValue = NULL,
                                   highlightValueColor = NULL,
                                   labelWrap = 12,
                                   legend = list(bins = 6,
                                                 position = "bottomleft",
                                                 title = NULL),
                                   mapName = "world_countries",
                                   marks = c(",", "."),
                                   nDigits = 2,
                                   label = NULL,
                                   percentage = FALSE,
                                   popup = NULL,
                                   tiles = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }
  dt <- geojsonio::topojson_read(geodataTopojsonPath(mapName))
  dt@data$color <- fill$nullColor
  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    d <- d  %>%
      tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                             b =  NA)) %>%
      # tidyr::drop_na() %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = agg(agg, b)) %>%
      dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))

    d <- fillColors(d, "b", fill$color, fill$scale, highlightValue, highlightValueColor,
                    labelWrap = labelWrap, bins = legend$bins, numeric = TRUE)

    dt@data <- dt@data %>%
      dplyr::select(-color) %>%
      dplyr::left_join(d, by = c(id = "a"))

    dt@data$color <- as.character(dt@data$color)
    dt@data$color[is.na(dt@data$color)] <- fill$nullColor
  }

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }
# los labels y popups
  if (is.null(label)) {
    label <- "{point.id}: <b> {point.b} </b>"
  }
  if (is.null(popup)) {
    popup <- "{point.id}: <b> {point.b} </b>"
  }
  dt@data$label <- labelPopup(dt@data, label, marks, nDigits, labelWrap)
  dt@data$popup <- labelPopup(dt@data, popup, marks, nDigits, labelWrap)

  lf <- leaflet(dt)
  if (!is.null(tiles)) {
    lf <- lf %>%
      addProviderTiles(tiles)
  }
  lf <- lf %>%
    addPolygons(fillColor = ~color,
                label = ~map(label, ~shiny::HTML(.x)),
                popup = ~map(popup, ~shiny::HTML(.x)),
                color = border$color,
                fillOpacity = fill$opacity,
                opacity = border$opacity,
                weight = border$weight)
  if (!legend$position %in% "no") {
    lf <- lf %>%
      addLegend(bins = legend$bins,
                colors = ~unique(color),
                labels = ~unique(ifelse(count, b, id)),
                # labFormat = labFor(prefix = format[1],
                #                    suffix = format[2],
                #                    big.mark = marks[1],
                #                    decimal.mark = marks[2],
                #                    digits = nDigits),
                opacity = fill$opacity,
                position = legend$position,
                title = legend$title)
  }
  lf
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
    if ("altnames" %in% names(geodata::geodataMeta(scope))) {
      alt <- geodata::geodataMeta(scope)$altnames %>%
        na.omit()
      a0 <- adist(d$a, alt$altname, ignore.case = TRUE)
      p0 <- map_df(1:nrow(a0), function(u) {
        f0 <- which(a0[u, ] == min(a0[u, ], na.rm = TRUE))[1]
        f1 <- unique(alt$id[f0])[1]
        f2 <- grep(f1, dgeo@data$id)
        if (sum(f2) > 0) {
          bind_cols(dgeo@data[f2, ],
                alt[f0, "altname"],
                d[u, ],
                data.frame("dist" = a0[u, f0]))
        }
      })
      p1 <- p0 %>%
        group_by(id) %>%
        arrange(dist) %>%
        slice(1)
      dgeo@data <- left_join(dgeo@data, p1, by = "id")
      names(dgeo@data) <- c("id", "name", "name_i", "altname", "a", "b", "dist")
    } else {
      a0 <- adist(d$a, dgeo@data$name, ignore.case = TRUE)
      p0 <- map_df(1:nrow(a0), function(u) {
        f0 <- which(a0[u, ] == min(a0[u, ], na.rm = TRUE))[1]
        bind_cols(dgeo@data[f0, ], d[u, ], data.frame("dist" = a0[u, f0]))
      })
      p1 <- p0 %>%
        group_by(id) %>%
        arrange(dist) %>%
        slice(1)
      dgeo@data <- left_join(dgeo@data, p1, by = "id")
      names(dgeo@data) <- c("id", "name", "a", "b", "dist")
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
    drop_na(1) %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::arrange(desc(c)) %>%
    dplyr::slice(1)

  if (scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    if ("altnames" %in% names(geodata::geodataMeta(scope))) {
      alt <- geodata::geodataMeta(scope)$altnames %>%
        na.omit()
      a0 <- adist(d$a, alt$altname, ignore.case = TRUE)
      p0 <- map_df(1:nrow(a0), function(u) {
        f0 <- which(a0[u, ] == min(a0[u, ], na.rm = TRUE))[1]
        f1 <- unique(alt$id[f0])[1]
        f2 <- grep(f1, dgeo@data$id)
        if (sum(f2) > 0) {
          bind_cols(dgeo@data[f2, ],
                alt[f0, "altname"],
                d[u, ],
                data.frame("dist" = min(a0[u, ], na.rm = TRUE)))
        }
      })
      p1 <- p0 %>%
        group_by(id) %>%
        arrange(dist) %>%
        slice(1)
      dgeo@data <- left_join(dgeo@data, p1, by = "id")
      names(dgeo@data) <- c("id", "name", "name_i", "altname", "a", "b", "c", "dist")
    } else {
      a0 <- adist(d$a, dgeo@data$name, ignore.case = TRUE)
      p0 <- map_df(1:nrow(a0), function(u) {
        f0 <- which(a0[u, ] == min(a0[u, ], na.rm = TRUE))[1]
        bind_cols(dgeo@data[f0, ], d[u, ], data.frame("dist" = a0[u, f0]))
      })
      p1 <- p0 %>%
        group_by(id) %>%
        arrange(dist) %>%
        slice(1)
      dgeo@data <- left_join(dgeo@data[, ], p1, by = "id")
      names(dgeo@data) <- c("id", "name", "a", "b", "c", "dist")
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
  }

  # los labels y popups
  if (is.null(label)) {
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
                opacity = 2,
                position = legendPosition)

  }
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
    drop_na(1) %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = do.call(agg, list(b, na.rm = TRUE)))

  d <- percentColumn(d, "b", percentage, nDigits)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  if (scope %in% geodata::availableGeodata()) {
    dgeo <- geojsonio::topojson_read(geodata::geodataTopojsonPath(scope))
    if ("altnames" %in% names(geodata::geodataMeta(scope))) {
      alt <- geodata::geodataMeta(scope)$altnames %>%
        na.omit()
      a0 <- adist(d$a, alt$altname, ignore.case = TRUE)
      p0 <- map_df(1:nrow(a0), function(u) {
        f0 <- which(a0[u, ] == min(a0[u, ], na.rm = TRUE))[1]
        f1 <- unique(alt$id[f0])[1]
        f2 <- grep(f1, dgeo@data$id)
        if (sum(f2) > 0) {
          bind_cols(dgeo@data[f2, ],
                    alt[f0, "altname"],
                    d[u, ],
                    data.frame("dist" = min(a0[u, ], na.rm = TRUE)))
        }
      })
      p1 <- p0 %>%
        group_by(id) %>%
        arrange(dist) %>%
        slice(1)
      dgeo@data <- left_join(dgeo@data, p1, by = "id")
      names(dgeo@data) <- c("id", "name", "name_i", "altname", "a", "b", "dist")
    } else {
      a0 <- adist(d$a, dgeo@data$name, ignore.case = TRUE)
      p0 <- map_df(1:nrow(a0), function(u) {
        f0 <- which(a0[u, ] == min(a0[u, ], na.rm = TRUE))[1]
        bind_cols(dgeo@data[f0, ], d[u, ], data.frame("dist" = a0[u, f0]))
      })
      p1 <- p0 %>%
        group_by(id) %>%
        arrange(dist) %>%
        slice(1)
      dgeo@data <- left_join(dgeo@data[, ], p1, by = "id")
      names(dgeo@data) <- c("id", "name", "a", "b", "dist")
    }
  } else {
    stop("Pick an available map for the 'scope' argument (geodata::availableGeodata)")
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
