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
                                mapName = "world_countries",
                                opts = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  opts <- getOpts(opts = opts)

  dt <- geojsonio::topojson_read(geodataTopojsonPath(mapName))


  plt0 <- opts$defaultFill

  if (is.null(opts$nDigits)) opts$nDigits <- 2

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    if (opts$count) {
      d <- d  %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
        dplyr::group_by(a) %>%
        dplyr::summarise(b = n()) %>%
        dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))

      col <- ifelse(opts$percentage, "percent", "b")
    } else {
      d <- d %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
        dplyr::group_by(a) %>%
        dplyr::slice(1) %>%
        dplyr::mutate(c = a)

      opts$legend$position <- "no"
      col <- "c"
    }
    dt@data <- dt@data %>%
      dplyr::left_join(d, by = c(id = "a"))

    plt <- fillColorsChoropleth(dt@data, col, opts$color, opts$scale, opts$nLevels, opts$legend$choropleth$mode, opts$count, opts$naColor)
    plt0 <- plt(dt@data[[col]])
  }

  if (opts$percentage & nchar(opts$format[2]) == 0) {
    opts$format[2] <- "%"
  }
  # los labels y popups
  if (is.null(opts$label)) {
    label <- paste0("{point.id}",
                    ifelse(opts$count & !is.null(data),
                           paste0(": <b>", opts$format[1], "{point.", ifelse(opts$percentage, "percent", "b"), "}", opts$format[2], "</b>"),
                           ""))
  }
  if (is.null(opts$popup)) {
    popup <- paste0("{point.id}",
                    ifelse(opts$count & !is.null(data),
                           paste0(": <b>", opts$format[1], "{point.", ifelse(opts$percentage, "percent", "b"), "}", opts$format[2],"</b>"),
                           ""))
  }
  dt@data$label <- labelPopup(dt@data, label, opts$marks, opts$nDigits, opts$labelWrap)
  dt@data$popup <- labelPopup(dt@data, popup, opts$marks, opts$nDigits, opts$labelWrap)

  lf <- leaflet(dt)
  if (!is.null(opts$tiles)) {
    lf <- lf %>%
      addProviderTiles(opts$tiles)
  }
  lf <- lf %>%
    addPolygons(fillColor = plt0,
                label = ~map(label, ~shiny::HTML(.x)),
                popup = ~map(popup, ~shiny::HTML(.x)),
                color = opts$borderColor,
                fillOpacity = opts$borderOpacity,
                opacity = opts$opacity,
                weight = opts$borderWidth)
  if (!opts$legend$position %in% "no" & !is.null(data)) {
    lf <- lf %>%
      addLegend(bins = opts$nLevels,
                pal = plt,
                values = dt@data[[col]],
                labFormat = labelFormat0(prefix = opts$format[1],
                                         suffix = opts$format[2],
                                         big.mark = opts$marks[1],
                                         decimal.mark = opts$marks[2],
                                         digits = opts$nDigits),
                na.label = "NULL",
                opacity = opts$opacity,
                position = opts$legend$position,
                title = opts$legend$title)
  }

  if (opts$graticule) {
    lf <- lf %>%
      addGraticule(interval = 50, style =
                     list(color = "#4B4A4A", weight = 0.3))
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
                                   mapName = "world_countries",
                                   opts = NULL
                                   ) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  opts <- getOpts(opts = opts)

  plt0 <- opts$defaultFill

  dt <- geojsonio::topojson_read(geodataTopojsonPath(mapName))

  if (is.null(opts$nDigits)) opts$nDigits <- 2

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    if (opts$count) {
      d <- d  %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                               b = ifelse(is.character(d$b), "NA", NA))) %>%
        # tidyr::drop_na() %>%
        dplyr::group_by(a, b) %>%
        dplyr::summarise(c = n()) %>%
        dplyr::arrange(desc(c)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percent = c * 100 / sum(c, na.rm = TRUE))

      col <- ifelse(opts$percentage, "percent", "c")
    } else {
      d <- d %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                               b = ifelse(is.character(d$b), "NA", NA))) %>%
        dplyr::group_by(a, b) %>%
        dplyr::summarise(c = n()) %>%
        dplyr::arrange(desc(c)) %>%
        dplyr::slice(1) %>%
        dplyr::select(a, b)

      col <- "b"
    }
    dt@data <- dt@data %>%
      dplyr::left_join(d, by = c(id = "a"))

    plt <- fillColorsChoropleth(dt@data, col, opts$color, opts$scale, opts$nLevels, opts$mode, opts$count, opts$naColor)
    plt0 <- plt(dt@data[[col]])
  }

  if (opts$percentage & nchar(opts$format[2]) == 0) {
    opts$format[2] <- "%"
  }
  # los labels y popups
  if (is.null(opts$label)) {
    label <- paste0("{point.id} <br/><b> {point.b}",
                    ifelse(opts$count & !is.null(data),
                           paste0(": ", opts$format[1], "{point.", ifelse(opts$percentage, "percent", "c"), "}", opts$format[2], "</b>"),
                           ""))
  }
  if (is.null(opts$popup)) {
    popup <- paste0("{point.id} <br/><b> {point.b}",
                    ifelse(opts$count & !is.null(data),
                           paste0(": ", opts$format[1], "{point.", ifelse(opts$percentage, "percent", "c"), "}", opts$format[2], "</b>"),
                           ""))
  }
  dt@data$label <- labelPopup(dt@data, label, opts$marks, opts$nDigits, opts$labelWrap)
  dt@data$popup <- labelPopup(dt@data, popup, opts$marks, opts$nDigits, opts$labelWrap)
  lf <- leaflet(dt)
  if (!is.null(opts$tiles)) {
    lf <- lf %>%
      addProviderTiles(opts$tiles)
  }
  lf <- lf %>%
    addPolygons(fillColor = plt0,
                label = ~map(label, ~shiny::HTML(.x)),
                popup = ~map(popup, ~shiny::HTML(.x)),
                color = opts$borderColor,
                fillOpacity = opts$opacity,
                opacity = opts$borderOpacity,
                weight = opts$borderWidth)
  if (!opts$legend$position %in% "no" & !is.null(data)) {
    ca <- unique(dt@data[[col]])
    ct <- as.numeric(ca[!is.na(ca)])
    if (length(ct) == 1) {
      ct <- ct + 1
    }
    lf <- lf %>%
      addLegend(bins = opts$nLevels,
                pal = plt,
                values = union(ca, ct),
                labFormat = labelFormat0(prefix = opts$format[1],
                                         suffix = opts$format[2],
                                         big.mark = opts$marks[1],
                                         decimal.mark = opts$marks[2],
                                         digits = opts$nDigits),
                na.label = "NULL",
                opacity = opts$opacity,
                position = opts$legend$position,
                title = opts$legend$title)
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
                                   mapName = "world_countries",
                                   opts = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  opts <- getOpts(opts = opts)

  plt0 <- opts$defaultFill

  dt <- geojsonio::topojson_read(geodataTopojsonPath(mapName))

  if (is.null(opts$nDigits)) opts$nDigits <- 2

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d

    d <- d  %>%
      tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                             b =  NA)) %>%
      # tidyr::drop_na() %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = agg(opts$agg, b)) %>%
      dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))

    col <- ifelse(opts$percentage, "percent", "b")

    dt@data <- dt@data %>%
      dplyr::left_join(d, by = c(id = "a"))

    plt <- fillColorsChoropleth(dt@data, col, opts$color, opts$scale, opts$nLevels, opts$mode, TRUE, opts$naColor)
    plt0 <- plt(dt@data[[col]])
  }

  if (opts$percentage & nchar(opts$format[2]) == 0) {
    opts$format[2] <- "%"
  }
  # los labels y popups
  if (is.null(opts$label)) {
    # label <- paste0("{point.id}: <b> {point.", ifelse(percentage, "percent", "b"), "} </b>")
    label <- paste0("{point.id} <br/><b> {point.b}",
                    ifelse(!is.null(data),
                           paste0(": ", opts$format[1], "{point.", ifelse(opts$percentage, "percent", "b"), "}", opts$format[2], "</b>"),
                           ""))

  }
  if (is.null(opts$popup)) {
    popup <- label <- paste0("{point.id}: <b> {point.", ifelse(opts$percentage, "percent", "b"), "} </b>")
  }
  dt@data$label <- labelPopup(dt@data, label, opts$marks, opts$nDigits, opts$labelWrap)
  dt@data$popup <- labelPopup(dt@data, popup, opts$marks, opts$nDigits, opts$labelWrap)

  lf <- leaflet(dt)
  if (!is.null(opts$tiles)) {
    lf <- lf %>%
      addProviderTiles(opts$tiles)
  }
  lf <- lf %>%
    addPolygons(fillColor = plt0,
                label = ~map(label, ~shiny::HTML(.x)),
                popup = ~map(popup, ~shiny::HTML(.x)),
                color = opts$borderColor,
                fillOpacity = opts$opacity,
                opacity = opts$borderOpacity,
                weight = opts$borderWidth)
  if (!opts$legend$position %in% "no" & !is.null(data)) {
    lf <- lf %>%
      addLegend(bins = opts$nLevels,
                pal = plt,
                values = dt@data[[col]],
                labFormat = labelFormat0(prefix = opts$format[1],
                                         suffix = opts$format[2],
                                         big.mark = opts$marks[1],
                                         decimal.mark = opts$marks[2],
                                         digits = opts$nDigits),
                opacity = opts$opacity,
                position = opts$legend$position,
                title = opts$legend$title)
  }
  lf
}

#' Leaflet choropleths by geographical code
#'
#' @name lflt_choropleth_Gnm
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' lflt_choropleth_Gnm(sampleData("Gnm", nrow = 10))
lflt_choropleth_Gnm <- function(data = NULL,
                                mapName = "world_countries",
                                opts = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  opts <- getOpts(opts = opts)

  dt <- geojsonio::topojson_read(geodataTopojsonPath(mapName))


  plt0 <- opts$defaultFill

  if (is.null(opts$nDigits)) opts$nDigits <- 2

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    if (opts$count) {
      d <- d  %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
        dplyr::group_by(a) %>%
        dplyr::summarise(b = n()) %>%
        dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))

      col <- ifelse(opts$percentage, "percent", "b")
    } else {
      d <- d %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
        dplyr::group_by(a) %>%
        dplyr::slice(1) %>%
        dplyr::mutate(c = a)

      opts$legend$position <- "no"
      col <- "c"
    }
    d$a <- as.character(d$a)
    dt@data <- dt@data %>%
      dplyr::left_join(d, by = c("name" = "a"))

    plt <- fillColorsChoropleth(dt@data, col, opts$color, opts$scale, opts$nLevels, opts$legend$choropleth$mode, opts$count, opts$naColor)
    plt0 <- plt(dt@data[[col]])
  }

  if (opts$percentage & nchar(opts$format[2]) == 0) {
    opts$format[2] <- "%"
  }
  # los labels y popups
  if (is.null(opts$label)) {
    label <- paste0("{point.name}",
                    ifelse(opts$count & !is.null(data),
                           paste0(": <b>", opts$format[1], "{point.", ifelse(opts$percentage, "percent", "b"), "}", opts$format[2], "</b>"),
                           ""))
  }
  if (is.null(opts$popup)) {
    popup <- paste0("{point.name}",
                    ifelse(opts$count & !is.null(data),
                           paste0(": <b>", opts$format[1], "{point.", ifelse(opts$percentage, "percent", "b"), "}", opts$format[2],"</b>"),
                           ""))
  }
  dt@data$label <- labelPopup(dt@data, label, opts$marks, opts$nDigits, opts$labelWrap)
  dt@data$popup <- labelPopup(dt@data, popup, opts$marks, opts$nDigits, opts$labelWrap)

  lf <- leaflet(dt)
  if (!is.null(opts$tiles)) {
    lf <- lf %>%
      addProviderTiles(opts$tiles)
  }
  lf <- lf %>%
    addPolygons(fillColor = plt0,
                label = ~map(label, ~shiny::HTML(.x)),
                popup = ~map(popup, ~shiny::HTML(.x)),
                color = opts$borderColor,
                fillOpacity = opts$borderOpacity,
                opacity = opts$opacity,
                weight = opts$borderWidth)
  if (!opts$legend$position %in% "no" & !is.null(data)) {
    lf <- lf %>%
      addLegend(bins = opts$nLevels,
                pal = plt,
                values = dt@data[[col]],
                labFormat = labelFormat0(prefix = opts$format[1],
                                         suffix = opts$format[2],
                                         big.mark = opts$marks[1],
                                         decimal.mark = opts$marks[2],
                                         digits = opts$nDigits),
                na.label = "NULL",
                opacity = opts$opacity,
                position = opts$legend$position,
                title = opts$legend$title)
  }

  if (opts$graticule) {
    lf <- lf %>%
      addGraticule(interval = 50, style =
                     list(color = "#4B4A4A", weight = 0.3))
  }

  lf

}


#' Leaflet choropleths by geographical variable
#'
#' Leaflet choropleths by geographical variable
#'
#' @name lflt_choropleth_GnmCat
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Cat
#' @export
#' @examples
#' lflt_choropleth_GnmCat(sampleData("Gnm-Cat", nrow = 10))
lflt_choropleth_GnmCat <- function(data = NULL,
                                   mapName = "world_countries",
                                   opts = NULL
) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  opts <- getOpts(opts = opts)

  plt0 <- opts$defaultFill

  dt <- geojsonio::topojson_read(geodataTopojsonPath(mapName))

  if (is.null(opts$nDigits)) opts$nDigits <- 2

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    if (opts$count) {
      d <- d  %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                               b = ifelse(is.character(d$b), "NA", NA))) %>%
        # tidyr::drop_na() %>%
        dplyr::group_by(a, b) %>%
        dplyr::summarise(c = n()) %>%
        dplyr::arrange(desc(c)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percent = c * 100 / sum(c, na.rm = TRUE))

      col <- ifelse(opts$percentage, "percent", "c")
    } else {
      d <- d %>%
        tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                               b = ifelse(is.character(d$b), "NA", NA))) %>%
        dplyr::group_by(a, b) %>%
        dplyr::summarise(c = n()) %>%
        dplyr::arrange(desc(c)) %>%
        dplyr::slice(1) %>%
        dplyr::select(a, b)

      col <- "b"
    }
    dt@data <- dt@data %>%
      dplyr::left_join(d, by = c("name" = "a"))

    plt <- fillColorsChoropleth(dt@data, col, opts$color, opts$scale, opts$nLevels, opts$mode, opts$count, opts$naColor)
    plt0 <- plt(dt@data[[col]])
  }

  if (opts$percentage & nchar(opts$format[2]) == 0) {
    opts$format[2] <- "%"
  }
  # los labels y popups
  if (is.null(opts$label)) {
    label <- paste0("{point.id} <br/><b> {point.b}",
                    ifelse(opts$count & !is.null(data),
                           paste0(": ", opts$format[1], "{point.", ifelse(opts$percentage, "percent", "c"), "}", opts$format[2], "</b>"),
                           ""))
  }
  if (is.null(opts$popup)) {
    popup <- paste0("{point.id} <br/><b> {point.b}",
                    ifelse(opts$count & !is.null(data),
                           paste0(": ", opts$format[1], "{point.", ifelse(opts$percentage, "percent", "c"), "}", opts$format[2], "</b>"),
                           ""))
  }
  dt@data$label <- labelPopup(dt@data, label, opts$marks, opts$nDigits, opts$labelWrap)
  dt@data$popup <- labelPopup(dt@data, popup, opts$marks, opts$nDigits, opts$labelWrap)
  lf <- leaflet(dt)
  if (!is.null(opts$tiles)) {
    lf <- lf %>%
      addProviderTiles(opts$tiles)
  }
  lf <- lf %>%
    addPolygons(fillColor = plt0,
                label = ~map(label, ~shiny::HTML(.x)),
                popup = ~map(popup, ~shiny::HTML(.x)),
                color = opts$borderColor,
                fillOpacity = opts$opacity,
                opacity = opts$borderOpacity,
                weight = opts$borderWidth)
  if (!opts$legend$position %in% "no" & !is.null(data)) {
    ca <- unique(dt@data[[col]])
    ct <- as.numeric(ca[!is.na(ca)])
    if (length(ct) == 1) {
      ct <- ct + 1
    }
    lf <- lf %>%
      addLegend(bins = opts$nLevels,
                pal = plt,
                values = union(ca, ct),
                labFormat = labelFormat0(prefix = opts$format[1],
                                         suffix = opts$format[2],
                                         big.mark = opts$marks[1],
                                         decimal.mark = opts$marks[2],
                                         digits = opts$nDigits),
                na.label = "NULL",
                opacity = opts$opacity,
                position = opts$legend$position,
                title = opts$legend$title)
  }
  lf
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
lflt_choropleth_GnmNum <- function(data = NULL,
                                   mapName = "world_countries",
                                   opts = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  opts <- getOpts(opts = opts)

  plt0 <- opts$defaultFill

  dt <- geojsonio::topojson_read(geodataTopojsonPath(mapName))

  if (is.null(opts$nDigits)) opts$nDigits <- 2

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d

    d <- d  %>%
      tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                             b =  NA)) %>%
      # tidyr::drop_na() %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = agg(opts$agg, b)) %>%
      dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))

    col <- ifelse(opts$percentage, "percent", "b")

    dt@data <- dt@data %>%
      dplyr::left_join(d, by = c("name" = "a"))

    plt <- fillColorsChoropleth(dt@data, col, opts$color, opts$scale, opts$nLevels, opts$mode, TRUE, opts$naColor)
    plt0 <- plt(dt@data[[col]])
  }

  if (opts$percentage & nchar(opts$format[2]) == 0) {
    opts$format[2] <- "%"
  }
  # los labels y popups
  if (is.null(opts$label)) {
    # label <- paste0("{point.id}: <b> {point.", ifelse(percentage, "percent", "b"), "} </b>")
    label <- paste0("{point.name} <br/><b> {point.b}",
                    ifelse(!is.null(data),
                           paste0(": ", opts$format[1], "{point.", ifelse(opts$percentage, "percent", "b"), "}", opts$format[2], "</b>"),
                           ""))

  }
  if (is.null(opts$popup)) {
    popup <- label <- paste0("{point.name}: <b> {point.", ifelse(opts$percentage, "percent", "b"), "} </b>")
  }
  dt@data$label <- labelPopup(dt@data, label, opts$marks, opts$nDigits, opts$labelWrap)
  dt@data$popup <- labelPopup(dt@data, popup, opts$marks, opts$nDigits, opts$labelWrap)

  lf <- leaflet(dt)
  if (!is.null(opts$tiles)) {
    lf <- lf %>%
      addProviderTiles(opts$tiles)
  }
  lf <- lf %>%
    addPolygons(fillColor = plt0,
                label = ~map(label, ~shiny::HTML(.x)),
                popup = ~map(popup, ~shiny::HTML(.x)),
                color = opts$borderColor,
                fillOpacity = opts$opacity,
                opacity = opts$borderOpacity,
                weight = opts$borderWidth)
  if (!opts$legend$position %in% "no" & !is.null(data)) {
    lf <- lf %>%
      addLegend(bins = opts$nLevels,
                pal = plt,
                values = dt@data[[col]],
                labFormat = labelFormat0(prefix = opts$format[1],
                                         suffix = opts$format[2],
                                         big.mark = opts$marks[1],
                                         decimal.mark = opts$marks[2],
                                         digits = opts$nDigits),
                opacity = opts$opacity,
                position = opts$legend$position,
                title = opts$legend$title)
  }
  lf
}
