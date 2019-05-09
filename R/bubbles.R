#' Leaflet bubbles by category
#'
#' Leaflet bubbles by category
#'
#' @name lflt_bubbles_CatGlnGlt
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' lflt_bubbles_CatGlnGlt(sampleData("Cat-Gln-Glt", nrow = 10))
lflt_bubbles_CatGlnGlt <- function(data = NULL,
                                   mapName = NULL,
                                   opts = NULL, ...) {

  opts <- getOpts(opts = opts)

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""


  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    d <- d %>% drop_na(b, c)

    if (opts$dropNa) {
      d <- d %>%drop_na()
    } else {
      d$a[is.na(d$a)] <- 'N.A'
    }

    if (is.null(opts$colors)) {
      colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
    } else {
      colorDefault <- opts$colors
    }
    colorDefault <- discreteColor(colorDefault, d)
    categorias <- unique(d$a)

    d <- sp::SpatialPointsDataFrame(
      cbind(
        d$b,  # lng
        d$c  # lat
      ),
      data.frame(type = factor(
        d$a
      ))
    )


    if (!is.null(mapName)) {

      if (!mapName %in% availableGeodata()) {
        stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
      }

      topoData <- readLines(geodataTopojsonPath(mapName)) %>% paste(collapse = "\n")
      lf <-  leaflet(data = d) %>%
        addTopoJSON(topoData,
                    weight = opts$borderWidth,
                    color = opts$border_color,
                    fill = FALSE)
    } else {
      lf <- leaflet(data = d)
    }

    pal <- colorFactor(colorDefault, domain = categorias)

    lf <- lf %>%
      addTiles() %>%
      addCircleMarkers(
        radius = opts$radius,
        color = ~pal(type),
        stroke = opts$strike,
        fillOpacity = opts$fill_opacity,
        label = ~type
      )

    if(opts$legend) {
      lf <- lf %>%
        addLegendCustom(colors = colorDefault,
                        labels = sort(categorias),
                        sizes = rep(opts$legend_size, length(colorDefault)),
                        position = opts$legend_position)
    }
} else {
  if (!is.null(mapName)) {
    if (!mapName %in% availableGeodata()) {
      stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
    }

    topoData <- readLines(geodataTopojsonPath(mapName)) %>% paste(collapse = "\n")

    lf <-  leaflet() %>%
      addTopoJSON(topoData,
                  weight = opts$borderWidth,
                  color = opts$border_color,
                  fill = FALSE)

  }
}


if (!is.null(opts$tiles)) {
  lf <- lf %>%
    addProviderTiles(opts$tiles)
}

if (opts$graticule) {
  lf <- lf %>%
    addGraticule(interval = opts$graticule_interval,
                 style = list(color = opts$graticule_color, weight = opts$graticule_weight))
}


lf %>%
  addControl(caption,
             position = "bottomright",
             className="map-caption")
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
lflt_bubbles_GcdNum <- function(data = NULL,
                                mapName = "world_countries",
                                opts = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }


  # d <- fillColors(d, "b", "#654f6a", opts$scale, NULL, NULL,
  #                 labelWrap = opts$labelWrap, numeric = TRUE)


  lfmap <- geodataMeta(mapName)
  lfmap$path <- file.path("geodata",lfmap$geoname,paste0(lfmap$basename,".topojson"))
  lfmap$centroides <- file.path("geodata",lfmap$geoname,paste0(lfmap$basename,".csv"))
  tj <- readLines(system.file(lfmap$path, package = "geodata")) %>% paste(collapse = "\n")

  centroides <- read_csv(system.file(lfmap$centroides,package = "geodata"))

  nid <- names(data)
  centroides <- centroides %>% plyr::rename(c("id" = nid[1]))
  data <- data %>% dplyr::left_join(centroides)

  data <- data %>% dplyr::select_("lat", "lon", nid[2])
  opts <- getOpts(opts = opts)


  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d  %>%
    tidyr::replace_na(list(a = NA,
                           b = NA,
                           c = NA)) %>%
    # tidyr::drop_na() %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg("sum", c)) %>%
    dplyr::mutate(percent = c * 100 / sum(c, na.rm = TRUE))

  d$color <- "#68516c"

  if (opts$percentage & nchar(opts$format[2]) == 0) {
    opts$format[2] <- "%"
  }
  # los labels y popups
  # if (is.null(opts$label)) {
  #   label <- paste0("Lng: <b> {point.a} </b><br/> Lat: <b> {point.b} <br/>",
  #                   opts$format[1], "{point.",
  #                   ifelse(opts$percentage, "percent}", "c}"),
  #                   opts$format[2], "<br/>",
  #                   "</b>")
  # }
  # if (is.null(opts$popup)) {
  #   popup <- paste0("Lng: <b> {point.a} </b><br/> Lat: <b> {point.b} <br/>",
  #                   opts$format[1], "{point.",
  #                   ifelse(opts$percentage, "percent}", "c}"),
  #                   opts$format[2], "<br/>",
  #                   "</b>")
  # }

  d <- d %>% drop_na(b)

  nDigits <- opts$nDigits
  if (is.null(nDigits)) nDigits <- 0

  #d$label <- labelPopup(d, opts$label, opts$marks)
  #d$popup <- labelPopup(d, opts$popup, opts$marks)

  lf <- leaflet(d)
  lf <- lf %>%
    addTopoJSON(tj,
                weight = opts$borderWeigth,
                color = opts$borderColor,
                fillColor = opts$fillColor,
                fillOpacity = opts$fillOpacity)
  if (!is.null(opts$tiles)) {
    lf <- lf %>%
      addProviderTiles(opts$tiles)
  }
  lf <- lf %>%
    addCircleMarkers(lng = ~as.numeric(a),
                     lat = ~as.numeric(b),
                     color = ~color,
                     radius = ~scales::rescale(sqrt(c), to = c(opts$size[1], opts$size[2])),
                     #label = ~c,
                     stroke = TRUE)

  lf
}
