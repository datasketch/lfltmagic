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
                                   mapName = "world_countries",
                                   opts = NULL, ...) {

  if (is.null(data) & is.null(mapName)) return("You must call a data or mapName argument")

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
      lf <- leaflet(data = d) %>% addTiles()
    }

    pal <- colorFactor(colorDefault, domain = categorias)

    lf <- lf %>%
      addCircleMarkers(
        radius = opts$radius,
        color = ~pal(type),
        stroke = opts$stroke,
        fillOpacity = opts$fill_opacity,
        label = ~type
      )

    if(opts$legend_show) {
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


