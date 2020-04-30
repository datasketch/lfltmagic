
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
                                   map_name = "world_countries",
                                   add_tiles = TRUE,
                                   colors = c('#f0f05a','#f03f4e'),
                                   drop_na = FALSE,
                                   fill_opacity = 0.5,
                                   legend_show = TRUE,
                                   popup = NULL,
                                   tile_name = NULL,
                                   topo_color = "#CCCCCC",
                                   topo_fill = FALSE,
                                   topo_weight = 1,
                                   opts = NULL, ...) {

  if (is.null(map_name)) map_name <- "world_countries"

  if (!map_name %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  defaultOptions <- list(
    addTiles = add_tiles,
    colors = colors,
    drop_na = drop_na,
    fill_opacity = fill_opacity,

    legend_show = legend_show,

    border_color = topo_color,
    border_weight = topo_weight,
    popup = popup,

    topo_fill = topo_fill
  )

  opts <- modifyList(defaultOptions, opts %||% list())

  lfmap <- geodataMeta(map_name)
  topoData <- topo_data(map_name)
  topoInfo <- topo_info(map_name)

  centroides <- data_centroid(lfmap$geoname, lfmap$basename)
  bbox <- topo_bbox(centroides$lon, centroides$lat)


  lf <- lf_base(topoData, opts$border_weight, opts$border_color, opts$topo_fill)


  if (!is.null(data)) {
    data <- sampleData("Gnm-Num")
    f <- fringe(data)
    nms <- getClabels(f)
    d <- f$d
    d <- d %>%
          drop_na() %>%
           mutate(name_alt = iconv(tolower(a), to = "ASCII//TRANSLIT"))


    topoInfo@data$name_alt <- iconv(tolower(topoInfo@data$name), to = "ASCII//TRANSLIT")
    topoInfo@data  <- left_join(topoInfo@data, d, by = "name_alt")
    if (opts$drop_na) topoInfo@data <- topoInfo@data %>% drop_na()

    pal <- colorNumeric(opts$colors, domain = c(0, max(d$b)))

    lf <-  lf_polygons(topoInfo, opts$border_weight, opts$border_color, opts$topo_fill)
  } else {
    lf <- lf
  }

  lf  %>%
    fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])


}
