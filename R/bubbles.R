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
