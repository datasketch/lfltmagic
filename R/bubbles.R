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

  opts <- getOpts(opts = opts)

  mapResults <- layerMap(mapName = mapName,
                         borderColor = opts$borderColor,
                         borderWeigth = opts$borderWidth,
                         fillColor = opts$defaultFill,
                         fillOpacity = opts$opacity)

  centroides <- mapResults[[1]]
  lfGraph <- mapResults[[2]]

}
