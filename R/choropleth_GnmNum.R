
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
lflt_choropleth_GnmNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)

  generalInfo <- lfltmagic_prep(data = data, opts = opts)

  topoInfo <- generalInfo$d@data
  topoInfo
  # lfmap <- geodataMeta(map_name)
  # centroides <- data_centroid(lfmap$geoname, lfmap$basename)
  # bbox <- topo_bbox(centroides$lon, centroides$lat)
  #
  # lf_polygons(topoInfo, opts_theme = opts$theme)

}
