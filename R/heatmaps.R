
#' lflt_heatmap_GltGln
#' Heatmap
#' @name lflt_heatmap_GltGln
#' @param x A data.frame
#' @export
#' @return leaflet viz
#' @section ftypes: Glt-Gln
#' @examples
#' lflt_heatmap_size_GltGln(sampleData("Glt-Gln",nrow = 10))
lflt_heatmap_GltGln <- function(data, radius = NULL){
  radius <- radius %||% 10

  f <- fringe(data)
  nms <- getClabels(f)

  d <- f$d %>% na.omit()

  leaflet(d) %>%
    addProviderTiles(providers$CartoDB) %>%
    addHeatmap(lat = ~a, lng = ~b, radius = radius)

}




