
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
lflt_choropleth_GcdNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- lfltmagic_prep(data = data, opts = opts, by_col = "id")

  qpal <- colorNumeric(c("#FEAFEA", "#000AAA"), l$d@data$b)

  leaflet(l$d) %>%
    addPolygons( weight = l$theme$border_weight,
                 fillOpacity = l$theme$topo_fill_opacity,
                 opacity = 1,
                 color = ~qpal(b),
                 label = ~labels
    ) %>%
    addLegend(pal = qpal, values = ~b, opacity = 1, na.label = l$na_label, title = l$titles$legend_title)
}
