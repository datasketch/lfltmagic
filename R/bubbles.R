

#' lflt_bubbles_size_GcdNum
#' Multilines
#' @name lflt_bubbles_size_GcdNum
#' @param x A data.frame
#' @export
#' @return leaflet viz
#' @section ftypes: Ye-Nu*
#' @examples
#' lflt_bubbles_size_GcdNum(sampleData("Gcd-Num",nrow = 10))
lflt_bubbles_size_GcdNum <- function(data,
                                     #infoVar = NULL,
                                     scope = "world",
                                     agg = NULL,
                                     bounds =  NULL
){
  f <- fringe(data)
  nms <- getClabels(f)

  geo <- geodataCsv(scope) %>% rename(a = id)
  dgeo <- f$d %>% na.omit() %>%
    group_by(a) %>%
    dplyr::summarise(b = mean(b)) %>%
    filter(b >= 0)

  dd <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")
  tpl <- str_tpl_format("<strong>{GcdName}: {a}</strong><br>{NumName}: {b}",
                        list(GcdName = nms[1], NumName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)

  l <- leaflet(dd) %>%
    #addTiles() %>%
    addProviderTiles("CartoDB.Positron")
  if(!is.null(bounds))
    l <- l %>%  fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  l  %>%  addCircleMarkers(lat = ~lat, lng = ~lon, weight = 3,
                           radius = ~scales::rescale(sqrt(b), to = c(3,20)),
                           popup = ~info,
                           color = "navy",
                           stroke = FALSE)
}


#' lflt_bubbles_size_GltLn
#' Multilines
#' @name lflt_bubbles_size_GltLn
#' @param x A data.frame
#' @export
#' @return leaflet viz
#' @section ftypes: Ye-Nu*
#' @examples
#' lflt_bubbles_size_GltLn(sampleData("Gcd-Num",nrow = 10))
lflt_bubbles_size_GltLn <- function(data,
                                    #infoVar = NULL,
                                    radius = NULL,
                                    bounds =  NULL){

  radius <- radius %||% 5
  f <- fringe(data)
  nms <- getClabels(f)
  dd <- f$d %>% na.omit()
  tpl <- str_tpl_format("<strong>{GltName}: {a}</strong><br>{GlnName}: {b}",
                        list(GltName = nms[1], GlnName = nms[2]))
  dd$info <- str_tpl_format(tpl,dd)

  l <- leaflet(dd) %>%
    addProviderTiles("CartoDB.Positron")
  if(!is.null(bounds))
    l <- l %>%  fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  l  %>%  addCircleMarkers(lat = ~a, lng = ~b, weight = 3,
                           #radius = ~scales::rescale(sqrt(b), to = c(3,20)),
                           radius = radius,
                           popup = ~info,
                           color = "navy",
                           stroke = FALSE)
}



