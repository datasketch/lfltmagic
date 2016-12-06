
#' lflt_bubbles_GeNu
#' Multilines
#' @name lflt_bubbles_GeNu
#' @param x A data.frame
#' @export
#' @return leaflet viz
#' @section ftypes: Ye-Nu*
#' @examples
#' lflt_bubbles_GeNu(sampleData("Ge-Nu",nrow = 10))
lflt_bubbles_GeNu <- function(data,
                              geoinfoPath = NULL,
                              geoCodeVar = NULL){
  f <- fringe(data)
  nms <- getClabels(f)

  geoinfoPath <- geoinfoPath %||% system.file("aux/world-geo.csv",package = "lfltmagic")
  geoCodeVar <- geoCodeVar %||% "code3"
  geo <- read_csv(geoinfoPath)
  idx <- which(names(geo)==geoCodeVar)
  names(geo)[idx] <- "a"
  varLabel <- nms[2]

  dgeo <- f$d %>% na.omit() %>% group_by(a) %>% summarise(b = mean(b))
  d <- dgeo %>% left_join(geo[c("a","name","Lat","Lon")],"a")
  d$info <- pystr_format("<strong>{name}</strong><br>{selectedVarName}: {b}",d) %>% pystr_format(list(selectedVarName = varLabel))
  dd <- d %>% filter(!is.na(b))
  leaflet(dd) %>%
    #addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    #fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addCircleMarkers(lng = ~Lon, lat = ~Lat, weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(3,20)),
                     popup = ~info,
                     color = "navy",
                     stroke = FALSE
    )
}

#' lflt_bubbles_GeNuNu
#' Multilines
#' @name lflt_bubbles_GeNuNu
#' @param x A data.frame
#' @export
#' @return leaflet viz
#' @section ftypes: Ye-Nu*
#' @examples
#' lflt_bubbles_GeNuNu(sampleData("Ge-Nu-Nu",nrow = 10))
lflt_bubbles_GeNuNu <- function(data,
                                geoinfoPath = NULL,
                                geoCodeVar = NULL){

  f <- fringe(data)
  nms <- getClabels(f)

  geoinfoPath <- geoinfoPath %||% system.file("aux/world-geo.csv",package = "lfltmagic")
  geoCodeVar <- geoCodeVar %||% "code3"
  geo <- read_csv(geoinfoPath)
  idx <- which(names(geo)==geoCodeVar)
  names(geo)[idx] <- "a"
  varLabel <- nms[2:3]

  dgeo <- f$d %>% na.omit() %>% group_by(a) %>%
    summarise(b = mean(b), c = mean(c))
  d <- dgeo %>% left_join(geo[c("a","name","Lat","Lon")],"a")
  d$info <- pystr_format(
    "<strong>{name}</strong><br>{selectedVarName1}: {b}<br>{selectedVarName2}: {c}",d) %>%
    pystr_format(list(selectedVarName1 = varLabel[1],selectedVarName2 = varLabel[2]))
  dd <- d %>% filter(!is.na(b))
  leaflet(dd) %>%
    #addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    #fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addCircleMarkers(lng = ~Lon, lat = ~Lat, weight = 3,
                     radius = ~scales::rescale(sqrt(b), to = c(3,20)),
                     popup = ~info,
                     color = "navy",
                     stroke = FALSE
    ) %>%
    addCircleMarkers(lng = ~Lon, lat = ~Lat, weight = 3,
                     radius = ~scales::rescale(sqrt(c), to = c(3,20)),
                     popup = ~info,
                     color = "red",
                     stroke = FALSE
    )
}



