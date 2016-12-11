
library(devtools)
load_all()
document()
install()

library(lfltmagic)

# Bubbles

data <- read_csv("inst/aux/world-geo.csv")
data$country <- data$code3
data$value <- runif(6)
data <- data[c("code","value")]
lflt_bubbles_GeNu(data,
                  geoinfoPath = "inst/aux/world-geo.csv",
                  geoCodeVar = "code",
                  geoNameVar = "name" )


data$val2 <- runif(6)
lflt_bubbles_GeNuNu(data,
                    geoinfoPath = "inst/aux/world-geo.csv",
                    geoCodeVar = "code",
                    geoNameVar = "name" )


data <- read_csv("inst/data/col-cities-GeNu.csv")
bounds <- c(-76,0,-71,10)
lflt_bubbles_GeNu(data,
                  geoinfoPath = "inst/aux/col-cities-geo.csv",
                  geoCodeVar = "municipio",
                  bounds = bounds)





# Segments

#' @export
lflt_net <- function(f, cluster = FALSE){
  bounds <- c(-76,0,-71,8)
  geo <- read_csv("data/geo/mpios.csv",col_types = cols(Cod_Mpio = "c",Cod_Depto = "c")) %>%
    rename(a = Cod_Mpio)
  varLabel <- "Total"
  dgeo <- f$d
  d <- dgeo %>% left_join(geo[c("a","Lat","Lon","Depto","Mpio")],"a")
  d <- unite(d,"label",Depto,Mpio,sep=" - ")

  geo2 <- geo
  names(geo2) <- c("Depto2","Cod_Depto2","Lat2","Lon2","Mpio2","b" )
  d2 <- d %>% left_join(geo2[c("b","Lat2","Lon2","Depto2","Mpio2")],"b")
  names(d2)[3] <- "info"
  dd <- d2 %>% na.omit()
  dd <- dd %>% mutate(sameOriginDest = a==b) %>% filter(!sameOriginDest)

  createLineSegments <- function(r){
    data_frame(Lat = c(r$Lat, r$Lat2,NA),
               Lon = c(r$Lon, r$Lon2,NA))
  }
  ddLines <- by_row(dd[,c("Lat","Lat2","Lon","Lon2")],createLineSegments) %>%
    .$.out %>% bind_rows()

  addCluster <- function(cluster){
    if(cluster) return(markerClusterOptions())
    NULL
  }

  leaflet(dd) %>%
    #addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addPolylines(data = ddLines, lng = ~Lon, lat = ~Lat,
                 color = "grey", fillOpacity = 0.01) %>%
    addCircleMarkers(lng = ~Lon, lat = ~Lat, weight = 3,
                     radius = 5,
                     popup = ~info,
                     color = "red",
                     stroke = FALSE,
                     clusterOptions = addCluster(cluster)
    ) %>%
    addCircleMarkers(lng = ~Lon2, lat = ~Lat2, weight = 3,
                     radius = 5,
                     popup = ~info,
                     color = "green",
                     stroke = FALSE,
                     clusterOptions = addCluster(cluster)
    )
}




