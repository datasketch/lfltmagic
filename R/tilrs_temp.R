library(leaflet)
m <- leaflet() %>% setView(lng = -5.6641, lat = 40.9650, zoom = 7)
m %>% addProviderTiles(providers$CartoDB.Voyager)
leaflet() %>% addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png"
) %>% setView(-122.36, 47.67, zoom = 10)

library(leaflet.esri)
leaflet() %>%
  addEsriBasemapLayer(esriBasemapLayers$Topographic) %>%
  setView(lng = -3.691944, lat = 40.418888888889, zoom = 13) %>%
  # addEsriFeatureLayer(
  #   url = paste0("https://sigma.madrid.es/arcgismalla/rest/services/SIGMA/MPCARTO2D_WGS84_FONDO/MapServer/1"),
  #   labelProperty = "NOMDIS") %>% 
  addEsriFeatureLayer(
    url = paste0("https://sigma.madrid.es/arcgismalla/rest/services/SIGMA/MPCARTO2D_WGS84_FONDO/MapServer/6"),
    labelProperty = "NOMDIS") %>% 
  addEsriFeatureLayer(
    url = paste0("https://sigma.madrid.es/arcgismalla/rest/services/SIGMA/MPCARTO2D_WGS84_FONDO/MapServer/15"),
    labelProperty = "OBJECTID")
 

leaflet() %>%
  addEsriBasemapLayer(esriBasemapLayers$Streets) %>%
  setView(-122.667, 45.526, 13) %>%
  addEsriFeatureLayer(
    url = paste0("https://services.arcgis.com/rOo16HdIMeOBI4Mb/arcgis/rest/services/",
                 "Heritage_Trees_Portland/FeatureServer/0"),
    useServiceSymbology = TRUE,
    labelProperty = "COMMON_NAM", labelOptions = labelOptions(textsize = "12px"))
