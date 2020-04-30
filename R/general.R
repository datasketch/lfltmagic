  library(lfltmagic)

  map_name <- "col_departments"
  data <- data.frame(depto = c("ANTIOQUIA", "QUINDIO", "NARIÑO", "RISARALDA", "GUAVIARE"), val = runif(5, 1, 100))

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  d <- d %>%
    drop_na() %>%
    mutate(name_alt = iconv(tolower(a), to = "ASCII//TRANSLIT"))

  lfmap <- geodataMeta(map_name)
  topoData <- topo_data(map_name)
  topoInfo <- topo_info(map_name)

  centroides <- data_centroid(lfmap$geoname, lfmap$basename)
  bbox <- topo_bbox(centroides$lon, centroides$lat)
  topoInfo@data$name_alt <- iconv(tolower(topoInfo@data$name), to = "ASCII//TRANSLIT")
  topoInfo@data  <- left_join(topoInfo@data, d, by = "name_alt")

  topoInfo@data <- lf_labels(topoInfo@data,
                             labels = "<strong>{a}</strong><br>Total: {b}",
                             popup = "<strong>{a}</strong><br>Total: {b}")


#
# bbox <- topo_bbox(centroides$lon, centroides$lat)
#
# colorFactor(, levels = 6)
pal <- colorNumeric(c("#FEAFEA", "#FFFCCC"), domain = c(0, max(d$b)))

lf_polygons(topoInfo, 2, "#000000", TRUE, 0.5) %>%
  addLegend(pal = pal, values = ~b, opacity = 1, title = "titulo",
            position = "bottomright", bins = 3)
