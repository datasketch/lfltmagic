context("lfltmagic meta data")

test_that("Viz meta info", {
  opts <- dsvizopts::dsviz_defaults()
  opts$extra$map_name <- "col_municipalities"
  l <- lfltmagic_prep(data=NULL, opts = opts)
  names_topoInfo <- names(l$topoInfo)
  centroides_mun <-  read_rds(suppressWarnings(geodata::geodataRdsPath(mapName = opts$extra$map_name)))
  expect_true(all(names_topoInfo %in% c(names(centroides_mun), "name_alt", "geometry", "labels")))


  opts <- dsvizopts::dsviz_defaults()
  opts$extra$map_name <- "col_departments"
  l <- lfltmagic_prep(data=NULL, opts = opts)
  names_topoInfo <- names(l$topoInfo)
  centroides <-  read_rds(suppressWarnings(geodata::geodataRdsPath(mapName = opts$extra$map_name)))
  expect_true(all(names_topoInfo %in% c(names(centroides), "name_alt", "geometry", "labels")))


  opts <- dsvizopts::dsviz_defaults()
  map_name <- "col_municipalities"
  data <-  fakeData(map_name = map_name)
  opts$extra$map_name <- "col_municipalities"
  l <- lfltmagic_prep(data=data, opts = opts)
  extra_cols <- c("a", "b", "..count", "..percentage", "..domain")
  expect_true(all(names_topoInfo %in% c(names(centroides_mun), "name_alt", "geometry", "labels",
                                       extra_cols, paste0(extra_cols, "_label"))))


})


