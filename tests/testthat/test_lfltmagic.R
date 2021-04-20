context("lfltmagic meta data")

test_that("Viz meta info", {
  opts <- dsvizopts::dsviz_defaults()
  opts$extra$map_name <- "col_municipalities"
  l <- lfltmagic_prep(data=NULL, opts = opts)

  opts <- dsvizopts::dsviz_defaults()
  map_name <- "col_municipalities"
  data <-  fakeData(map_name = map_name)
  opts$extra$map_name <- "col_municipalities"
  l <- lfltmagic_prep(data=data, opts = opts)
  lflt_basic_choropleth(l)
})


