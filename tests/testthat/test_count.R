context("lflt magic prep")

test_that("Count", {

  data <- data.frame(Countries = c(rep("COL", 33),
                                   rep("BGD", 11),
                                   rep("LSO", 37)))
  data <- data %>% filter(Countries != "-99")
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts, by_col = "id", ftype = "Gcd")

  expect_equal(sum(l$d@data$..count, na.rm = TRUE), nrow(data))




  data <- data.frame(Ciudad = c(rep("Cauca", 15),
                                rep("chocó", 11),
                                rep("nariño", 31),
                                rep("Santander", 73)),
                      Cosa = c(rep("Río", 7), rep("Montaña", 8),
                               rep("Mar", 5), rep("Montaña", 6),
                               rep("Volcanes", 13), rep("Río", 18),
                               rep("Montaña", 65), rep("Río", 8))
                     )

  opts <- dsvizopts::dsviz_defaults()
  opts$extra$map_name <- "col_departments"
  l <- lfltmagic_prep(data, opts, by_col = "name", ftype = "Gnm-Cat")

  expect_equal(sum(l$d@data$..count, na.rm = T), nrow(data))



})


