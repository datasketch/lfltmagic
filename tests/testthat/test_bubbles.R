context("lflt magic prep")

test_that("Bubbles", {

  data <- data.frame(Countries = c(rep("COL", 33),
                                   rep("BGD", 11),
                                   rep("LSO", 37)))
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts, by_col = "id", ftype = "Gcd")
  lfltmagic:::lflt_basic_bubbles(l)


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




test_that("Bubbles Gcd", {


  # Bubbles Gcd Cat
  data <- sample_data("Gcd-Cat-Dat-Yea-Cat", n = 30, addNA = F)
  lflt_bubbles_GcdCat(data)
  lflt_bubbles_GcdCat(data,
                      color_by = names(data)[2],
                      palette_type = "sequential")

  names_data <- names(data)
  info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[2],":</b> {", names_data[2],"}<br/>")
  data %>%
   lflt_bubbles_GcdCat(tooltip = info_tool)


  # Bubbles Gcd Cat Num
  lflt_bubbles_GcdCatNum()

  df <- sample_data("Gcd-Cat-Num")
  lflt_bubbles_GcdCatNum(data = df)


})

