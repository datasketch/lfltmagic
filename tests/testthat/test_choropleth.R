context("lflt magic prep")


test_that("lfltmagic has a valid class after valid data input", {
  data(mpg, package = "ggplot2")
  lm <- lflt_choropleth_GnmNum(data)
  expect_true(all(class(lm) %in% c("leaflet","htmlwidget")))
})

test_that("Choropleth", {

  # choropleth Gnm
  data <- sample_data("Gnm", n = 30)
  lflt_choropleth_Gnm(data)

  # choropleth Gnm Num
  data <- sample_data("Gnm-Num", n = 30)
  lf <- lflt_choropleth_GnmNum(data)


  # choropleth Gnm Cat Num
  data <- sample_data("Gnm-Cat-Num", n = 30)
  lflt_choropleth_GnmCatNum(data,
                            color_by = names(data)[2],
                            palette_type = "sequential")
  # choropleth Gnm Cat
  data <- sample_data("Gnm-Cat", n = 30)
  lflt_choropleth_GnmCat(data)

  # choropleth Gcd Num
  data <- sample_data("Gcd-Num", n = 30)
  lflt_choropleth_GcdNum(data)

  # choropleth Gcd Cat
  data <- sample_data("Gcd-Cat", n = 100, addNA = F)
  lflt_choropleth_GcdCat(data)
  lflt_choropleth_GcdCat(data,
                      color_by = names(data)[2],
                      palette_type = "sequential")

  names_data <- names(data)
  info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[2],":</b> {", names_data[2],"}<br/>")
  data %>%
   lflt_choropleth_GcdCat(tooltip = info_tool)


  # choropleth Gcd Cat Num
  lflt_choropleth_GcdCatNum()

  df <- sample_data("Gcd-Cat-Num")
  lflt_choropleth_GcdCatNum(data = df)



})

