context("helpers")

test_that("Tooltips", {

  data <- sample_data("Gcd")
  names(data) <- "Countries"
  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  t_t <- lflt_tooltip(nms = nms, tooltip = "test {Countries}")
  expect_equal(t_t,  "test {a_label}")


  data <- sample_data("Glt-Gln-Cat-Num-Num-Num")
  names(data) <- c("Lat", "Long", "Categories", "Values_uno", "Values_two", "Values_trhee")
  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  t_t <- lflt_tooltip(nms = nms, tooltip = "La Categoria {Categories} tiene {Values_uno} y {Values_trhee} mariposas")
  expect_equal(t_t,  "La Categoria {c_label} tiene {d_label} y {f_label} mariposas")

})
