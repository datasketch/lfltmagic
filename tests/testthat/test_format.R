context("lflt magic prep")

test_that("Categories Format", {

  data <- sample_data("Gcd", n = 1000, addNA = F)
  names(data) <- "Countries"
  data <- data %>% filter(Countries != "-99")
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts)
  expect_equal(l$data$a, l$data$a_label)


  opts <- dsvizopts::dsviz_defaults()
  opts$style$format_sample_cat <- "tolower"
  l <- lfltmagic_prep(data, opts)
  expect_equal(tolower(l$data$a), l$data$a_label)


})


test_that("Numeric Format", {

  data <- sample_data("Gcd-Num-Num", n = 1000, addNA = F)
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts)
  expect_equal(trimws(format(l$data$b, big.mark = ",", digits = 2)), l$data$b_label)

  data <- sample_data("Gcd-Num-Num", n = 1000, addNA = F)
  data$test <- runif(1000, 3000, 7000)
  opts <- dsvizopts::dsviz_defaults()
  opts$style$format_sample_num <- "1.234,1"
  l <- lfltmagic_prep(data, opts)
  expect_equal(trimws(format(round(l$data$d, 1), big.mark = ".", decimal.mark = ",")), l$data$d_label)




})
