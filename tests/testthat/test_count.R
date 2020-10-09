context("lflt magic prep")

test_that("Count", {

  data <- sample_data("Gcd", n = 1000, addNA = F)
  names(data) <- "Countries"
  data <- data %>% filter(Countries != "-99")
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts)

  expect_equal(sum(l$data$b), nrow(data))




  data <- sample_data("Gnm-Cat", n = 5000, addNA = F)
  names(data) <- c("Countries", "Categories")
  data <- data %>% filter(Countries != "-99")
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts)

  expect_equal(sum(l$data$c), nrow(data))



})


