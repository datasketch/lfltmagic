context("lflt magic prep")

test_that("Categories Format", {

  data <-  data.frame(c(rep("IDN", 5), rep("RUS", 11), rep("SRB", 23), rep("NER", 37)))
  names(data) <- "Countries"
  data <- data %>% filter(Countries != "-99")
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts, ftype = "Gcd", by_col = "id")
  expect_equal(l$d@data %>% drop_na(a_label) %>% .$a_label, sort(unique(data$Countries)))


  opts <- dsvizopts::dsviz_defaults()
  opts$style$format_sample_cat <- "tolower"
  l <- lfltmagic_prep(data, opts, ftype = "Gcd", by_col = "id")
  expect_equal(l$d@data %>% drop_na(a_label) %>% .$a_label, tolower(sort(unique(data$Countries))))


})


test_that("Numeric Format", {

  data <- sample_data("Gcd-Num-Num", n = 1000, addNA = F)
  names(data) <- c("a", "b", "c")
  data <- data %>% filter(a != "-99")
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts, by_col = "id", ftype = "Gcd-Num-Num")
  data <- data %>% group_by(a) %>% summarise(b = sum(b, na.rm = T))
  expect_equal(sort(l$d@data %>% drop_na(b_label) %>% .$b_label), sort(makeup_num(as.vector(data$b), "1,234.34")))

  data <- sample_data("Gcd-Num-Num", n = 1000, addNA = F)
  names(data) <- c("a", "b", "c")
  data$test <- runif(1000, 3000, 7000)
  opts <- dsvizopts::dsviz_defaults()
  opts$style$format_sample_num <- "1.234,1"
  l <- lfltmagic_prep(data, opts, by_col = "id", ftype = "Gcd-Num-Num")
  data <- data %>% group_by(a) %>% summarise(test = sum(test, na.rm = T))
  expect_equal(sort(l$d@data %>% drop_na(d_label) %>% .$d_label), sort(makeup_num(as.vector(data$test), opts$style$format_sample_num)))


})
