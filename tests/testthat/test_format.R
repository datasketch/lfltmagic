context("lflt magic prep")

test_that("Categories Format", {

  data <-  data.frame(c(rep("IDN", 5), rep("RUS", 11), rep("SRB", 23), rep("NER", 37)), stringsAsFactors = FALSE)
  names(data) <- "Countries"
  data$Countries <- as_Gcd(data$Countries)
  data <- data %>% filter(Countries != "-99")
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts, ftype = "Gcd", by_col = "id")
  expect_equal(l$topoInfo %>% drop_na(a_label) %>% .$a_label, sort(as.vector(unique(data$Countries))))


  opts <- dsvizopts::dsviz_defaults()
  opts$style$format_sample_cat <- "tolower"
  l <- lfltmagic_prep(data, opts, ftype = "Gcd", by_col = "id")
  expect_equal(l$topoInfo %>% drop_na(a_label) %>% .$a_label, tolower(sort(as.vector(unique(data$Countries)))))


})


test_that("Numeric Format", {

  data <- sample_data("Gcd-Num-Num", n = 1000, addNA = F)
  names(data) <- c("a", "b", "c")
  data <- data %>% filter(a != "-99")
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts, by_col = "id", ftype = "Gcd-Num")
  data_test <- data %>% group_by(a) %>% summarise(b = sum(b, na.rm = T)) %>% arrange(b)
  # expect_equal((l$topoInfo %>% arrange(b) %>%  drop_na(b) %>% distinct(id, .keep_all = TRUE) %>% .$b_label),
  #              makeup_num(as.vector(data_test$b), "1,234.34"))

})
