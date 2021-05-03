context("lflt magic prep")

test_that("Percentage", {

  data <- data.frame(Countries = c(rep("COL", 33),
                                   rep("BGD", 11),
                                   rep("LSO", 37)))
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts, by_col = "id", ftype = "Gcd")
  expect_equal(sum(l$data$..percentage, na.rm = TRUE), 100)


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
  dd <- data %>% group_by(Ciudad, Cosa) %>% summarise(total = n())
  d_test <- dd %>% ungroup() %>% group_by(Ciudad) %>% mutate(p = total/sum(total) * 100)
  l <- lfltmagic_prep(data, opts, by_col = "name", ftype = "Gnm-Cat")
  expect_equal(l$data %>% drop_na(..percentage) %>% .$..percentage, d_test$p)


  d_test <- dd %>% ungroup() %>% group_by(Cosa) %>% mutate(p = total/sum(total) * 100)
  opts$postprocess$percentage_col <- names(data)[2]
  l <- lfltmagic_prep(data, opts, by_col = "name", ftype = "Gnm-Cat")
  expect_equal(l$data %>% drop_na(..percentage) %>% .$..percentage, d_test$p)



  data <- data.frame(Ciudad = c(rep("Cauca", 15),
                                rep("chocó", 11),
                                rep("nariño", 31),
                                rep("Santander", 73)),
                     Cosa = runif(130)
  )
  opts <- dsvizopts::dsviz_defaults()
  opts$extra$map_name <- "col_departments"
  l <- lfltmagic_prep(data, opts, by_col = "name", ftype = "Gnm-Num")
  expect_equal(sum(l$data$..percentage, na.rm = TRUE), 100)

  data <- data.frame(Ciudad = c(rep("Cauca", 15),
                                rep("chocó", 11),
                                rep("nariño", 31),
                                rep("Santander", 73)),
                     Cosa = c(rep("Río", 7), rep("Montaña", 8),
                              rep("Mar", 5), rep("Montaña", 6),
                              rep("Volcanes", 13), rep("Río", 18),
                              rep("Montaña", 65), rep("Río", 8)),
                     Valor = runif(130, 100, 10000)
  )
  opts <- dsvizopts::dsviz_defaults()
  opts$extra$map_name <- "col_departments"
  l <- lfltmagic_prep(data, opts, by_col = "name", ftype = "Gnm-Cat-Num")
  dd <- data %>% group_by(Ciudad, Cosa) %>% summarise(Valor = sum(Valor))
  d_test <- dd %>% ungroup() %>% group_by(Ciudad) %>% mutate(p = Valor/sum(Valor) * 100)
  expect_equal(l$data %>% drop_na(..percentage) %>% .$..percentage, d_test$p)

})


