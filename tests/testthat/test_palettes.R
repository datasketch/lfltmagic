context("lflt_palette")

test_that("Palette colors", {

  data <-  data.frame(Countries = c(rep("IDN", 5), rep("RUS", 11), rep("SRB", 23), rep("NER", 37)))
  data$Countries <- as_Gcd(data$Countries)
  opts <- dsvizopts::dsviz_defaults()
  l <- lfltmagic_prep(data, opts, ftype = "Gcd", by_col = "id")
  opt_ptt <- list(color_scale = l$color_scale,
                  palette = l$palette_colors,
                  na_color = l$theme$na_color,
                  domain = as.numeric(l$topoInfo$value),
                  n_bins = l$n_bins,
                  n_quantile = l$n_quantile,
                  pretty = l$bins_pretty)
  expect_equal(attributes(lflt_palette(opt_ptt))$colorType, "numeric")
  lflt_basic_choropleth(l)


  data <- sample_data("Gcd-Cat")
  opts <- dsvizopts::dsviz_defaults()
  opts$style$color_by <- names(data)[2]
  opts$extra$map_color_scale <- "Category"
  l <- lfltmagic_prep(data, opts, ftype = "Gcd-Cat", by_col = "id")
  opt_ptt <- list(color_scale = l$color_scale,
                  palette = l$palette_colors,
                  na_color = l$theme$na_color,
                  domain = l$topoInfo$value,
                  n_bins = l$n_bins,
                  n_quantile = l$n_quantile,
                  pretty = l$bins_pretty)
  expect_equal(attributes(lflt_palette(opt_ptt))$colorType, "factor")
  lflt_basic_choropleth(l)


})
