context("colors")

test_that("Factor", {

  opts <- list(
    color_scale = "Category",
    n_quantile = NULL,
    palette = c("#1a535c", "#00afb9", "#9d4edd", "#fed9b7"),
    domain = factor(letters[1:5]),
    na_color = "#DADADA"
  )
  pal <- lflt_palette(opts)
  sample_domain <- opts$domain[2:4]
  expect_identical(pal(sample_domain), pal(droplevels(sample_domain)))

  # Sending in values outside of the color scale should result in a warning and na_color
  expect_warning(pal(letters[10:20]))
  expect_equal(opts$na_color, suppressWarnings(unique(pal(letters[10:20]))))

})

test_that("Numeric", {

  opts <- list(
    color_scale = "Numeric",
    n_quantile = NULL,
    palette = c("#1a535c", "#00afb9", "#9d4edd", "#fed9b7"),
    domain = 1:10,
    na_color = "#DADADA"
  )
  pal <- lflt_palette(opts)
  sample_domain <- opts$domain[2:4]
  expect_equal(length(pal(sample_domain)), length(sample_domain))

})
