context("lflt magic prep")

test_that("Tooltip", {

  data <- sample_data("Gcd-Cat-Num-Num-Cat-Gnm")
  names(data) <- gsub("[][()*`|]", "", names(data))
  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  lf <- c("a", "c", "d")
  l_tooltip <- lflt_tooltip(nms,
                            label_ftype = lf,
                            tooltip = "")

  nms_filter <-  nms[names(nms) %in% lf]
  nms_names <- names(nms_filter)
  l <- map(seq_along(nms_filter), function(i){
    paste0("<span style='font-size:15px;'><strong>", nms_filter[[i]], ":</strong> {", nms_names[i], "_label}</span>")
  }) %>% unlist()
  tooltip <- paste0(l, collapse = "<br/>")
  expect_identical(l_tooltip, tooltip)

 tooltip_eg <- paste0(names(data)[6], ": {", names(data)[6], "}", "<br/>",
                      names(data)[3], ": {", names(data)[3], "}", "<br/>",
                      "tooltip test")
  l_tooltip <- lflt_tooltip(nms,
                            label_ftype = names(nms),
                            tooltip = tooltip_eg)
  tooltip_eg <- gsub(paste0("\\{",names(data)[6], "\\}"), paste0("{",letters[6], "_label}"), tooltip_eg)
  tooltip_eg <- gsub(paste0("\\{",names(data)[3], "\\}"), paste0("{",letters[3], "_label}"), tooltip_eg)
  expect_identical(l_tooltip, tooltip_eg)

})

