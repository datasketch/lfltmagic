#' @export
lflt_palette <- function(opts) {
  if (opts$color_scale == "Category") {
    color_mapping <- "colorFactor"
    l <- list(levels = opts$levels,
              ordered = opts$ordered)
  } else if (opts$color_scale == "Quantile") {
    color_mapping <- "colorQuantile"
    l <- list(n = opts$n_quantile)
  } else if (opts$color_scale == 'Bins') {
    color_mapping <- "colorBin"
    l <- list(bins = opts$bins)
  } else {
    color_mapping <- "colorNumeric"
    l <- list()
  }
  l$palette <- opts$palette
  l$domain <- opts$domain
  l$na.color <- opts$na.color
  do.call(color_mapping, l)
}

#' labels
lflt_tooltip <- function(nms, tooltip) {
  if (is.null(nms)) stop("Enter names")
  nms_names <- names(nms)
  if (is.null(tooltip)) {
    l <- map(seq_along(nms), function(i){
      paste0("<strong>", nms[[i]], ":</strong> {", nms_names[i], "_label}")
    }) %>% unlist()
    tooltip <- paste0(l, collapse = "<br/>")
  } else {
    points <- gsub("\\{|\\}", "",
                   stringr::str_extract_all(tooltip, "\\{.*?\\}")[[1]])
    if (identical(points, character())) {
      tooltip <- tooltip
    } else {
      l <- purrr::map(1:length(points), function(i){
        true_points <-  paste0("{",names(nms[match(points[i], nms)]),"_label}")
        tooltip <<- gsub(paste0("\\{",points[i], "\\}"), true_points, tooltip)
      })[[length(points)]]}
  }
  tooltip
}

#' Format
lflt_format <- function(d, dic, nms, opts) {
  var_nums <- dic$id[dic$hdType %in% c("Num", "Gln", "Glt")]
  l_num <- map(var_nums, function(v) {
    d[[paste0(v, "_label")]] <<- ifelse(is.na(d[[v]]), NA,
                                        paste0(opts$prefix,
                                               makeup::makeup_num(v = d[[v]],
                                                                  opts$format_num_sample,
                                                                  locale = opts$locale),
                                               opts$suffix))
  })
  var_cats <- dic$id[dic$hdType %in% c("Cat", "Gnm", "Gcd")]
  l_cat <- map(var_cats, function(v) {
    d[[paste0(v, "_label")]] <<- ifelse(is.na(d[[v]]), NA,
                                        makeup::makeup_chr(v = d[[v]],
                                                           opts$format_cat_sample))
  })
  d
}
