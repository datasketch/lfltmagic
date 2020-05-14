
#' @export
lfltmagic_prep <- function(data = NULL, opts = NULL, by_col = "name", ...) {

  map_name <- opts$extra$map_name
  topoInfo <- topo_info(map_name)

  if (is.null(data)) {
    topoInfo@data <- topoInfo@data %>%
      mutate(labels = glue::glue('<strong>{name}</strong>') %>% lapply(htmltools::HTML))
   } else {

    f <- homodatum::fringe(data)
    nms <- homodatum::fringe_labels(f)
    d <- homodatum::fringe_data(f)


    frtype_d <- f$frtype
    d_frtype <- strsplit(frtype_d, split = "-") %>% unlist()
    var_num <- grep("Num", d_frtype)

    if (identical(var_num, integer())) {
      if (length(d_frtype) == 1) {
        d <- d %>%
          dplyr::group_by_all() %>%
          dplyr::summarise(b = n())
        nms[2] <- opts$summarize$agg_text %||% "Count"}
      if (length(d_frtype) == 2) {
        d <- d %>%
          dplyr::group_by_all() %>%
          dplyr::summarise(c = n())
        nms[3] <-  opts$summarize$agg_text %||% "Count"}
    }

    # Summarize
    d <- summarizeData(d, opts$summarize$agg, to_agg = b, a)

    if (opts$postprocess$percentage) {
      opts$style$suffix <- "%"
      d <- d %>% group_by(a) %>%
        dplyr::mutate(b = (b / sum(b, na.rm = TRUE)) * 100)
    }


    d$b_label <- makeup::makeup_num(d$b, opts$style$format_num_sample, locale = opts$style$locale)
    d <- d %>%
      drop_na(a) %>%
      mutate(name_alt = iconv(tolower(a), to = "ASCII//TRANSLIT"))

    topoInfo@data$name_alt <- iconv(tolower(topoInfo@data[[by_col]]), to = "ASCII//TRANSLIT")
    topoInfo@data  <- left_join(topoInfo@data, d, by = "name_alt")
    topoInfo@data$name <- makeup::makeup_chr(topoInfo@data$name, opts$style$format_cat_sample)

    if (is.null(opts$chart$tooltip)) {
      topoInfo@data <- topoInfo@data %>%
        mutate(labels = ifelse(is.na(a), glue::glue("<strong>{name}</strong>"),
                               glue::glue(paste0('<strong>{name}</strong><br/>',
                                                 nms[2], ": ", '{b_label}'))) %>% lapply(htmltools::HTML))
    } else {
      topoInfo@data <- topoInfo@data %>%
        mutate(labels = ifelse(is.na(a), glue::glue("<strong>{name}</strong>"),
                               glue::glue(opts$chart$tooltip)) %>% lapply(htmltools::HTML))
    }
    if (opts$preprocess$drop_na) topoInfo@data <- topoInfo@data %>% drop_na()
  }

  list(
  d = topoInfo,
  map_name = map_name,
  preprocess = opts$preprocess,
  summarize = opts$summarize,
  na_label = opts$preprocess$na_label,
  suffix = opts$style$suffix,
  prefix = opts$tyle$prefix,
  titles = opts$title,
  bubbles = list(bubble_min = opts$extra$bubble_min,
                 bubble_max = opts$extra$bubble_max,
                 bubble_opacity = opts$extra$bubble_opacity),
  graticule = list(map_graticule = opts$extra$map_graticule,
                   map_graticule_color = opts$extra$map_graticule_color,
                   map_graticule_interval = opts$extra$map_graticule_interval,
                   map_graticule_weight = opts$extra$map_graticule_weight),
  theme = opts$theme
  )

}

