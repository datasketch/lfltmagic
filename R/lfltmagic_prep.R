
#' @export
lfltmagic_prep <- function(data = NULL, opts = NULL, by_col = "name", ...) {

  map_name <- opts$extra$map_name
  topoInfo <- topo_info(map_name)
  lfmap <- geodataMeta(map_name)
  centroides <- data_centroid(lfmap$geoname, lfmap$basename)
  bbox <- topoInfo@bbox
  if (is.null(bbox)) bbox <- topo_bbox(centroides$lon, centroides$lat)
  color_scale <- opts$extra$map_color_scale

  if (is.null(data)) {
    topoInfo@data <- topoInfo@data %>%
      mutate(labels = glue::glue('<strong>{name}</strong>') %>% lapply(htmltools::HTML))
  } else {

    f <- homodatum::fringe(data)
    nms <- homodatum::fringe_labels(f)
    d <- homodatum::fringe_d(f)
    dic <- guess_ftypes(data, map_name = map_name)
    dic$id <- names(nms)
    # true_ctype <- geoType(d, map_name)
    # print(true_ctype )
    # if (is.null(true_ctype)) true_ctype <- dic$id[1]
    # dic$hdType[dic$id == "a"] <- true_ctype
    frtype_d <- paste0(dic$hdType, collapse = "-")#f$frtype
    #frtype_d <- gsub("Num-Num", "Glt-")
    d_frtype <- strsplit(frtype_d, split = "-") %>% unlist()


    if(frtype_d %in% c("Gcd", "Gnm")){
      d <- d %>%
        mutate(a = na_if(a, "-99")) %>%
        drop_na(a) %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(b = n())

        ind_nms <- length(nms)+1
        nms[ind_nms] <- 'Count'
        names(nms) <- c(names(nms)[-ind_nms], 'b')
        dic_num <- data.frame(id = "b", label = "Count", hdType= as_hdType(x = "Num"))
        dic <- dic %>% bind_rows(dic_num)
        }

    if (frtype_d %in% c("Gnm-Cat", "Gcd-Cat")) {
      d_agg <- d %>%
        drop_na(a) %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(c = n())

      tooltips <- d_agg %>%
        mutate(html_row_unique = paste0("<strong>", nms[[2]], ":</strong> ", b, "</span>")) %>%
        group_by(a) %>%
        summarise(tooltip = ifelse(n()>1,
                             paste0("<strong>", nms[[2]], ":</strong> ", paste0(b, collapse = "/"), "</span>"),
                             html_row_unique)) %>%
        mutate(tooltip = paste0("<span style='font-size:15px;'><strong>", nms[[1]], ":</strong> ",a, "<br/>", tooltip))

      d <- d_agg %>%
        dplyr::filter(c == max(c)) %>%
        dplyr::group_by(a) %>%
        dplyr::mutate(d = n(), b = ifelse(d == 1, b, "tie")) %>%
        dplyr::distinct(a, b) %>%
        left_join(tooltips, by = "a")

      if(nrow(d) < nrow(d_agg)){
        warning("Multiple categorical classes found for at least one geography.
                Geographies are colored based on majority class and all counts are displayed in tooltips.
                Potential ties are indicated by new category 'tie'.")
        }
      }

    if (frtype_d %in% "Gln-Glt-Cat") {
      d <- d %>%
        drop_na(a, b) %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(d = n()) %>%
        dplyr::filter(d == max(d)) %>%
        dplyr::group_by(a, b) %>%
        dplyr::mutate(e = n(), c = ifelse(e == 1, c, "tie")) %>%
        dplyr::distinct(a, b, c, d)

        ind_nms <- length(nms)+1
        nms[ind_nms] <- 'Count'
        names(nms) <- c(names(nms)[-ind_nms], 'd')
        dic_num <- data.frame(id = "d", label = "Count", hdType= as_hdType(x = "Num"))
        dic <- dic %>% bind_rows(dic_num)}

    if (frtype_d %in% c("Gcd-Num", "Gnm-Num", "Cat-Num")) {
      d <- summarizeData(d, opts$summarize$agg, to_agg = b, a) %>% drop_na()}

    if (frtype_d %in% c("Gcd-Cat-Num", "Gnm-Cat-Num")) {
      d_agg <- summarizeData(d, opts$summarize$agg, to_agg = c, a, b) %>% mutate(a = na_if(a, "-99")) %>% drop_na(a)

      tooltips <- d_agg %>%
        mutate(c = makeup::makeup_num(c),
               html_row_unique = paste0("<strong>", nms[[2]], ":</strong> ", b, "<br/><strong>", nms[[3]], ":</strong> ", c),
               html_row = paste0("<strong>", b, ":</strong> ", c)) %>%
        group_by(a) %>%
        summarise(tooltip = ifelse(n()>1, paste0(html_row, collapse = "<br/>"), html_row_unique)) %>%
        mutate(tooltip = paste0("<span style='font-size:15px;'><strong>", nms[[1]], ":</strong> ",a, "<br/>", tooltip, "</span>"))

      d <- d_agg %>%
        dplyr::group_by(a) %>%
        dplyr::filter(c == max(c)) %>%
        dplyr::mutate(d = n(), b = ifelse(d == 1, b, "tie")) %>%
        dplyr::distinct(a, b, c) %>%
        left_join(tooltips, by = "a")

      if(nrow(d) < nrow(d_agg)){
        warning("Multiple categorical classes found for at least one geography.
                Geographies are colored based on majority class and all counts are displayed in tooltips.
                Potential ties are indicated by new category 'tie'.")
      }

    }

    if (frtype_d %in% c("Gln-Glt-Cat-Num")) {
      d_agg <- summarizeData(d, opts$summarize$agg, to_agg = d, a, b, c) %>%
        drop_na(a, b)

      tooltips <- d_agg %>%
        mutate(d = makeup::makeup_num(d),
               html_row_unique = paste0("<strong>", nms[[3]], ":</strong> ", c, "<br/><strong>", nms[[4]], ":</strong> ", d),
               html_row = paste0("<strong>", c, ":</strong> ", d)) %>%
        group_by(a, b) %>%
        summarise(tooltip = ifelse(n()>1, paste0(html_row, collapse = "<br/>"), html_row_unique)) %>%
        mutate(tooltip = paste0("<span style='font-size:15px;'><strong>", nms[[1]], ":</strong> ",
                                paste0(opts$prefix,makeup::makeup_num(v = a,sample="1,500.00"), opts$suffix), "<br/>
                          <strong>", nms[[2]], ":</strong> ",
                                paste0(opts$prefix,makeup::makeup_num(v = b,sample="1,500.00"), opts$suffix), "<br/>",
                          tooltip, "</span>"))

      d <- d_agg %>%
        dplyr::group_by(a, b) %>%
        dplyr::filter(d == max(d)) %>%
        dplyr::mutate(e = n(), d = ifelse(e == 1, d, "tie")) %>%
        dplyr::distinct(a, b, c, d) %>%
        left_join(tooltips, by = c("a", "b"))

      if(nrow(d) < nrow(d_agg)){
        warning("Multiple categorical classes found for at least one geography.
                Geographies are colored based on majority class and all counts are displayed in tooltips.
                Potential ties are indicated by new category 'tie'.")
      }
    }

    if (frtype_d %in% c("Gln-Glt", "Glt-Gln", "Num-Num")) {
      d <- d %>% mutate(c = opts$extra$map_radius) %>% drop_na() }
    if (frtype_d %in% c("Gln-Glt-Num", "Glt-Gln-Num", "Num-Num-Num", "Gln-Glt-Num-Cat-Cat", "Num-Num-Num-Cat-Cat")) {
      d <- d %>% drop_na() }

    # define type of color scale
    if (frtype_d %in% c("Gnm-Cat", "Gcd-Cat", "Gln-Glt-Cat", "Gcd-Cat-Num", "Gnm-Cat-Num", "Gln-Glt-Cat-Num")){
      color_scale <- "Category"
    }

    cond_cat <- grep("Gnm|Gcd|Cat", d_frtype)
    if (identical(cond_cat, integer())) cond_cat <- 0
    if (cond_cat %in% 1) {

      centroides$name_alt <- iconv(tolower(centroides[[by_col]]), to = "ASCII//TRANSLIT")
      centroides <- centroides[,c("name_alt","lat", "lon")]

      topoInfo@data$name_alt <- iconv(tolower(topoInfo@data[[by_col]]), to = "ASCII//TRANSLIT")
      topoInfo@data <- left_join(topoInfo@data, centroides, by = "name_alt")

      d <- d %>%
        mutate(name_alt = iconv(tolower(a), to = "ASCII//TRANSLIT"))

      topoInfo@data  <- left_join(topoInfo@data, d, by = "name_alt")
      topoInfo@data$name <- makeup::makeup_chr(topoInfo@data[[by_col]], opts$style$format_cat_sample)

    } else {
      topoInfo@data <- d
      topoInfo@data$name <- opts$preprocess$na_label
    }
    topoInfo@data <- lflt_format(topoInfo@data, dic, nms, opts$style)

    if (frtype_d %in% c("Gcd-Cat-Num", "Gnm-Cat-Num","Gnm-Cat", "Gcd-Cat", "Gln-Glt-Cat-Num")) {
      topoInfo@data <- topoInfo@data %>%
        mutate(labels = ifelse(is.na(a), glue::glue("<span style='font-size:13px;'><strong>{name}</strong></span>") %>%
                                 lapply(htmltools::HTML), tooltip %>% lapply(htmltools::HTML))
        )
    } else {
      topoInfo@data <- topoInfo@data %>%
        mutate(labels = ifelse(is.na(a), glue::glue("<span style='font-size:13px;'><strong>{name}</strong></span>") %>%
                                 lapply(htmltools::HTML),
                               glue::glue(lflt_tooltip(nms, tooltip = opts$chart$tooltip)) %>% lapply(htmltools::HTML))
        )
    }
  }

  title <- tags$div(HTML(paste0("<div style='margin-bottom:0px;font-family:", opts$theme$text_family,
                                ';color:', opts$theme$title_color,
                                ';font-size:', opts$theme$title_size,"px;'>", opts$title$title %||% "","</div>")))
  subtitle <- tags$div(HTML(paste0("<p style='margin-top:0px;font-family:", opts$theme$text_family,
                                   ';color:', opts$theme$subtitle_color,
                                   ';font-size:', opts$theme$subtitle_size,"px;'>", opts$title$subtitle %||% "","</p>")))
  caption <- tags$div(HTML(paste0("<p style='font-family:", opts$theme$text_family,
                                  ';color:', opts$theme$caption_color,
                                  ';font-size:', opts$theme$caption_size,"px;'>", opts$title$caption %||% "","</p>")))
  legend_title <- HTML(paste0("<p style='font-family:", opts$theme$text_family,
                              ';color:', opts$theme$legend_color,
                              ';font-size:', opts$theme$legend_size,"px;'>", opts$title$legend_title %||% "","</p>"))

  if(color_scale == "Custom" & is.null(opts$extra$map_cutoff_points)){
    warning("No cutoff points specified for color scale 'Custom', overwriting color scale with default 'Numeric'.
            To use custom colorS scale, please define custoff points in parameter 'map_cutoff_points'.")
    color_scale <- "Numeric"
  }

  list(
    d = topoInfo,
    data = data,
    b_box = bbox,
    color_scale = color_scale,
    border_color = opts$theme$border_color,
    n_quantile = opts$extra$map_quantile,
    n_bins = opts$extra$map_bins,
    cutoff_points = opts$extra$map_cutoff_points,
    na_label = opts$preprocess$na_label,
    suffix = opts$style$suffix,
    prefix = opts$style$prefix,
    format_num = opts$style$format_num_sample,
    locale = opts$style$locale,
    titles = list(title = title,
                  subtitle = subtitle,
                  caption = caption),
    legend_title = legend_title,
    theme = opts$theme,
    min_size = opts$extra$map_min_size,
    max_size = opts$extra$map_max_size,
    bubble_opacity = opts$extra$bubble_opacity,
    map_stroke = opts$extra$map_stroke,
    graticule = list(map_graticule = opts$extra$map_graticule,
                     map_graticule_color = opts$theme$grid_color,
                     map_graticule_interval = opts$extra$map_graticule_interval,
                     map_graticule_weight = opts$theme$grid_size),
    min_zoom = opts$extra$map_min_zoom
    # USE IN POINT OR BUBBLES
  )
}

