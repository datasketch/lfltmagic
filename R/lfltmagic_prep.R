
#' @export
lfltmagic_prep <- function(data = NULL, opts = NULL, by_col = "name", ftype="Gnm-Num", ...) {

  map_name <- opts$extra$map_name
  label_by <- opts$extra$map_label_by

  topoInfo <- topo_info(map_name)
  lfmap <- geodataMeta(map_name)
  centroides <- data_centroid(lfmap$geoname, lfmap$basename)
  bbox <- topoInfo@bbox
  if (is.null(bbox)) bbox <- topo_bbox(centroides$lon, centroides$lat)
  color_scale <- opts$extra$map_color_scale

  if (is.null(data)) {
    topoInfo@data <- topoInfo@data %>%
      mutate(labels = glue::glue(paste0('<strong>{', label_by, '}</strong>')) %>% lapply(htmltools::HTML))
  } else {

    f <- homodatum::fringe(data)
    nms <- homodatum::fringe_labels(f)
    d <- homodatum::fringe_d(f)
    dic <- fringe_dic(f, id_letters = T)
    pre_ftype <- strsplit(ftype, "-") %>% unlist()

    if (length(dic$hdType) == 1) {
      dic$hdType <- pre_ftype[1]
    } else {
      dic$hdType[1:length(pre_ftype)] <- pre_ftype
    }


    frtype_d <- paste0(dic$hdType, collapse = "-")
    if (sum(grepl("Cat", dic$hdType))>1) color_scale <- "Category"

    if (grepl("Pct", frtype_d)) {
      dic$hdType[dic$hdType == "Pct"] <- "Num"
      frtype_d <- gsub("Pct", "Num", frtype_d)
    }

    if (sum(c("Glt", "Gln", "Gcd", "Gnm") %in% dic$hdType)==0) stop("Make sure you have a geographic variable in your data, it can be name or country codes, or latitude and longitude coordinates. Also make sure the map_name entered is correct.")



    if(frtype_d %in% c("Gcd", "Gnm")){
      d <- d %>%
        mutate(a = na_if(a, "-99")) %>%
        drop_na(a) %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(b = n())
      frtype_d <- paste0(frtype_d, "-Num")
      nms[2] <- opts$summarize$agg_text %||% "Count"
      names(nms) <- c("a", "b")
      dic_num <- data.frame(id = "count", label = "Count", hdType= as_hdType(x = "Num"), id_letters = "b")
      dic <- dic %>% bind_rows(dic_num)
    }


    if(frtype_d %in% c("Gcd-Cat", "Gnm-Cat")){
      d <- d %>%
        mutate(a = na_if(a, "-99")) %>%
        drop_na(a) %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(c = n())
      frtype_d <- paste0(frtype_d, "-Num")
      nms[3] <- opts$summarize$agg_text %||% "Count"
      names(nms) <- c("a", "b", "c")
      dic_num <- data.frame(id = "count", label = "Count", hdType= as_hdType(x = "Num"), id_letters = "c")
      dic <- dic %>% bind_rows(dic_num)
    }


    var_cats <- grep("Cat|Gcd|Gnm", dic$hdType)
    var_nums <- grep("Num|Glt|Gln", dic$hdType)

    # category, geocode, geoname formtat


    if (!identical(var_cats, integer())) {
      var_cats <- dic$id_letters[var_cats]
      l_cats <- map(var_cats, function(f_cats){
        d[[paste0(f_cats, "_label")]] <<- makeup_chr(d[[f_cats]], opts$style$format_sample_cat)
      })}


    # numeric format

    if (!identical(var_nums, integer())) {
      var_nums <- dic$id_letters[var_nums]

      l_nums <- map(var_nums, function(f_nums){
        d[[paste0(f_nums, "_label")]] <<- makeup_num(d[[f_nums]], sample = opts$style$format_sample_num)
      })}


    if (grepl("Gln|Glt", frtype_d)) {
      d <- d %>%
        mutate(labels = glue::glue(lflt_tooltip(nms, tooltip = opts$chart$tooltip)) %>% lapply(htmltools::HTML))
    }


    if (grepl("Gcd|Gnm", frtype_d)) {
      centroides$name_alt <- iconv(tolower(centroides[[by_col]]), to = "ASCII//TRANSLIT")

      centroides <- centroides[,c("name_alt","lat", "lon")]


      topoInfo@data$name_label <- makeup::makeup_chr(topoInfo@data$name, opts$style$format_sample_cat)
      topoInfo@data$id_label <- topoInfo@data$id

      if(grepl("\\{name\\}", opts$chart$tooltip)) {
        nm <- names(nms)
        nms <- c(nms, "name")
        names(nms) <- c(nm, "name")
      }

      topoInfo@data$name_alt <- iconv(tolower(topoInfo@data[[by_col]]), to = "ASCII//TRANSLIT")
      topoInfo@data <- left_join(topoInfo@data, centroides, by = "name_alt")

      d <- d %>%
        mutate(name_alt = iconv(tolower(a), to = "ASCII//TRANSLIT"))

      topoInfo@data  <- left_join(topoInfo@data, d, by = "name_alt")

      topoInfo@data$name <- makeup::makeup_chr(topoInfo@data[[by_col]], opts$style$format_cat_sample)




      topoInfo@data <- topoInfo@data %>%
        mutate(labels = ifelse(is.na(a),
                               glue::glue(paste0("<span style='font-size:13px;'><strong>{", label_by,"_label}</strong></span>")) %>% lapply(htmltools::HTML),
                               glue::glue(lflt_tooltip(nms, tooltip = opts$chart$tooltip)) %>% lapply(htmltools::HTML))
        )

    }


    data <- d
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


  list(
    d = topoInfo,
    data = data,
    b_box = bbox,
    color_scale = color_scale,
    titles = list(title = title,
                  subtitle = subtitle,
                  caption = caption),
    legend_title = legend_title,
    border_color = opts$theme$border_color,
    theme = opts$theme,
    min_size = opts$extra$map_min_size,
    max_size = opts$extra$map_max_size,
    bubble_opacity = opts$extra$bubble_opacity,
    map_stroke = opts$extra$map_stroke,
    graticule = list(map_graticule = opts$extra$map_graticule,
                     map_graticule_color = opts$theme$grid_color,
                     map_graticule_interval = opts$extra$map_graticule_interval,
                     map_graticule_weight = opts$theme$grid_size),
    n_quantile = opts$extra$map_quantile,
    n_bins = opts$extra$map_bins,
    cutoff_points = opts$extra$map_cutoff_points,
    na_label = opts$preprocess$na_label,
    suffix = opts$style$suffix,
    prefix = opts$style$prefix,
    format_num = opts$style$format_sample_num,
    locale = opts$style$locale,
    min_zoom = opts$extra$map_min_zoom
  )


}

