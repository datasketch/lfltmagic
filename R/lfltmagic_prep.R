
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

    # data <- data.frame(lat=c(-74), lon = c(-3))
    #data <- sample_data("Gnm-Cat-Num-Num-Gcd", 1000)
    map_name <- "world_countries"
    f <- homodatum::fringe(data)
    nms <- homodatum::fringe_labels(f)
    d <- homodatum::fringe_d(f)
    dic <- guess_coords(data, map_name = map_name) #un voto de fe al "adivinador"
    frtype_d <- paste0(dic$hdType, collapse = "-")


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
      print(var_nums)
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

      topoInfo@data$name_alt <- iconv(tolower(topoInfo@data[[by_col]]), to = "ASCII//TRANSLIT")
      topoInfo@data <- left_join(topoInfo@data, centroides, by = "name_alt")

      d <- d %>%
        mutate(name_alt = iconv(tolower(a), to = "ASCII//TRANSLIT"))

      topoInfo@data  <- left_join(topoInfo@data, d, by = "name_alt")
      topoInfo@data$name <- makeup::makeup_chr(topoInfo@data[[by_col]], opts$style$format_cat_sample)

      topoInfo@data <- topoInfo@data %>%
        mutate(labels = ifelse(is.na(a),
                               glue::glue("<span style='font-size:13px;'><strong>{name}</strong></span>") %>% lapply(htmltools::HTML),
                               glue::glue(lflt_tooltip(nms, tooltip = opts$chart$tooltip)) %>% lapply(htmltools::HTML))
        )

    }







    print(nms)
    data <- d
  }

  list(
    d = topoInfo,
    data = data,
    b_box = bbox
  )


}

