
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
    #data <- sample_data("Cat-Cat-Num-Num", 1000)
    map_name <- "world_countries"
    f <- homodatum::fringe(data)
    nms <- homodatum::fringe_labels(f)
    d <- homodatum::fringe_d(f)
    dic <- guess_coords(data, map_name = map_name) #un voto de fe al "adivinador"
    frtype_d <- paste0(dic$hdType, collapse = "-")
    var_cats <- grep("Cat|Gcd|Gnm", dic$hdType)
    var_nums <- grep("Num|Glt|Gln", dic$hdType)

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
    }


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










    print(nms)
    data <- d
  }

  list(
    d = topoInfo,
    data = data,
    b_box = bbox
  )


}

