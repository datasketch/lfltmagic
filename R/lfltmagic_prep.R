
#' @export
lfltmagic_prep <- function(data = NULL, opts = NULL, by_col = "name", ftype="Gnm-Num", ...) {
  map_name <- opts$extra$map_name
  label_by <- opts$extra$map_label_by

  path_rds_centroides <- geodata::geodataRdsPath(mapName = map_name)
  topoData <- read_rds(gsub("-centroides", "", path_rds_centroides))
  centroides <- read_rds(suppressWarnings(path_rds_centroides))
  nms_centroides <- names(centroides)
  aditional_name <- setdiff(nms_centroides, c("id", "name", "lat", "lon"))
  centroides_join <- centroides[c("id", "lat", "lon")]
  topoInfo <- suppressWarnings(topo_info(map_name))

  topoInfo <- topoInfo %>%
                dplyr::left_join(centroides_join, by =  "id") %>%
                 mutate(id = as.character(id))

  topoInfo_names <- names(topoInfo)



  if (grepl("Gnm", ftype) & !identical(aditional_name, character())) {
    topoInfo$name_alt <- paste0(topoInfo$name, " - ", topoInfo[[aditional_name]])
  } else {
    topoInfo$name_alt <- as.character(topoInfo[[by_col]])
  }

  topoInfo$name_alt <- iconv(tolower(topoInfo$name_alt), to = "ASCII//TRANSLIT")

  color_scale <- opts$extra$map_color_scale
  palette_colors <-  opts$theme$palette_colors
  palette_type <-  opts$theme$palette_type

  d <- NULL

  if (is.null(data)) {
    topoInfo <- topoInfo %>%
      mutate(labels = glue::glue(paste0('<strong>{', label_by, '}</strong>')) %>% lapply(htmltools::HTML))
  } else {
    data <- data %>% drop_na()
    f <- homodatum::fringe(data)
    nms <- homodatum::fringe_labels(f)
    d <- homodatum::fringe_d(f)
    dic <- fringe_dic(f, id_letters = T)
    pre_ftype <- strsplit(ftype, "-") %>% unlist()
    dic$hdType[dic$hdType == "Pct"] <- "Num"
    default_tooltip <- names(d)
    # searches the centroids for matches of the name or code belonging to the information geographic of map_name
    if (grepl("Gcd", ftype)) {
      have_geocode <- find_geoinfo(d, centroides)
      if (is.null(have_geocode)) stop("make sure your data has geographic information")
    }

    if (grepl("Gnm", ftype) & (!identical(aditional_name, character()))) {
      d$a <- paste0(d$a, " - ", d$b)
      d <- d %>% select(-b)
      dic <- dic %>%  filter(id_letters != "b")
      dic$id_letters[dic$id_letters == "c"] <- "b"
      d <- d %>% rename(c("b" = "c"))
      nms <- nms[c(1,3)]
      names(nms) <- c("a", "b")
    }


    nms[length(nms)+1] <- c("%")
    names(nms) <- c(names(nms)[-length(nms)], "..percentage")
    nms[length(nms)+1] <- c("Count")
    names(nms) <- c(names(nms)[-length(nms)], "..count")
    dic$id <- names(d)


    # separarte only data plot ------------------------------------------------

    ftype_vec <- stringr::str_split(ftype, pattern = "-") %>% unlist()
    ftype_length <- length(ftype_vec)
    dd <- d[,1:ftype_length]
    dic_p <- dic %>% dplyr::filter(id %in% names(dd))


    # agregation all numeric variables and collapse categorica variabl --------


    var_g <- NULL ## categorical ..groups
    dic_agg <- NULL


    if (any(c("Cat", "Gcd", "Gnm") %in% pre_ftype)) {
      dic_agg <- dic_p %>% dplyr::filter(hdType %in% c("Cat", "Gcd", "Gnm"))
      var_g <- unique(dic_agg$id)
    }


    # agregation all numeric variables and collapse categorica variabl --------

    var_nums <- grep("Num", dic$hdType)
    agg_num <- NULL
    if (!identical(var_nums, integer())) agg_num <- dic$id[var_nums]
    func_paste <- function(x) paste(unique(x), collapse = '. ')
    var_cats <- grep("Cat|Gnm|Gcd", dic$hdType)
    agg_cats <- NULL
    if (!identical(var_cats, integer())) agg_cats <- dic$id[var_cats]
    dd <- NULL

    agg_var <- "..count"
    has_num_var <- "Num" %in% dic_p$hdType

    if (has_num_var &  sum(grepl("Num",  ftype_vec)) >= 1) {
      agg_var <- opts$postprocess$percentage_col %||% "b"
    }


    if (!is.null(var_g)) {
      if (length(grep("Cat|Gnm|Gcd", ftype_vec)) == 1) {
        if (has_num_var & sum(grepl("Num",  ftype_vec)) == 1)  {
          agg_var <- "b"
        }
      } else {
        if (has_num_var) {
          agg_var <- "c"
        }
      }
    }
    dic_alt <- dic

    if (agg_var == "..count") {
      dic_p <- dic_p %>% dplyr::bind_rows(dplyr::bind_rows(data.frame(id = "..count", label = "Count", hdType = "Num", id_letters = "..count") %>%
                                                             dplyr::mutate_all(as.character)))
    } else {
      dic_p <- dic_p
    }

    if (opts$postprocess$percentage) {
      dic_p <- dic_p %>% dplyr::filter(id != agg_var)
      dic_p <- dic_p %>% dplyr::bind_rows(dplyr::bind_rows(data.frame(id = "..percentage", label = "%", hdType = "Num", id_letters = "..percentage") %>%
                                                             dplyr::mutate_all(as.character)))
    }

    #print(var_g)
    if (!is.null(var_g)) {
      dn <- d
      if (!is.null(agg_num))  dn <- d[,-var_nums]

      n_cats <- length(var_g)

      if (grepl("Gln-Glt-Cat", ftype)) n_cats <- 3

      if (n_cats == 1) {
        dd <- function_agg(df = d, agg = opts$summarize$agg, to_agg = agg_num, a)

        dd <- dsvizopts::preprocessData(dd, drop_na = opts$preprocess$drop_na,
                                        na_label = opts$preprocess$na_label, na_label_cols = "a")

        dd <- dsvizopts::postprocess(dd, agg_var, sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)

        dd$..percentage <- (dd[[agg_var]]/sum(dd[[agg_var]], na.rm = TRUE)) * 100
        dn <- dn %>%
          dplyr::group_by(a) %>%
          dplyr::summarise_all(.funs = func_paste)
      } else if (n_cats == 2) {

        dd <- function_agg(df = d, agg = opts$summarize$agg, to_agg = agg_num, a, b)

        by_col <- opts$postprocess$percentage_col
        if (is.null(by_col)) {
          by_col <- "a"
        } else {
          by_col <- names(nms[match(by_col, nms)])
        }
        agg_var_t <- rlang::sym(agg_var)
        dd <- dd %>%
          dplyr::group_by_(by_col) %>%
          dplyr::mutate(..percentage = (!!agg_var_t/sum(!!agg_var_t, na.rm = TRUE))*100)

        dd <- dd %>% drop_na(a)
        dd <- dsvizopts::preprocessData(dd, drop_na = opts$preprocess$drop_na,
                                        na_label = opts$preprocess$na_label, na_label_cols = "b")


        dd <- dsvizopts::postprocess(dd, agg_var, sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)

        dn$a[is.na(dn$a)] <- opts$preprocess$na_label
        dn$b[is.na(dn$b)] <- opts$preprocess$na_label


        dn <- dn %>%
          dplyr::group_by(a, b) %>%
          dplyr::summarise_each(dplyr::funs(func_paste))
        dd$b <- as.character(dd$b)
        dn$b <- as.character(dn$b)
      } else {
        var_g <- c("a", "b", "c")
        dd <- function_agg(df = d, agg = opts$summarize$agg, to_agg = agg_num, a, b, c)

        by_col <- "c"

        agg_var_t <- rlang::sym(agg_var)
        dd <- dd %>%
          dplyr::group_by_(by_col) %>%
          dplyr::mutate(..percentage = (!!agg_var_t/sum(!!agg_var_t, na.rm = TRUE))*100)

      }

      if (!is.null(agg_num)) {
        dd$..domain <- dd[[agg_num]]
      } else {
        dd$..domain <- dd$..count
      }

      dic_alt <- dic_alt %>%
        dplyr::bind_rows(data.frame(id = c("..count", "..percentage"), label = c("Count", "%"), hdType = c("Num", "Num")) %>%
                           dplyr::mutate_all(as.character))

      d <- dd %>% dplyr::left_join(dn, by = var_g)
    }
    print(dic_p)
    #default_tooltip <- dic_p$id
    ####################################

    if (ftype == "Gcd" | ftype == "Gnm") {
      d$..domain <- d$..count
      d <- d %>% select(a, ..domain, everything())
    }

    if (ftype == "Gcd-Cat" | ftype == "Gnm-Cat") {
      d$..domain <- as.numeric(as.factor(d$b))
      d <- d %>% select(a, b, ..domain, everything())
    }

    # categoric format
    if (!identical(var_cats, integer())) {
      var_cats <- dic$id_letters[var_cats]
      l_cats <- map(var_cats, function(f_cats){
        d[[paste0(f_cats, "_label")]] <<- makeup_chr(d[[f_cats]], opts$style$format_sample_cat)
      })}

    # numeric format

    var_nums <- grep("Num|Glt|Gln", dic_alt$hdType)


    if (!identical(var_nums, integer())) {
      var_nums <- dic_alt$id[var_nums]

      l_nums <- purrr::map(var_nums, function(f_nums){
        d[[paste0(f_nums, "_label")]] <<- makeup::makeup_num(d[[f_nums]], sample = opts$style$format_sample_num)
      })}

    if (grepl("Gln|Glt", ftype)) {
      topoInfo$labels <- topoInfo[[label_by]]
      #default_tooltip <- names(d)
      d$..domain <- 1

      if (grepl("Num|Cat", ftype)) {
        d$..domain <- d$c
        #default_tooltip <- names(d)
      }


      d <- d %>%
        mutate(labels = glue::glue(lflt_tooltip(nms, label_ftype = default_tooltip, tooltip = opts$chart$tooltip)) %>% lapply(htmltools::HTML))
    }

    if (grepl("Gnm|Gcd", ftype)) {
      topoInfo$name_label <- makeup::makeup_chr(topoInfo$name, opts$style$format_sample_cat)
      topoInfo$id_label <- topoInfo$id

      if(grepl("\\{name\\}", opts$chart$tooltip)) {
        nm <- names(nms)
        nms <- c(nms, "name")
        names(nms) <- c(nm, "name")
      }

      d$name_alt <- iconv(tolower(d$a), to = "ASCII//TRANSLIT")

      topoInfo <- topoInfo %>% dplyr::left_join(d, by = "name_alt")
      topoInfo <- topoInfo %>%
        mutate(labels = ifelse(is.na(a),
                               glue::glue(paste0("<span style='font-size:13px;'><strong>{", label_by,"_label}</strong></span>")) %>% lapply(htmltools::HTML),
                               glue::glue(lflt_tooltip(nms, label_ftype = default_tooltip, tooltip = opts$chart$tooltip)) %>% lapply(htmltools::HTML))
        )
      # print(topoInfo)
    }
    # define color palette based on data type
    var_cat <- "Cat" %in% dic$hdType
    if(!is.null(palette_type)){
      if(!palette_type %in% c("categorical", "sequential", "divergent")){
        warning("Palette type must be one of 'categorical', 'sequential', or 'divergent'; reverting to default.")
        palette_type <- NULL
      }
      if(!var_cat & palette_type == "categorical" | (var_cat & palette_type %in% c("sequential", "divergent"))){
        warning("Palette type might not be suitable for data type.")
      }
    } else {
      if(var_cat){
        palette_type <- "categorical"
      } else {
        palette_type <- "sequential"
      }

      if (ftype == "Gcd-Cat" | ftype == "Gnm-Cat") {
        palette_type <- "categorical"
      }
    }


    if(is.null(palette_colors)){
      palette_colors <- opts$theme[[paste0("palette_colors_", palette_type)]]
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


  list(
    topoInfo = topoInfo,
    data = d,
    geoInfo = topoData,
    color_scale = color_scale,
    palette_colors = palette_colors,
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

