#' @importFrom dplyr %>%
#' @export
lfltmagic_prep <- function(data = NULL, opts = NULL, by_col = "name", ftype="Gnm-Num", ...) {
  # call geographical info
  shape <- dsvizprep::shape_info(map_name = opts$extra$map_name,
                                 ftype = ftype,
                                 by_col = by_col,
                                 addRds = FALSE)
  #topoData <- shape$rdsInfo
  topoInfo <- shape$topoInfo
  topoInfo$labels <- topoInfo[[by_col]]

  # data preparation by type
  if (!is.null(data)) {

    list_d <- dsvizprep::data_map_prep(data = data,
                                       ftype = ftype,
                                       agg =  opts$summarize$agg,
                                       color_by = opts$style$color_by,
                                       ptage = opts$postprocess$percentage,
                                       more_levels = shape$more_levels,
                                       ptage_col = opts$postprocess$percentage_col)

    # format setting of data being displayed
    data_format <- dsvizprep::format_prep(data = list_d$data,
                                          dic = list_d$dic,
                                          formats = list(sample_num = opts$style$format_sample_num,
                                                         sample_cat = opts$style$format_sample_cat,
                                                         prefix = opts$style$prefix,
                                                         suffix = opts$style$suffix))


    if (grepl("Gnm|Gcd", ftype)) {
      data_format$name_alt <- tolower(stringi::stri_trans_general(str = data_format$a, id = "Latin-ASCII"))
      topoInfo <- topoInfo %>% dplyr::left_join(data_format, by = "name_alt")
      # add info tooltip in data
      if (length(list_d$nms_tooltip) > 1) {
        if (opts$postprocess$percentage)  list_d$nms_tooltip[length(list_d$nms_tooltip)] <- "%"
      }
      topoInfo <- agg_tooltip(data = topoInfo, label_by = opts$extra$map_label_by,nms = list_d$nms, label_ftype = list_d$nms_tooltip, tooltip = opts$chart$tooltip)
    } else {
      topoInfo <- list(topoInfo = topoInfo, data = data_format)
      # add info tooltip in data
      topoInfo$data <- agg_tooltip(data = topoInfo$data, label_by = opts$extra$map_label_by,nms = list_d$nms, label_ftype = list_d$nms_tooltip, tooltip = opts$chart$tooltip)
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
                              ';color:', opts$theme$legend_color %||% "#ffffff",
                              ';font-size:', opts$theme$legend_size,"px;'>", opts$title$legend_title %||% "","</p>"))

  color_scale <- opts$extra$map_color_scale
  palette_type <-  opts$theme$palette_type %||% "sequential"
  palette_colors <-  opts$theme$palette_colors %||% opts$theme[[paste0("palette_colors_", palette_type)]]

  list(
    topoInfo = topoInfo,
    #geoInfo = topoData,
    more_levels = shape$more_levels,
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
    map_opacity = opts$extra$map_opacity,
    min_zoom = opts$extra$map_min_zoom,
    max_zoom = opts$extra$map_max_zoom,
    map_zoom_snap = opts$extra$map_zoom_snap,
    map_zoom_delta = opts$extra$map_zoom_delta,
    map_name_extra = opts$extra$map_name_extra,
    map_extra_layer = opts$extra$map_extra_layer,
    map_extra_weight = opts$extra$map_extra_weight,
    map_extra_opacity = opts$extra$map_extra_opacity,
    map_extra_fillColor = opts$extra$map_extra_fillColor,
    map_extra_color = opts$extra$map_extra_color

  )

}
