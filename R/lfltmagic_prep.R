
#' @export
lfltmagic_prep <- function(data = NULL, opts = NULL, by_col = "name", ftype="Gnm-Num", ...) {
  # call geographical info
  shape <- dsvizopts::shape_info(map_name = opts$extra$map_name,
                                 ftype = ftype,
                                 by_col = by_col,
                                 addRds = TRUE)
  topoData <- shape$rdsInfo
  topoInfo <- shape$topoInfo


  # data preparation by type
  if (!is.null(data)) {
    list_d <- dsvizopts::data_map_prep(data = data,
                                       ftype = ftype,
                                       agg =  opts$summarize$agg,
                                       more_levels = shape$more_levels,
                                       ptage_col = opts$postprocess$percentage_col)
    # format setting of data being displayed
    data_format <- dsvizopts::format_prep(data = list_d$data,
                                          dic = list_d$dic,
                                          formats = list(sample_num = opts$style$format_sample_num,
                                                         sample_cat = opts$style$format_sample_cat))
    # add info tooltip in data
    data_format <- data_format %>%
      dplyr::mutate(labels = ifelse(is.na(a),
                                    glue::glue(paste0("<span style='font-size:13px;'><strong>{", label_by,"_label}</strong></span>")) %>% lapply(htmltools::HTML),
                                    glue::glue(
                                      lflt_tooltip(nms = list_d$nms,
                                                   label_ftype = list_d$nms_tooltip,
                                                   tooltip = opts$chart$tooltip)) %>%
                                      lapply(htmltools::HTML))
      )
  }

}
