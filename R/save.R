#' save
#' @export
save_lfltmagic <- function(viz,
                           filename,
                           format = NULL,
                           width = 660,
                           height = 500, ...) {
  format <- file_ext(filename) %||% "png"
  tmp <- paste(tempdir(), 'html', sep ='.')
  htmlwidgets::saveWidget(viz, tmp)
  tmpSave <- filename
  if (format == 'html') {
    htmlwidgets::saveWidget(viz, filename)
  } else {
    webshot::webshot(tmp, filename, vwidth = width, vheight = height)
  }
  file.copy(filename, filename)
}

