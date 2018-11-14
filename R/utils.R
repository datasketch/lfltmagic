
sort_list <- function(l){
  l[sort(names(l))]
}


discard_all_na_cols <- function (d){
  f <- function(x) !all(is.na(x))
  d %>% keep(f)
}

remove_accents <- function(string){
  accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"
  translation <- "aeiouAEIOUaeiouyAEIOUYaeiouAEIOUaeiouAEIOUnNc"
  chartr(accents, translation, string)
}

match_replace <- function(v,dic, force = TRUE){
  matches <- dic[[2]][match(v,dic[[1]])]
  out <- matches
  if(!force)
    out[is.na(matches)] <- v[is.na(matches)]
  out
}

str_tpl_format <- function(tpl, l){
  if("list" %in% class(l)){
    listToNameValue <- function(l){
      mapply(function(i,j) list(name = j, value = i), l, names(l), SIMPLIFY = FALSE)
    }
    f <- function(tpl,l){
      gsub(paste0("{",l$name,"}"), l$value, tpl, fixed = TRUE)
    }
    return( Reduce(f,listToNameValue(l), init = tpl))
  }
  if("data.frame" %in% class(l)){
    myTranspose <- function(x) lapply(1:nrow(x), function(i) lapply(l, "[[", i))
    return( unlist(lapply(myTranspose(l), function(l, tpl) str_tpl_format(tpl, l), tpl = tpl)) )
  }
}

`%||%` <- function (x, y)
{
  if (is.empty(x))
    return(y)
  else if (is.null(x) || is.na(x))
    return(y)
  else if (class(x) == "character" && nchar(x) == 0)
    return(y)
  else x
}

is.empty <- function (x)
{
  !as.logical(length(x))
}

file_path_sans_ext <- function (x)
{
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}




# leaflet labelFormat with decimal.mark
#' @export
labFor <- function (prefix = "",
                    suffix = "",
                    between = " &ndash; ",
                    digits = 3,
                    big.mark = ",",
                    decimal.mark = ".",
                    transform = identity) {
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE,
           big.mark = big.mark, decimal.mark = decimal.mark)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      paste0(prefix, formatNum(cuts), suffix)
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]),
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n],
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}

# aggregation
#'@export
agg <- function(aggregation,...){
  f <- NULL
  if(aggregation == "sum")
    f <- sum(..., na.rm = TRUE)
  if(aggregation == "mean")
    f <- mean(..., na.rm = TRUE)
  if(aggregation == "median")
    f <- median(...,na.rm = TRUE)
  f
}


# colores
#' @export
fillColors <- function (data, col, colors, colorScale, highlightValue, highlightValueColor,
                        labelWrap, bins = NULL, numeric = TRUE) {
  cat <- unique(data[[col]])
  highlightValue <- stringr::str_wrap(highlightValue, labelWrap)
  ds <- dsColorsHex(TRUE)
  if (!is.null(colors)) {
    cl <- col2rgb(colors)
    colors <- map_chr(1:ncol(cl), function(s) {
      rgb(cl[1, s], cl[2, s], cl[3, s], maxColorValue = 255)
    })
  }
  if (colorScale == "no") {
    if (is.null(colors)) {
      colors <- dsColorsHex()[2]
    }
    fillCol <- data.frame(a = cat,
                          color = rep(colors, length(cat))[1:length(cat)])
    names(fillCol)[1] <- col
    # fillCol <- rep(colors, length(cat))[1:length(cat)]
    # names(fillCol) <- cat
  }
  if (colorScale == "discrete") {
    if (is.null(colors)) {
      colors <- dsColorsHex()
    }
    ad <- unlist(map(colors, function(y) {
      l0 <- ds[((grep(substr(y, 2, 2), ignore.case = TRUE, ds) + 6) %% 16) + 1]
      l1 <- paste0(substr(y, 1, 1), l0, substr(y, 3, 7))
      p0 <- ds[((grep(substr(l1, 4, 4), ignore.case = TRUE, ds) + 6) %% 16) + 1]
      p1 <- paste0(substr(l1, 1, 3), p0, substr(l1, 5, 7))
      # p0 <- ds[((grep(substr(ifelse(length(colors) > 1, y, l1), 4, 4), ignore.case = TRUE, ds) + 6) %% 16) + 1]
      # p1 <- paste0(substr(ifelse(length(colors) > 1, y,  l1), 1, 3),
      #              p0,
      #              substr(ifelse(length(colors) > 1, y, l1), 5, 7))
      c(l1, p1)
    }))
  }
  if (colorScale == "continuous") {
    if (is.null(colors)) {
      colors <- dsColorsHex()[c(1, 7, 3, 4)]
    }
    if (length(colors) == 1) {
      l0 <- ds[((grep(substr(colors, 2, 2), ignore.case = TRUE, ds) + 7) %% 16) + 1]
      colors <- c(colors, paste0(substr(colors, 1, 1), l0, substr(colors, 3, 7)))
    }
  }
  if (numeric) {
    # fillCol <- data.frame(a = cat,
    #                       color = c(colors, colorNumeric(ad, cat)(cat))[sample(1:length(cat))])
    fillCol <- data.frame(a = cat,
                          color = c(colors, colorBin(c(colors, ad), cat, bins = bins)(cat))[sample(1:length(cat))])
    names(fillCol)[1] <- col
  } else {
    # fillCol <- data.frame(a = cat,
    #                       color = c(colors, colorFactor(ad, cat)(cat))[sample(1:length(cat))])
    fillCol <- data.frame(a = cat,
                          color = c(colors, colorFactor(c(colors, ad), cat)(cat))[sample(1:length(cat))])
    names(fillCol)[1] <- col
  }
  if (!is.null(highlightValue) & sum(highlightValue %in% data[[1]]) > 0) {
    wh <- which(data[[1]] %in% highlightValue)
    if (is.null(highlightValueColor)) {
      l0 <- ds[((grep(substr(colors[1], 2, 2), ignore.case = TRUE, ds) + 13) %% 16) + 1]
      highlightValueColor <- paste0(substr(colors[1], 1, 1), l0, substr(colors[1], 3, 7))
    }
    print(wh)
    fillCol[[2]][wh] <- highlightValueColor
  }
  fillCol <- data %>%
    dplyr::left_join(fillCol)
  fillCol
}

# ds palette
#' @export
dsColorsHex <- function(hex = FALSE) {
  if (hex) {
    c <- c(0:9, "A", "B", "C", "D", "E", "F")

  } else {
    c <- c("#2E0F35", "#74D1F7", "#B70F7F", "#C2C4C4", "#8097A4",  "#A6CEDE", "#801549",
           "#FECA84", "#ACD9C2", "#EEF1F2")
  }
  c
}


