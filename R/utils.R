
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

# percent column
#' @export
percentColumn <- function (data, col, percentage = TRUE, nDigits = 2) {
  if (percentage) {
    data[[col]] <- round((data[[col]] * 100)/sum(data[[col]],
                                                 na.rm = TRUE), digits = nDigits)
  }
  else {
    data[[col]] <- round(data[[col]], digits = nDigits)
  }
  data
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


