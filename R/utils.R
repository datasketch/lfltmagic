
sort_list <- function(l){
  l[sort(names(l))]
}


discard_all_na_cols <- function (d){
  f <- function(x) !all(is.na(x))
  d %>% keep(f)
}

remove_accents <- function(string){
  iconv(string, from="UTF-8", to="ASCII//TRANSLIT")
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
labelFormat0 <- function (prefix = "",
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
      paste0(prefix, formatNum(cuts[-n]), suffix, between, prefix, formatNum(cuts[-1]),
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n], suffix,
             between, prefix, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}

# aggregation
#'@export
agg <- function (aggregation, ...) {
  if (!is.null(aggregation) | nchar(aggregation) > 0 | !is.na(aggregation)) {
    do.call(aggregation, list(..., na.rm = TRUE))
  }
}

# colores
#' @export
fillColors <- function (data, col, colors, colorScale, highlightValue, highlightValueColor,
                        labelWrap, numeric = TRUE) {
  cat <- unique(data[[col]])
  highlightValue <- stringr::str_wrap(highlightValue, labelWrap)
  ds <- dsColorsHex(TRUE)
  ad <- c()
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
    fillCol <- data.frame(a = cat,
                          color = union(colors, colorNumeric(c(colors, ad), cat)(cat))[sample(1:length(cat))])
    names(fillCol)[1] <- col
  } else {
    fillCol <- data.frame(a = cat,
                          color = union(colors, colorFactor(c(colors, ad), cat)(cat))[sample(1:length(cat))])
    names(fillCol)[1] <- col
  }

  fillCol <- data %>%
    dplyr::left_join(fillCol)

  if (!is.null(highlightValue) & sum(highlightValue %in% fillCol[[1]]) > 0) {
    wh <- which(fillCol[[1]] %in% highlightValue)
    if (is.null(highlightValueColor)) {
      l0 <- ds[((grep(substr(colors[1], 2, 2), ignore.case = TRUE, ds) + 13) %% 16) + 1]
      highlightValueColor <- paste0(substr(colors[1], 1, 1), l0, substr(colors[1], 3, 7))
    }
    fillCol$color <- as.character(fillCol$color)
    fillCol$color[wh] <- highlightValueColor
  }
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

# label popups
#' @export
labelPopup <- function(data, lp, marks = c(".", "."), nDigits = 2, labelWrap = 12) {
  if (is.null(lp)) lp <- ""
  m0 <- regmatches(lp, gregexpr("\\{.[^\\{]+}", lp))[[1]]
  m1 <- gsub("\\{", "\\\\{", gsub("\\.", "\\\\.", m0))
  n0 <- gsub("\\.", "", unlist(regmatches(m0, gregexpr("\\.[^}]+", m0))))
  cl <- rep(lp, nrow(data))
  map(1:length(n0), function(p) {
    st0 <- strsplit(cl, m1[p])
    st1 <- map(st0, ~`[`(.x, 1))
    st2 <- map(st0, ~`[`(.x, 2))
    md <- data[[n0[p]]]

    if (!nzchar(st1[[1]]) | is.na(st1[[1]])) {
      st1 <- ""
    }
    if (!nzchar(st2[[1]]) | is.na(st2[[1]])) {
      st2 <- ""
    }
    if (is.null(md)) {
      md <- ""
    }
    cl <<- paste0(st1,
                  stringr::str_wrap(format(md,
                                           big.mark = marks[1],
                                           decimal.mark = marks[2],
                                           nsmall = nDigits,
                                           digits = nDigits),
                                    labelWrap),
                  st2)

  })
  cl
}




# fillcolors choropleth returns pal
#' @export
fillColorsChoropleth <- function(data, col, color, colorScale, bins, mode, numeric, nullColor) {
  cat <- unique(data[[col]])
  ds <- dsColorsHex(TRUE)
  if (is.null(color)) {
    color <- dsColorsHex()
  } else {
    cl <- col2rgb(color)
    color <- map_chr(1:ncol(cl), function(s) {
      rgb(cl[1, s], cl[2, s], cl[3, s], maxColorValue = 255)
    })
  }
  if (colorScale == "discrete") {
    ad <- unlist(map(color, function(y) {
      l0 <- ds[((grep(substr(y, 2, 2), ignore.case = TRUE, ds) + 6) %% 16) + 1]
      l1 <- paste0(substr(y, 1, 1), l0, substr(y, 3, 7))
      p0 <- ds[((grep(substr(l1, 4, 4), ignore.case = TRUE, ds) + 6) %% 16) + 1]
      p1 <- paste0(substr(l1, 1, 3), p0, substr(l1, 5, 7))
      c(l1, p1)
    }))
    color <- union(color, ad)
  }
  if (colorScale == "continuous") {
    if (length(color) == 1) {
      l0 <- ds[((grep(substr(color, 2, 2), ignore.case = TRUE, ds) + 7) %% 16) + 1]
      color <- c(color, paste0(substr(color, 1, 1), l0, substr(color, 3, 7)))
    }
    color <- unique(color)
  }
  # YA TENGO DATOS, COLUMNA, COLOR
  if (numeric) {
    ca <- unique(cat)
    ct <- as.numeric(ca[!is.na(ca)])
    if (length(ct) == 1) {
      ct <- ct + 1
    }
    if (colorScale == "discrete") {
      if (mode %in% "quantile") {
        pal <- colorBin(color, union(ca, ct), quantile(union(ca, ct), seq(0, 1, 1/bins), na.rm = TRUE), na.color = nullColor)
      } else {
        pal <- colorBin(color, union(ca, ct), bins, na.color = nullColor)
      }
    } else {
      pal <- colorNumeric(color, unique(ca, ct), na.color = nullColor)
    }
  } else {
    pal <- colorFactor(color, cat, na.color = nullColor)
  }
  pal
}

# discrete color
#' @export
discreteColor <- function (colorDefault, d) {
  lengData <- length(unique(d$a))
  lengColor <- length(colorDefault)
  if (lengData == lengColor) {
    colorDefault <- colorDefault
  } else if (lengData > lengColor) {
    colorDefault <- c(colorDefault, sample(colorDefault, lengData-lengColor))
  } else {
    colorDefault <- colorDefault[1:lengData]
  }
  colorDefault
}

# layerMap and dataMap
#' @export
layerMap <- function(mapName, borderColor, borderWeigth, fillColor, fillOpacity ) {
  lfmap <- geodataMeta(mapName)
  lfmap$path <- file.path("geodata",lfmap$geoname,paste0(lfmap$basename,".topojson"))
  lfmap$centroides <- file.path("geodata",lfmap$geoname,paste0(lfmap$basename,".csv"))
  centroides <- read_csv(system.file(lfmap$centroides,package = "geodata"))

  tj <- readLines(system.file(lfmap$path, package = "geodata")) %>% paste(collapse = "\n")

  mapLf <- leaflet()%>%
            addTiles() %>%
             addTopoJSON(tj,
                         weight = borderWeigth,
                         color = borderColor,
                         fillColor = fillColor,
                         fillOpacity = fillOpacity)

  result <- list(centroides, mapLf)
  result

}




#legend for categories
#' export
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.8, position ="bottomleft", title = " "){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, position = position, title = title))
}









