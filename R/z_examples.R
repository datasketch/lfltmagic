# Choropleths: Gcd, Gcd-Cat, Gcd-Num
# PROBLEMA CON LABELS, POPUPS Y NA'S (QUEDA EL FORMAT O LO QUE SE PONGA EN PLANTILLA)
g0 <- sampleData("Gcd-Cat-Num", 123)

# Gcd
lflt_choropleth_Gcd()
lflt_choropleth_Gcd(g0[, 1])
lflt_choropleth_Gcd(g0[, 1], legend = list(bins = 6))
lflt_choropleth_Gcd(g0[, 1], count = FALSE)
# COLOR REVISAR
lflt_choropleth_Gcd(g0[, 1], fill = list(mode = "no", color = c("red", "yellow", "green"), opacity = 0.4),
                    marks = c(".", ","), nDigits = 6, percentage = TRUE, tiles = providers$OpenStreetMap.Mapnik,
                    format = c("DD ", " &UI"))

lflt_choropleth_Gcd(g0[, 1], fill = list(mode = "no", color = c("red", "yellow", "green"),
                                         scale = "continuous", nullColor = "orange"), marks = c(",", "."),
                    border = list(color = "blue", weight = 3),
                    nDigits = 6,
                    percentage = FALSE,
                    format = c("C| ", " @s"),
                    legend = list(position = "topleft", title = "JSJJDJF"),
                    mapName = "africa_countries")
# Gcd-Cat
lflt_choropleth_GcdCat()
lflt_choropleth_GcdCat(g0[, 1:2])
lflt_choropleth_GcdCat(g0[, 1:2], legend = list(bins = 6))
lflt_choropleth_GcdCat(g0[, 1:2], count = FALSE)
lflt_choropleth_GcdCat(g0[, 1:2], fill = list(mode = "no", color = c("red", "yellow", "green"), opacity = 0.4),
                       marks = c(".", ","), nDigits = 6, percentage = TRUE, tiles = providers$OpenStreetMap.Mapnik,
                       format = c("DD ", " &UI"))

lflt_choropleth_GcdCat(g0[, 1:2], fill = list(mode = "no", color = c("red", "yellow", "green"),
                                              scale = "continuous", nullColor = "orange"), marks = c(",", "."),
                       border = list(color = "blue", weight = 3),
                       nDigits = 6,
                       percentage = FALSE,
                       format = c("C| ", " @s"),
                       legend = list(position = "topleft", title = "JSJJDJF"),
                       mapName = "africa_countries")

# Gcd-Num
lflt_choropleth_GcdNum()
lflt_choropleth_GcdNum(g0[, c(1, 3)])
lflt_choropleth_GcdNum(g0[, c(1, 3)], fill = list(mode = "no", color = c("red", "yellow", "green")),
                       marks = c(".", ","), nDigits = 6, percentage = TRUE)

lflt_choropleth_GcdNum(g0[, c(1, 3)], fill = list(mode = "quantile", color = c("red", "yellow", "green"),
                                                  scale = "discrete"), marks = c(".", ","), nDigits = 6,
                       percentage = FALSE,
                       legend = list(position = "topleft", bins = 6, title = "JSJJDJF"),
                       mapName = "africa_countries", tiles = providers$OpenStreetMap.Mapnik)

# Bubbles: Gln-Glt, Gln-Glt-Cat, Gln-Glt-Num, Gln-Glt-Cat-Num
g1 <- sampleData("Gln-Glt-Cat-Num", 76)
# Gln-Glt
lflt_bubbles_GlnGlt(g1)
lflt_bubbles_GlnGlt(g1, fill = list(color = "green", opacity = 1))
lflt_bubbles_GlnGlt(g1, fill = list(color = c("green", "yellow"), opacity = 1,
                                    scale = "continuous"), count = FALSE,
                    border = list(stroke = FALSE))
lflt_bubbles_GlnGlt(g1, fill = list(scale = "continuous"), percentage = TRUE, size = c(20, 21),
                    format = c("oj ", "po"), border = list(color = "green", weight = 5))

# Gln-Glt-Cat
lflt_bubbles_GlnGltCat(g1)
lflt_bubbles_GlnGltCat(g1, fill = list(color = "green", opacity = 1))
lflt_bubbles_GlnGltCat(g1, fill = list(color = c("green", "yellow", "red"), opacity = 1,
                                       scale = "continuous"), count = FALSE,
                       border = list(stroke = FALSE))
lflt_bubbles_GlnGltCat(g1, fill = list(scale = "continuous"), percentage = TRUE, size = c(20, 21),
                       format = c("oj ", "po"), border = list(color = "green", weight = 5))

