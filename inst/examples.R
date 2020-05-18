library(devtools)
load_all()
document()
install()
library(lfltmagic)


data <- sampleData("Gnm-Num")
lflt_choropleth_GnmNum(data)

lflt_choropleth_GnmNum()
lflt_choropleth_GnmNum(border_color = "#000000",
                       border_weight = 2)


lflt_choropleth_GnmNum(data = sampleData("Gnm-Num", 100), percentage = TRUE)
lflt_choropleth_GnmNum(data = sampleData("Gnm-Num", 100),
                       percentage = TRUE,
                       legend_title = "Titulo de legenda")
lflt_choropleth_GnmNum(data = sampleData("Gnm-Num", 100),
                       map_graticule = TRUE)

data <- data.frame(Ciudad = c("Cauca", "chocó", "nariño"), Val = runif(3, 1, 1000))
lflt_choropleth_GnmNum(data, map_name = "col_pacifico")
lflt_choropleth_GnmNum(data, map_name = "col_pacifico",
                       tooltip = "<b>{Ciudad}:</b> {Val}", format_cat_sample = "Titulo")
lflt_choropleth_GnmNum(data, map_name = "col_pacifico",
                       title = "Pacífico Colombiano",
                       map_zoom = FALSE,
                       map_tiles = "OpenStreetMap")
lflt_choropleth_GnmNum(data, map_name = "col_pacifico", branding_include = TRUE)
lflt_choropleth_GnmNum(data,
                       map_name = "col_pacifico",
                       branding_include = TRUE,
                       map_tiles = "OpenStreetMap", tooltip = "hola {name}")
lflt_choropleth_GnmNum(data,
                       map_name = "col_pacifico",
                       branding_include = TRUE,
                       caption = "Esto es un footer",
                       caption_color = "red", title = "Titulo", map_zoom = F)


lflt_choropleth_GcdNum(sampleData("Gcd-Num", 300))
lflt_choropleth_GcdNum(sampleData("Gcd-Num", 300), topo_fill_opacity = 0.8)


lflt_bubbles_GnmNum(data, map_name = "col_pacifico")
lflt_bubbles_GlnGlt(data = sampleData("Gln-Glt"))
