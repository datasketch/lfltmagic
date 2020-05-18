library(lfltmagic)


# Gnm-Num examples --------------------------------------------------------

lflt_choropleth_GnmNum()
lflt_choropleth_GnmNum(map_tiles = "OpenStreetMap")
lflt_choropleth_GnmNum(map_name = "col_departments",
                       map_graticule = TRUE,
                       map_graticule_interval = 3,
                       map_graticule_color = "#000")
lflt_choropleth_GnmNum(map_name = "col_departments",
                       map_tiles = "OpenStreetMap"
)

lflt_choropleth_GnmNum(map_name = "col_departments",
                       map_tiles = "OpenStreetMap",
                       na_color = "#000000",
                       topo_fill_opacity = 0.2,
                       border_weight = 2)

data <- sampleData("Gnm-Num", 100)
lflt_choropleth_GnmNum(data, palette_colors = c("#FEAFEA", "#000AAA"))
lflt_choropleth_GnmNum(data, map_color_scale = "Quantile")
lflt_choropleth_GnmNum(data, map_color_scale = "Bins")
lflt_choropleth_GnmNum(data, map_color_scale = "Bins", map_bins = 3)


lflt_choropleth_GnmNum(data = sampleData("Gnm-Num", 100),
                       legend_title = "Titulo de legenda")
lflt_choropleth_GnmNum(data = sampleData("Gnm-Num", 100),
                       map_graticule = TRUE)

data <- data.frame(Ciudad = c("Cauca", "chocó", "nariño", "nariño"), Val = runif(4, 1, 1000))
lflt_choropleth_GnmNum(data, map_name = "col_pacifico",
                       palette_colors = c("#FEAFEA", "#000CCC"),
                       map_color_scale = "Bins", prefix = "$")
lflt_choropleth_GnmNum(data, map_name = "col_pacifico",
                       tooltip = "<b>{Ciudad}:</b> {Val}", format_cat_sample = "Titulo")
lflt_choropleth_GnmNum(data, map_name = "col_pacifico",
                       title = "Pacífico Colombiano",
                       map_zoom = FALSE,
                       map_tiles = "OpenStreetMap")
lflt_choropleth_GnmNum(data, map_name = "col_pacifico", branding_include = TRUE)
lflt_choropleth_GnmNum(data,
                       map_name = "col_pacifico",
                       palette_colors = c("#FEAFEA", "#000CCC"),
                       branding_include = TRUE,
                       map_tiles = "OpenStreetMap",
                       tooltip = "<p style='font-size:15px;font-weight:bold;'>{Ciudad}</p>",
                       format_cat_sample = "Titulo")
lflt_choropleth_GnmNum(data,
                       map_name = "col_pacifico",
                       branding_include = TRUE,
                       caption = "Esto es un footer",
                       caption_color = "red",
                       title = "Un titulo muy laaaaaaaaaargo",
                       subtitle = "Un subtitulo", map_zoom = F)

lflt_choropleth_Gnm(sampleData('Gnm', 300))
# Gcd Num examples  -------------------------------------------------------

lflt_choropleth_GcdNum()
map_changes <- c("col_municipalities", "venezuela_states")
availableMaps <- setdiff(availableGeodata(), map_changes)
#map(availableMaps, function(n) {lflt_choropleth_GcdNum(map_name = n)})

lflt_choropleth_GcdNum(map_name = "bra_states" )
data <- data.frame(State = c("BR.PA", "BR.RS", "BR.RS", "BR.TO", "BR.MT", "BR.MA", "BR.ES"),
                   `Fake population` = runif(7, 20000, 600000))
lflt_choropleth_GcdNum(data, map_name = "bra_states")
lflt_choropleth_GcdNum(data, map_name = "bra_states",
                       palette_colors = c("#FEAFEA", "#000CCC"),
                       map_color_scale = "Bins",
                       map_zoom = FALSE,
                       title = "Brazil map",
                       subtitle = "Fake map of Brazil population",
                       caption = "Random data")

lflt_choropleth_Gcd(sampleData("Gcd", 300))
lflt_choropleth_Gcd(sampleData("Gcd", 3000),
                    topo_fill_opacity = 1,
                    palette_colors = c("#FEAFEA", "#000CCC"))
#
#
# lflt_bubbles_GnmNum(data, map_name = "col_pacifico")
# lflt_bubbles_GlnGlt(data = sampleData("Gln-Glt"))
#
# data <- data.frame(Lon = runif(15, -75, -70) , Lat = runif(15, -4, 7))
# lflt_bubbles_GlnGlt(data,
#                     map_name = "col_departments",
#                     tooltip = "Coord 1: {Lon} <br/> Coord 2: {Lat}")
