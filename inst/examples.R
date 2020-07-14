library(lfltmagic)
# devtools::load_all()
# devtools::document()
# devtools::install()

# Gnm-Num examples --------------------------------------------------------

lflt_choropleth_GnmNum()
lflt_choropleth_GnmNum(map_tiles = "CartoDB")
lflt_choropleth_GnmNum(map_name = "col_departments",
                       map_graticule = TRUE,
                       map_graticule_interval = 3,
                       grid_size =2,
                       grid_color = "red")
lflt_choropleth_GnmNum(map_name = "col_departments",
                       map_tiles = "OpenStreetMap"
)

lflt_choropleth_GnmNum(map_name = "col_departments",
                       map_tiles = "OpenStreetMap",
                       na_color = "#000000",
                       topo_fill_opacity = 0.2,
                       border_weight = 2)

data <- sample_data("Gnm-Num", 100)
lflt_choropleth_GnmNum(data, palette_colors = c("#FEAFEA", "#000AAA"))
lflt_choropleth_GnmNum(data, map_color_scale = "Quantile", map_quantile = 5)
lflt_choropleth_GnmNum(data, map_color_scale = "Bins")
lflt_choropleth_GnmNum(data, map_color_scale = "Bins", map_bins = 3)


lflt_choropleth_GnmNum(data = sample_data("Gnm-Num", 100),
                       legend_title = "Titulo de legenda")
lflt_choropleth_GnmNum(data = sample_data("Gnm-Num", 100),
                       map_graticule = TRUE)

data <- data.frame(Ciudad = c("Cauca", "chocó", "nariño", "nariño"), pepito = runif(4, 1, 1000))
lflt_choropleth_GnmNum(data, map_name = "col_pacifico",
                       palette_colors = c("#FEAFEA", "#000CCC"),
                       map_color_scale = "Bins", prefix = "$")
lflt_choropleth_GnmNum(data, map_name = "col_pacifico",
                        tooltip = "<b>Hola Lena esta ciudad es {Ciudad}:</b> que tiene {pepito} pepitos",
                       format_cat_sample = "Titulo",
                       text_color = "red")
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
                       tooltip = "<p style='font-size:15px; color:#FEAFEA;font-weight:bold;'>{Ciudad}</p>",
                       format_cat_sample = "Titulo")
lflt_choropleth_GnmNum(data,
                       map_name = "col_pacifico",
                       branding_include = TRUE,
                       caption = "Esto es un footer",
                       caption_color = "red",
                       title = "Un titulo muy laaaaaaaaaargo",
                       subtitle = "Un subtitulo", map_zoom = F)

lflt_choropleth_Gnm(sample_data('Gnm', 300))
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

lflt_choropleth_Gcd(sample_data("Gcd", 300))
lflt_choropleth_Gcd(sample_data("Gcd", 3000),
                    topo_fill_opacity = 1,
                    palette_colors = c("#FEAFEA", "#000CCC"))



# Gnm-Num bubbles examples ------------------------------------------------
lflt_bubbles_GnmNum(sample_data("Gnm-Num", 100),
                    palette_colors = 'orange',
                    background_color = "#000",
                    topo_fill_opacity = 0.2)
data <- data.frame(Ciudad = c("Cauca", "chocó", "nariño", "nariño"), Val = runif(4, 1, 1000))
lflt_bubbles_GnmNum(data,
                    map_name = "col_pacifico",
                    map_min_size = 5,map_max_size = 30)
lflt_bubbles_Gnm(sample_data("Gnm", 300),
                 palette_colors = 'purple')


# Gcd Num bubbles examples ------------------------------------------------

lflt_bubbles_GcdNum(sample_data("Gcd-Num"))

data <- data.frame(State = c("BR.PA", "BR.RS", "BR.RS", "BR.TO", "BR.MT", "BR.MA", "BR.ES"),
                   Population = runif(7, 300000, 6000000))
lflt_bubbles_GcdNum(data,
                    map_name = "bra_states")
lflt_bubbles_GcdNum(data,
                    map_name = "bra_states",
                    format_num_sample = "1 244,2",
                    map_min_size = 5,
                    map_max_size = 30, tooltip = "<b>Información</b> </br> {State}: {Population}")

lflt_bubbles_Gcd(sample_data("Gcd", 300),
                 palette_colors = 'purple')
# Gln-Glt-Num examples ----------------------------------------------------
lflt_bubbles_GlnGltNum()
lflt_bubbles_GlnGltNum(sample_data("Gln-Glt-Num"), map_min_zoom = 5)
lflt_bubbles_GlnGltNum(sample_data("Gln-Glt-Num"),
                       map_stroke = TRUE,
                       palette_colors = "#FEAFEA")
lflt_bubbles_GlnGltNum(sample_data("Gln-Glt-Num"),
                       map_tiles = "OpenStreetMap")

lflt_bubbles_GlnGlt(sample_data("Gln-Glt"))
lflt_bubbles_GlnGlt(sample_data("Gln-Glt"), map_radius = 10)

data <- data.frame(Lon = runif(35, -75, -70) , Lat = runif(35, -3.5, 6))
lflt_bubbles_GlnGlt(data,
                    map_name = "col_departments",
                    tooltip = "Coord 1: {Lon} <br/> Coord 2: {Lat}",
                    palette_colors = "#000000",
                    na_color = "#FEAFEA",
                    topo_fill_opacity = 0.2)
