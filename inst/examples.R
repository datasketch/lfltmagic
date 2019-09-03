library(devtools)
load_all()
document()
install()
library(lfltmagic)
df <- sampleData("Cat-Gln-Glt")
m <- lflt_bubbles_CatGlnGlt(data = df)
lflt_bubbles_CatGlnGlt(data = df, mapName = NULL)
opts <- list(
  tiles = "OpenTopoMap"
)
lflt_bubbles_CatGlnGlt(data = df, opts = opts)

df <- sampleData("Gln-Glt")
lflt_bubbles_GlnGlt(data = df)
lflt_bubbles_GlnGlt(data = df, mapName = NULL)

df <- sampleData("Gnm-Num",50)
lflt_bubbles_GnmNum(df)
df <- sampleData("Gnm",4)
opts <- list(title = "hola",
             min_radius = 5,
             max_radius =  10,
             agg_text = " ",
             stroke = T,
             marks = c("-", "."))
lflt_bubbles_Gnm(df, opts = opts)

lflt_bubbles_CatGlnGltNum(sampleData("Cat-Gln-Glt-Num", nrow = 10))

# choropleth
dta <- sampleData("Gcd")

lflt_bubbles_GcdNum(mapName = "col_departments",
data = sampleData("Gcd-Num"),
                    opts = list(borderColor = 'red'))

data <- sampleData('Gnm', 300)
lflt_choropleth_Gnm(data = data,
                    opts = list(borderColor = 'red'))
lflt_choropleth_Gcd(data = dta) %>%  setView(0, 0, 3)
lflt_choropleth_Gcd(data = dta,
                    opts = list(
                            tiles = "Esri.WorldImagery",
                            graticule = TRUE))

dta <- sampleData("Gcd-Cat")
lflt_choropleth_GcdCat()
lflt_choropleth_GcdCat(data = dta)

dta <- sampleData("Gnm")
lflt_choropleth_Gnm()
lflt_choropleth_Gnm(data = dta)

dta <- sampleData("Gnm-Num")
lflt_choropleth_GnmNum()
lflt_choropleth_GnmNum(data = dta)


lflt_bubbles_GnmNum(sampleData("Gnm-Num"))
