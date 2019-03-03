
library(devtools)
load_all()
document()
install()

library(lfltmagic)

# choropleth
dta <- sampleData("Gcd")
lflt_choropleth_Gcd(mapName = "col_departments",
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
