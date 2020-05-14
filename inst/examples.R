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
