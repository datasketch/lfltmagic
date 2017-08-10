
library(lfltmagic)

data <- sampleData("Gcd-Num",nrow = 10)
scope <- "world_countries"
lflt_bubbles_size_GcdNum(data, scope)

data <- sampleData("Glt-Gln",scope = "col_municipalities", n = 100)
data <- sampleData("Glt-Gln",scope = "col_dc_districts", n = 100)
lflt_bubbles_size_GltLn(data, radius = 5)

