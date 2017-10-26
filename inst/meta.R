library(tidyverse)
library(homodatum)
# Create meta data for funs
db <- tools::Rd_db("lfltmagic")
if(length(db)==0) stop("Restart session")
f <- function(rd){
  #rd <- db[[1]]
  rd <- capture.output(rd)
  con <- textConnection(rd)
  l <- Rd2roxygen::parse_file(con)
  l <- map_if(l,~length(.)==0,function(x)'')
  map(l,paste,collapse = "_")
}
funs <- map(db,f)
funsMeta <- funs %>% bind_rows()
funsMeta <- funsMeta %>% filter(grepl("^lflt_",name))
meta <- funsMeta[c("name","title","desc","section")]
meta$ctypes <- stringr::str_extract(meta$section,"(?<=\n).*?(?=\n)$")
meta$section <- NULL
meta$ftype <- ctypesToFtype(meta$ctypes, as_string = TRUE)
meta$group <- stringr::str_extract(meta$name,"(?<=_).*?(?=_)")
meta <- left_join(meta, select(read_csv("inst/meta-status.csv"), name, status), by = "name")
write_csv(meta,file.path("inst","meta.csv"))


## meta-status
metaStatus <- data.frame("name" = c("lflt_bubbles_GcdCatNum", "lflt_bubbles_GlnGltCatNum", "lflt_bubbles_GnmCatNum",
                                    "lflt_bubbles_grouped_GcdCat", "lflt_bubbles_grouped_GlnGltCat", "lflt_bubbles_grouped_GnmCat",
                                    "lflt_bubbles_size_Gcd", "lflt_bubbles_size_GcdCat", "lflt_bubbles_size_GcdNum",
                                    "lflt_bubbles_size_GlnGlt", "lflt_bubbles_size_GlnGltCat", "lflt_bubbles_size_GlnGltNum",
                                    "lflt_bubbles_size_Gnm", "lflt_bubbles_size_GnmCat", "lflt_bubbles_size_GnmNum",

                                    "lflt_choropleth_Gcd", "lflt_choropleth_GcdCat", "lflt_choropleth_GcdNum",
                                    "lflt_choropleth_Gnm", "lflt_choropleth_GnmCat", "lflt_choropleth_GnmNum",

                                    "lflt_heatmap_Gcd", "lflt_heatmap_GlnGlt", "lflt_heatmap_Gnm",

                                    "lflt_markers_GlnGlt", "lflt_markers_GlnGltCat", "lflt_markers_GlnGltImg"),



                         "status" = c("OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK",

                                      "OK", "OK", "OK", "OK", "OK", "OK",

                                      "OK", "OK", "OK",

                                      "OK", "OK", "OK"),

                         "coments" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,

                                       NA, NA, NA, NA, NA, NA,

                                       NA, NA, NA,

                                       NA, NA, NA)
)
#metaStatus <- left_join(select(hgchmagic::hgchMeta(), name), metaStatus, by = name)
write_csv(metaStatus, "inst/meta-status.csv")
