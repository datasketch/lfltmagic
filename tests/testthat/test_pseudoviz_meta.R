context("lfltmagic meta data")

test_that("Viz meta info", {
  #library(lfltmagic)
  db <- Rd_db("lfltmagic")
  meta <- map(db, tools:::.Rd_get_section, "section")
  meta <- meta[grepl("test_docs",names(meta))]
  ftype <- meta$lflt_test_docs.Rd
  expect_error(lfltmagic:::cleanFtypeDoc(ftype),"No section name ftype")
  ftype <- meta$lflt_test_docs2.Rd
  expect_equal(lfltmagic:::cleanFtypeDoc(ftype),c("Ca","Ca-Ca-Ca"))
  #lfltFtype()
  #expect_error()
  #expect_true()
  #expect_false()
})


