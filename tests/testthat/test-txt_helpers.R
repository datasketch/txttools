test_that("txt helpers work", {

  d <- read.csv(sys_file("comienzos-libros.csv"))

  d_txt <- txt_add_id(d)
  expect_true("..txt_id" %in% names(d_txt))


  d_txt <- txt_df(d, txtcol = "comienzo")
  expect_equal(d$comienzo, d_txt$..txt)

  d_txt <- txt_df("Hello world")
  expect_equal("Hello world", d_txt$..txt)
  expect_equal(names(d_txt), c("..txt_id", "..txt"))


})
