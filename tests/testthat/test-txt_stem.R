test_that("Txt Stem", {




  str <- "jump jumper jumps jumping"
  expect_equal(str_stem(str, lang = "en"), str_collapse(rep("jump", 4)))

  str <- "salto saltó salté salte saltare"
  expect_equal(str_stem(str, lang = "es"), str_collapse(rep("saltar", 5)))


  d <- read.csv(sys_file("comienzos-libros.csv"))

  expect_error(txt_stem(d, txtcol = NULL, lang = "es"),
               "txtcol not found in input")

  txt_stem(d, txtcol = "comienzo", lang = "es")

  stems <- txt_stem(d,  txtcol = "comienzo", lang = "es", one_stem = FALSE)
  expect_true(is.list(stems$..stem))

})
