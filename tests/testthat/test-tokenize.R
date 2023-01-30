test_that("Tokenization works", {


  str <- "The quick brown fox"
  expect_equal(str_tokenize(str),
               c("the", "quick", "brown", "fox"))

  str <- c("The quick brown fox", "jumps over the lazy dog")
  expect_equal(str_tokenize(str)[[2]],
               c("jumps", "over", "the", "lazy", "dog"))

  expect_equal(str_tokenize(str, remove_stop_words = TRUE)[[2]],
               c("jumps", "lazy", "dog"))




  # library(readr)
  #
  # d <- read_csv(sys_file("comienzos-libros.csv"),
  #                      col_types = cols(.default = "c"))
  #
  # x <- d$comienzo[1]
  # t <- txt_tokenize(x)
  #
  # t <- txt_tokenize(x) # not removing stopwords
  # expect_equal(t$..token, gsub(",|\\.", "", tolower(t$..txt)))
  #
  # expect_error(txt_tokenize(x, remove_stop_words = TRUE),
  #              "Need to provide lang when removing stop words")
  #
  # x <- d$comienzo[2]
  # t <- txt_tokenize(x, remove_stop_words = TRUE, lang = "es")
  # t$tokenized
  #
  # t <- txt_tokenize(d, remove_stop_words = TRUE, lang = "es")


})
