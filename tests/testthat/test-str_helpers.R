test_that("multiplication works", {

  str <- c("The lazy fox jumps", "the lazy dog")
  expect_false(str_are_words(str))

  str <- c("The", "lazy", "fox")
  expect_true(str_are_words(str))

})
