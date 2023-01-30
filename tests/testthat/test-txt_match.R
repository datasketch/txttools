test_that("Txt match", {

  library(readr)

  # 2 data frames (d1,d2) with txt columns
  # match a txt column in d1, with the txt column in d2

  d <- read_csv(sys_file("exam-answers-1.csv"),
                col_types = cols(.default = "c"))

  d1 <- d |> filter(Respondiente == "Docente") |>
    select(Respuesta_Pregunta_1)
  d2 <- d |> filter(Respondiente != "Docente")
  by <- "Respuesta_Pregunta_1"

  dm_jw <- txt_match(d1, d2, by = by, method = "jw")
  dm_cosine <- txt_match(d1, d2, by = by, method = "cosine")
  dm_lcs <- txt_match(d1, d2, by = by, method = "lcs")

  method <- c("jw", "cosine")
  dm <- txt_match(d1, d2, by = by, method = method)

  method <- "all"
  dm <- txt_match(d1, d2, by = by, method = method)


})
