


respondant_row <- function(d, regex){
  col <- respondant_col(d)
  docente <- map_df(d, ~ grepl("^docente", ., ignore.case = TRUE)) |>
    select(..id, any_of(col))
  docente$..id <- d$..id
  docente |>
    filter(get({{col}})) |>
    pull(..id)
}

respondant_col <- function(d, regex){
  docente <- map_df(d, ~sum(grepl(regex, ., ignore.case = TRUE))) |>
    pivot_longer(everything(), names_to = "column", values_to = "match_docente")
  estudiante <- map_df(d, ~sum(grepl("estudiante", ., ignore.case = TRUE))) |>
    pivot_longer(everything(), names_to = "column", values_to = "match_estudiante")
  n <- nrow(d)
  matches <- left_join(docente, estudiante, by = "column") |>
    mutate(match = n * match_docente + match_estudiante) |>
    select(column, match)
  matches |>
    slice_max(match) |>
    pull(column)
}



