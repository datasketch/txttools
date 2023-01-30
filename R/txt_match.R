
#' @export
txt_match <- function(d1, d2, by, method = NULL,
                      tokenize = TRUE,
                      lang = "en",
                      custom_stop_words = NULL){

  if("all" %in% method){
    method <- stringdist_methods()
  }

  if(length(by) > 1) stop("by length must be 1... for now")
  if(!by %in% names(d1)) stop("by column not in d1")
  if(!by %in% names(d2)) stop("by column not in d2")
  # if(!is.null(names(by))){
  #   d1col <- d2col <- by
  # } else{
  #   d1col <- names(by)
  #   d2col <- unname(by)
  # }

  d1 <- txt_add_id(d1) |>
    rename(..txt = by)

  d2 <- d2 |>
    txt_add_id() |>
    select(..txt_id, all_of(by)) |>
    rename(..txt = by)

  if(length(method) == 1){
    dm <- fuzzyjoin_stringdist_method(method)(d1, d2)
  }else{
    ldm <- purrr::map(method, ~ fuzzyjoin_stringdist_method(.)(d1,d2))
    dm <- purrr::reduce(ldm, dplyr::left_join)
  }




}



fuzzyjoin_stringdist_method <- function(method){


  if(!all(method %in% stringdist_methods())){
    stop("Method must be one of: ",
         paste0(stringdist_methods(), collapse = ", "))
  }

  function(d1, d2){
    outcol_name <- paste0("distance_", method)
    outcol_rankname <- paste0("rank_", method)
    fuzzyjoin::stringdist_left_join(d1, d2,
                                    method = method,
                                    by = "..txt",
                                    max_dist = Inf,
                                    distance_col = "distance",
                                    ignore_case = TRUE) |>
      arrange(distance) |>
      mutate(rank = row_number()) |>
      rename(txt = "..txt.x", txt2 = "..txt.y",
             !!outcol_name := "distance",
             !!outcol_rankname := "rank"
             )
  }


}




stringdist_methods <- function(){
  c("osa", "lv", "dl", "hamming", "lcs", "qgram",
    "cosine", "jaccard", "jw")
}



#' @export
string_match_rank <- function(v){

  d0 <- tibble(txt = v) |> add_txt_id()

  words <- d0 |> tidytext::unnest_tokens(word, txt)
  words1 <- words |> txt_remove_stopwords(lang = "es")

  #sentences <- d0 |> tidytext::unnest_tokens(sentences, txt)

  # TF, IDF... no nos sirve. Normalmente los textos son pequeños
  # Las frecuencias de palabras son únicas
  # d3 <- d2 %>%
  #   group_by(..txt_id, word) %>%
  #   tally() %>%
  #   arrange(desc(n)) |>
  # bind_tf_idf(word, modalidad, n)

  library(widyr)
  pairwise_count(words1, word, ..txt_id)

  words1 |>
   group_by(word) |>
    pairwise_cor(word, ..txt_id)

  group_by(word)


}




