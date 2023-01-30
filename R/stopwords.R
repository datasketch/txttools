
#' @export
txt_remove_stopwords <- function(d, lang = NULL,
                                 custom_stop_words = NULL){

  if(is.null(lang)) stop("Need to provide lang when removing stop words")
  stop_words <- txt_stopwords(lang, custom_stop_words = custom_stop_words)
  d |>
    anti_join(stop_words, by = "..word")
}


#' @export
txt_stopwords <- function(lang = "en", custom_stop_words = NULL){

  # https://github.com/stopwords-iso/stopwords-es/blob/master/stopwords-es.txt


  if(lang == "en") lang_long <- "english"
  if(lang == "es") lang_long <- "spanish"
  stop_words <- tidytext::get_stopwords(language = lang) |>
    rename(..word = word)
  if(!is.null(custom_stop_words)){
    stop_words <- bind_rows(
      tibble(..word = custom_stop_words, lexicon = "custom"),
      stop_words
    )
  }
  stop_words
}

