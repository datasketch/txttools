

#' @export
txt_tokenize <- function(d, txtcol = NULL,
                         remove_stop_words = FALSE, lang = NULL,
                         custom_stop_words = NULL,
                         collapse_tokens = TRUE,
                         stem = TRUE){

  if(class(d) == "character"){
    output_class <- "character"
  }
  d <- txt_df(d, txtcol = txtcol)
  if(is.null(txtcol)){
    # Guess the txt column here
    txtcol <- "..txt"
  }
  if(!txtcol %in% names(d)){
    stop("txtcol not found in input")
  }

  x <- d |>
    tidytext::unnest_tokens("..word", "..txt")

  if(remove_stop_words){
  x <- x |>
      txt_remove_stopwords(lang = lang,
                           custom_stop_words = custom_stop_words) |>
      mutate(..word = str_remove_accents(..word))

  }

  if(collapse_tokens){
    y <- x |>
      group_by(..txt_id) |>
      summarise(..tokens = paste(..word, collapse = " "))
  }else{
    y <- x |>
      mutate(..tokens = ..word) |>
      select(-..word)
  }

  out <- d |> left_join(y, by = "..txt_id", multiple = "all")

  if(stem){
  }

  if(output_class == "character"){
    chars <- out |>
      group_split(..txt_id)
    if(collapse_tokens){
      out <- chars |>
        purrr::map(~ paste0(.$..tokens, collapse = " "))
    }else{
      out <- chars |>
        purrr::map(~ .$..tokens)
    }

  }
  out

}


#' @export
str_tokenize <- function(str, by = "words",
                         remove_stop_words = FALSE, lang = NULL,
                         custom_stop_words = NULL,
                         stem = FALSE, ...){

  lang <- lang %||% "en"

  d <- txt_df(str, txtcol = NULL)
  x <- d |>
    tidytext::unnest_tokens("..word", "..txt", token = by)

  if(remove_stop_words){
    x <- x |>
      txt_remove_stopwords(lang = lang,
                           custom_stop_words = custom_stop_words) |>
      mutate(..word = str_remove_accents(..word))

  }

  out <- x |>
    group_split(..txt_id) |>
    purrr::map(~.$..word)



  if(length(out) == 1)
    out <- out[[1]]

  out

}





