

#' @export
txt_stem <- function(d, txtcol = NULL, lang = NULL,
                     unnest = FALSE,
                     one_stem = TRUE){

  if(is_character(d)){
    return(str_stem(d, lang = lang, one_stem = one_stem))
  }

  if(is.null(txtcol)){
    # Guess the txt column here
    txtcol <- "..txt"
  }
  if(!txtcol %in% names(d)){
    stop("txtcol not found in input")
  }

  d_txt <- txt_df(d, txtcol = txtcol)

  if(unnest){

  }else{

    str_stem(d_txt$..txt, lang = {{lang}}, one_stem = {{one_stem}})

    stems <- d_txt |>
      mutate(..stem = str_stem(..txt, lang = {{lang}}, one_stem = {{one_stem}}))
  }
  stems

}



#' @export
str_stem <- function(str, lang = NULL, one_stem = TRUE, collapse = TRUE){

  if(is.null(lang)){
    dict <- "en_US"
  } else if(lang == "es"){
    dict <- "es_ES"
  } else{
    dict <- "en_US"
  }
  check_dictionary(dict)
  if(!dict %in% hunspell::list_dictionaries()){
    dict_path <- sys_file(paste0(file.path("dictionaries", dict), ".dic"))
    dict <- hunspell::dictionary(dict_path)
  }

  if(str_are_words(str)){
    stems_list <- hunspell::hunspell_stem(str, dict = dict)
  }else{
    tokens <- str_tokenize(str, lang = lang, collapse = FALSE)
    stems_list <- purrr::map(tokens, ~ hunspell::hunspell_stem(., dict = dict))
  }

  if(one_stem){
    # Make sure to return the last stem word from possible values
    stems <- stems_list |>
      purrr::map_chr(~ list_str_pull_last(., collapse = TRUE))
    if(collapse){
      stems <- stems |>
        str_collapse()
    }
  } else{
    stems <- stems_list
  }
  stems
}

