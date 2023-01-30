


#' @export
str_are_words <- function(str){
  if(!is_character(str))
    stop("Need a character vector")
  all(stringr::str_count(str, "\\S+") == 1)
}

#' @export
str_collapse <- function(str, collapse = " "){
  str <- str |> purrr::keep(~nchar(.) > 0)
  paste0(str, collapse = collapse)
}

#' @export
list_str_pull_last <- function(str, collapse = TRUE){
  out <- purrr::map_chr(str, function(x){
    ln <- length(x)
    if( ln == 0) return("")
    x[[ln]]
  })
  if(collapse){
    out <- str_collapse(out)
  }
  out
}


#' @export
str_remove_accents <- function(string){
  accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"
  translation <- "aeiouAEIOUaeiouyAEIOUYaeiouAEIOUaeiouAEIOUnNc"
  chartr(accents, translation, string)
}
