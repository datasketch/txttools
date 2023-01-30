

#' @export
txt_add_id <- function(d){
  n <- nrow(d)
  d <- d |>
    mutate(..txt_id = 1:n) |>
    relocate(..txt_id)
  d
}

txt_df <- function(d, txtcol){

  if(!"data.frame" %in% class(d)){
    x <- tibble::tibble(..txt = d) |>
      txt_add_id()
    return(x)
  } else if (is_character(d)){
    stop("Need a character or dataframe with characters")
  } else if(!"tbl_df" %in% class(d)){
      d <- tibble::as_tibble(d)
  }

  d |>
    mutate(..txt = .data[[txtcol]]) |>
    txt_add_id()
}



