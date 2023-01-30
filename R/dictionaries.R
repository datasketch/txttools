

check_dictionary <- function(dict){
  if(!dict %in% available_dictionaries())
    stop("Dictionary not available: ", )
}

available_dictionaries <- function(){
  c(hunspell::list_dictionaries(), "es_ES")
}

