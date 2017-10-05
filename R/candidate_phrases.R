#' Find candidate phrases from a vector of characters.
#' 
#' This is an input to the main phrase ranking function. It's included here 
#' because it may have utility as tokenizer that allows tokenization based on 
#' arbitrary tokens and puncuation. The default tokenization does not cross sentences 
#' and line breaks are treated as sentences for the purpose of tokenization.
#' @export
#' @param x a character vector
#' @param split_words a vector of words to split your texts by. By defaults this
#'   calls a function that includes generated stop words.
#' @param split_punct a vector of punctuation to use in splitting your words. By
#'   default calls a function with basic punctuation
#' @return always returns a list with one element for each input text and
#'   phrases stored in a character vector. If the character vector is name
#'   then the names will be used throughout, otherwise this function 
#'   generates sequential documents names. 
#' @examples 
#' candidate_phrases(test_text)
#' candidate_phrases(test_text, c("the","and"), c(","," \\."))
#' candidate_phrases(test_text, NULL, " ")   
candidate_phrases <- function(x, 
                              split_words = smart_stop_words(), 
                              split_punct = basic_punct(),
                              remove_numbers = T){
  
  splits <- prep_stop_words(split_words = split_words, 
                            split_punct = split_punct,
                            remove_numbers = remove_numbers)
  

  candidates <- stringr::str_to_lower(x)
  candidates <- stringr::str_replace_all(candidates, pattern = splits)
  candidates <- stringr::str_split(candidates, pattern = "\\*", simplify = F)
  candidates <- purrr::map(candidates, stringr::str_trim, side = "both")
  candidates <- purrr::map(candidates, function(x) x[nchar(x)> 0])
  
  if (!is.null(names(x))){
    candidates <- purrr::set_names(candidates, names(x))
  } else {
    names <- stringr::str_c(c("document"),as.character(1:length(candidates)), sep = "-")
    candidates <- purrr::set_names(candidates, names)
  }
  
  if (any(lengths(candidates) == 0)){
    warning("Some documents were silently dropped")
    return(candidates[-which(lengths(candidates)== 0)])

  } else(
    candidates
  )
  
}


