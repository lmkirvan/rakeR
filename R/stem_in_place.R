#' stem but don't tokenize
#' 
#' a helper function to stem your chr vector and return stemmed vector this
#' reduces the total vocab significantly
#' @export
#' @param x is a chr vector.
#' @param language should be one of “danish”, “dutch”, “english”, “finnish”,
#'   “french”, “german”, “hungarian”, “italian”, “norwegian”, “porter”,
#'   “portuguese”, “romanian”, “russian”, “spanish”, “swedish”, “turkish”
stem_in_place <- function(x, language = "english"){
  
  x <- tokenizers::tokenize_word_stems(x, language = language)
  x <- purrr::map(x, stringr::str_c, collapse = " ")
  x <- purrr::as_vector(x)
  
  x
  
}











