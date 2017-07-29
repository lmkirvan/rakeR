#' Find candidate phrases from a vector of characters.
#' 
#' This is an input to the main phrase ranking function. It's included here 
#' because it may have utility as tokenizer that allows tokenization based on 
#' arbitrary tokens and puncuation. The tokenization does not cross sentences 
#' and line breaks are treated as sentences for the purpose of tokenization.
#' 
#' @param x a character vector
#' @param split_words a vector of words to split your texts by. By defaults this
#'   calls a function that includes generated stop words.
#' @param split_punct a vector of punctuation to use in splitting your words. By
#'   default calls a function with basic punctuation
#' @return always returns a list with one element for each input text and
#'   phrases stored in a character vector.
#' @examples 
#' candidate_phrases(test_text)
#' candidate_phrases(test_text, c("the","and"), c(","," \\."))
#' candidate_phrases(test_text, NULL, " ")   



candidate_phrases <- function(x, split_words = stop_words(), split_punct = basic_punct()){
  
  line_break <- "\\n(?!\\.)"
  
  splits <- prep_stop_words(split_words = split_words, split_punct = split_punct)
  
  sentences <- stringr::str_replace_all(x, line_break, "\\.")
  sentences <- quanteda::tokenize(sentences, what = "sentence")
  sentences <- purrr::map(sentences, .f = stringr::str_replace_all, pattern =  "\\.", replacement = "")
  # there are stupid curly apostrophes that must be removed
  sentences <- purrr::map(sentences, .f = stringr::str_replace_all, pattern =  "’", replacement = "'")
  sentences <- purrr::map(sentences, .f = tolower)

  candidates <- purrr::map(
    sentences, 
    .f = stringr::str_replace_all, pattern = splits)

  candidates <- purrr::map(candidates, stringr::str_split, pattern = "\\*", simplify = F)
  # the list at this point is list(document)  -> list(sentence1... sentenceN) -> list(token1...2) 
  candidates <- purrr::at_depth(.x = candidates, .depth = 1, .f = purrr::as_vector)
  # todo this next line shouldn't be necessary, but I can't figure out why the split is comming back with spaces
  candidates <- purrr::map(candidates, stringr::str_trim, side = "both")
  candidates <- purrr::map(candidates, function(x) x[nchar(x) > 1])

  candidates
}




