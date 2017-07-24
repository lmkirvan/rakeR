
# tokenize with words and punctuation -------------------------------------

# only want to generate stopwords once when getting candidates from a vector
prep_stop_words <- function(split_words = stop_words(), split_punct = basic_punct()) {
  if (is.null(split_words)) {
    stop("Please provide a vector of stop words or use provided stopwords")
  } 
  if (is.null(split_punct)){
    stop("Please provide a vector of puncation for tokenizing or use provided punctuation")
  } 
  
  
  words <- stringr::str_c("\\b(", split_words, ")\\b", sep = "")
  names <- c(words, split_punct)
  splits <- rep("*", length(names))
  
  purrr::set_names(splits, names)
}



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
  candidates <- at_depth(.x = candidates, .depth = 1, .f = as_vector)
  # todo this next line shouldn't be necessary, but I can't figure out why the split is comming back with spaces
  candidates <- purrr::map(candidates, stringr::str_trim, side = "both")
  candidates <- purrr::map(candidates, function(x) x[nchar(x) > 1])

  candidates
}











