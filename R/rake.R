get_token_scores <- function(candidate_phrase_set){
  
  tokens <- tokenizers::tokenize_words(candidate_phrase_set)
  
  lengths <- lengths(tokens)
  
  vector <- purrr::as_vector(purrr::map(lengths, function(x) rep(x, x)))
  
  temp_df <- dplyr::data_frame(
    tokens = purrr::as_vector(tokens),
    degrees = vector
  )
  
  temp_df <- temp_df %>% 
    group_by(tokens) %>% 
    summarise(
      freq = n(),
      degree = sum(degrees)) %>% 
    mutate(degreeFreq = degree / freq)
  
  temp_df
}

score_one <- function(test_token, df, method = method) {
  
  sum(df[df$tokens %in% test_token, ][ , method], na.rm = T)
}

get_scores <- function(token_list, df, method){
  
  #if (any(lengths(token_list) < 1)){
  #  token_list <- token_list[lengths(token_list) > 0]
  #}
  
  scores <- purrr::map_dbl(token_list, .f = score_one, df  = df, method  = method)
}


#' Rapid automatic extraction of keywords from documents
#' 
#' This is the main functions for extracting and returning ranked keywords. This
#' algorithm works by tokenizing words and phrases based on stopwords. After 
#' tokenization the frequency of tokens and their degree \(i.e. their 
#' coocurrence with words in the remaining phrases\) are used to rank phrases. 
#' Stopwords can include both common english words and can be enhanced by 
#' including a vector of domain specific stop words.
#' 
#' 
#' @param x a character vector of texts to find keywords for
#' @param n the number of ranked keyphrases to return
#' @param method one of "degreeFreq", "degree", or "freq". degreeFreq is simply 
#'   degree // frequency.
#' @examples 
#' rake(test_text, 5, "degree")
#' rake(test_text, 15)
#' @value returns a list with elements composed of one named integer vector for
#'   each document. key phrases are names and values are ranked
#'   
#'   
rake <- function(x, n = 10, method = "degreeFreq", split_words = stop_words(), split_punct = basic_punct()) {
  
  if(!purrr::is_character(x)){
    stop("rake only works on character vectors")
  }
  
  candidates <- candidate_phrases(x, split_words = split_words, split_punct = split_punct)
  #candidates <- candidates[purrr::map_lgl(candidates, function(x) length(x) != 0)]
  
  token_list <- purrr::map(candidates, tokenizers::tokenize_words)
  token_scores <- purrr::map(candidates, get_token_scores)
  
  phrase_scores <- purrr::map2(.x = token_list, .y = token_scores, .f = get_scores, method = method)
  phrase_scores <- purrr::map2(phrase_scores, candidates, purrr::set_names)
  phrase_scores <- purrr::map(phrase_scores, function(x)  x[order(x, decreasing = T) ][1:n] )
  
  phrase_scores
  
}

