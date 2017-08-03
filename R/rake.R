get_token_scores <- function(candidate_phrase_set){
  
  tokens <- tokenizers::tokenize_words(candidate_phrase_set)
  lengths <- lengths(tokens)
  degree_vec <- purrr::as_vector(purrr::map(lengths, function(x) rep(x, x)))
  phrase_vec <- purrr::as_vector(purrr::map2(candidate_phrase_set, lengths, rep))
  
  temp_df <- dplyr::data_frame(
    tokens = purrr::as_vector(tokens),
    degrees = degree_vec,
    phrase_vec = phrase_vec
    
  )
  
  temp_df <- temp_df %>% 
    group_by(tokens) %>% 
    mutate(
      freq = n(),
      degree = sum(degrees),
      degreeFreq = degree / freq)
  
  temp_df
}


get_scores <- function(score_df) {
  score_df %>% 
    group_by(phrase_vec) %>% 
    summarise(
      score = sum(degreeFreq)
    )
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
rake <- function(x, n = 10, split_words = stop_words(), split_punct = basic_punct()) {
  
  
  if(!purrr::is_character(x)){
    stop("rake only works on character vectors")
  }
  
  candidates <- candidate_phrases(x, split_words = split_words, split_punct = split_punct)
  token_list <- purrr::map(candidates, tokenizers::tokenize_words)
  token_scores <<- purrr::map(candidates, get_token_scores)
  
  scores <- purrr::map(token_scores, get_scores)
  scores
  }


