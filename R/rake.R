get_phrase_scores <- function(candidates, top_fraction = top_fraction){
  
  tokens <- purrr::map(candidates, tokenizers::tokenize_words)
  token_lengths <- purrr::map(tokens, lengths)
  
  temp_df <<- dplyr::data_frame(
    names = purrr::as_vector(purrr::map2(names(candidates), purrr::map_int(token_lengths, sum), rep)),
    phrases = purrr::as_vector(purrr::map2(candidates, token_lengths, rep)),
    tokens = purrr::as_vector(purrr::at_depth(tokens, 1, .f = purrr::as_vector)),
    degree = purrr::as_vector(purrr::map2(token_lengths, token_lengths, rep))  
  )
  
  temp_df <- temp_df %>% 
    group_by(names, tokens) %>% 
    mutate(
      freq = n(),
      degree = sum(degree),
      degreeFreq = degree / freq)%>%
    group_by(names, phrases) %>% 
    summarise(
      score = sum(degreeFreq)) %>% 
    filter(
      score > quantile(score, top_fraction)
    )
     
  
  temp_df
}  


# get_token_scores <- function(candidate_phrase_set){
#   
#   tokens <- tokenizers::tokenize_words(candidate_phrase_set)
#   token_lengths <- lengths(tokens)
#   degree_vec <- rep(token_lengths, token_lengths) 
#   phrase_vec <- rep(candidate_phrase_set, token_lengths)
#   
#   temp_df <- dplyr::data_frame(
#     tokens = purrr::as_vector(tokens),
#     degrees = degree_vec,
#     phrase_vec = phrase_vec
#     
#   )
#   
#   temp_df <- temp_df %>% 
#     dplyr::group_by(tokens) %>% 
#     dplyr::mutate(
#       freq = n(),
#       degree = sum(degrees),
#       degreeFreq = degree / freq)
#   
#   temp_df
# }
# 
# 
# get_phrase_scores <- function(score_df, top_fraction) {
#   score_df %>% 
#     dplyr::group_by(phrase_vec) %>% 
#     dplyr::summarise(
#       score = sum(degreeFreq)) %>% 
#     dplyr::arrange(
#       desc(score)) %>% 
#     head(round(nrow(score_df) * top_fraction))
# }
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
#' @param top_fraction the fraction of the most highly ranked phrases to return
#' @param method one of "degreeFreq", "degree", or "freq". degreeFreq is simply 
#'   degree // frequency.
#' @examples 
#' rake(test_text, 5, "degree")
#' rake(test_text, 15)
#' @value returns a list with elements composed of one named integer vector for
#'   each document. key phrases are names and values are ranked
rake <- function(x, 
                 split_words = smart_stop_words(),
                 split_punct = basic_punct(),
                 top_fraction = 1/3) {
  
  
  if(!purrr::is_character(x)){
    stop("rake only works on character vectors")
  }
  
  candidates <- candidate_phrases(x, split_words = split_words, split_punct = split_punct)
  scores <- get_phrase_scores(candidates, top_fraction = top_fraction)
  
  scores
 
}


