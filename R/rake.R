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
#' 
rake <- function(x, n = 10, method = "degreeFreq") {
  
  score_one <- function(test_token, df, method = method) {
    
    sum(df[df$tokens %in% test_token, ][ , method])
    
  }
  
  get_scores <- function(token_list, df, method, n = n){
    
    scores <- purrr::map_dbl(token_list, .f = score_one, df  = df, method  = method)
    
  }
  
  candidates <<- candidate_phrases(x)
  candidates <- candidates[purrr::map_lgl(candidates, function(x) length(x) != 0)]
  
  token_list <- purrr::map(candidates, quanteda::tokenize)
  token_scores <- purrr::map(candidates, get_token_scores)
  
  phrase_scores <- purrr::map2(.x = token_list, .y = token_scores, .f = get_scores, n = n, method = method)
  phrase_scores <- purrr::map2(phrase_scores, candidates, purrr::set_names)
  phrase_scores <- purrr::map(phrase_scores, function(x)  x[order(x, decreasing =T) ][1:n] )
  
  phrase_scores
  
}


