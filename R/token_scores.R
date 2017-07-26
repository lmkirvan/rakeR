examples <- readr::read_csv("Book1.csv")

get_token_scores <- function(candidate_phrases) {
  
  get_unique_tokens <- function(x) unique(purrr::as_vector(quanteda::tokenize(x)))
  in_list <- function(i, list) {purrr::map_lgl(list, .f = function(x) i %in% x )}
  which_phrases <- function(i, list){list[in_list(i, list)] }
  
  get_scores <- function(token, phrase_tokens) {
    phrases <- which_phrases(token, phrase_tokens)
    freq <- length(phrases)
    degree <- sum(lengths(phrases))
    degreeFreq <- degree / freq
  
  c("freq" = freq, "degree" = degree, "degreeFreq" = degreeFreq)
  
  }

  tokens <- get_unique_tokens(candidate_phrases)
  token_list <- quanteda::tokenize(candidate_phrases)
  
  token_scores <- purrr::map(tokens, get_scores, phrase_tokens = token_list)
  

  df <- data_frame(
    freq = purrr::as_vector(purrr::map(token_scores, "freq")),
    degree = purrr::as_vector(purrr::map(token_scores, "degree")),
    degreeFreq = purrr::as_vector(purrr::map(token_scores, "degreeFreq"))
  )
  
  df$tokens <- tokens
  
  df
}


get_rake_phrases <- function(x, n = 10, method = "degreeFreq") {
  
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



examples <- examples[ is.null(examples$Fairresolution), ]

start_time <- Sys.time()
scores <- get_rake_phrases(examples$Fairresolution, n = 5)
finish_time <- Sys.time()

finish_time - start_time


scores[[1]]






