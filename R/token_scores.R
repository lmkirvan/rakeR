
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
  
  df <- dplyr::data_frame(
    freq = purrr::as_vector(purrr::map(token_scores, "freq")),
    degree = purrr::as_vector(purrr::map(token_scores, "degree")),
    degreeFreq = purrr::as_vector(purrr::map(token_scores, "degreeFreq"))
  )
  
  df$tokens <- tokens
  
  df
}







