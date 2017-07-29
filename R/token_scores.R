
get_token_scores <- function(candidates) {

  get_unique_tokens <- function(x) unique(purrr::as_vector(quanteda::tokenize(x)))
  
  get_scores <- function(token, phrase_tokens) {
    purrr::keep(phrase_tokens, function(x) token %in% x)
  }

  tokens <- get_unique_tokens(candidates)
  token_list <- quanteda::tokenize(candidates)
  
  token_scores <- purrr::map(tokens, get_scores, phrase_tokens = token_list)
  
  df <- dplyr::data_frame(
    freq = lengths(token_scores),
    degree = purrr::as_vector(purrr::map(token_scores, function(x) sum(lengths(x))))
  )
  
  df$degreeFreq <- df$degree / df$freq
  
  df$tokens <- tokens
  
  df
} 



innaug <- quanteda::data_corpus_inaugural$documents$texts


scores <- rake(innaug[1:2])

candidates_test <- candidate_phrases(innaug[1:3])

tokens <- get_unique_tokens(candidates_test[[3]])
token_list <- quanteda::tokenize(candidates_test[[3]])

get_scores <- function(token, phrase_tokens) {
    purrr::keep(phrase_tokens, function(x) fastmatch::fmatch(token, x))
  }

token_scores <- purrr::map(tokens, get_scores, phrase_tokens = token_list)


dt <- data.table::data.table(as.matrix(quanteda::dfm(candidates_test[[3]])))

dt2 <- dt[first > 0, rowSums(), by = names(dt) ]

dt <- dt[, .(freq = .N, degree = sum(rowsums())),  ]


?data.table(