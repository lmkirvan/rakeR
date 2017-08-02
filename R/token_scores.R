


#
##get_token_scores <- function(candidates) {
##
##  get_unique_tokens <- function(x) unique(purrr::as_vector(tokenizers::tokenize_words(x)))
##  
##  get_scores <- function(token, phrase_tokens) {
##    purrr::keep(phrase_tokens, function(x) token %in% x)
##  }
##
##  tokens <- get_unique_tokens(candidates)
##  token_list <- tokenizers::tokenize_words(candidates)
##  
##  token_scores <- purrr::map(tokens, get_scores, phrase_tokens = token_list)
##  
##  df <- dplyr::data_frame(
##    freq = lengths(token_scores),
##    degree = purrr::as_vector(purrr::map(token_scores, function(x) sum(lengths(x))))
##  )
##  
##  df$degreeFreq <- df$degree / df$freq
##  
##  df$tokens <- tokens
##  
##  df
##} 
##
##
#
#
##innaug_15 <- quanteda::data_corpus_inaugural[1:5]
#
#
##immig <- quanteda::data_char_ukimmig2010
#
#
#
#
#get_token_scores <- function(candidate_phrase_set){
#  
#  tokens <- quanteda::tokenize(candidate_phrase_set)
#  
#  lengths <- lengths(tokens)
#  
#  vector <- purrr::as_vector(purrr::map(lengths, function(x) rep(x, x)))
#  
#  df <- data.frame(
#    tokens = purrr::as_vector(tokens),
#    degrees = vector
#    )
#  
#  df <- df %>% 
#    group_by(tokens) %>% 
#    summarise(
#      freq = n(),
#      degree = sum(degrees)) %>% 
#    mutate(degreeFreq = degree / freq)
#  
#  df
#  
#  }
#
##tokens_scores <- purrr::map(candidates, get_token_scores2)
#
##rakes <- rake(test_text)
##
#start <- Sys.time()
#rakes <- rake(quanteda::data_corpus_inaugural$documents$texts)
#finish <- Sys.time()
##
#finish - start
#
#rakes[57]
# innaug <- quanteda::data_corpus_inaugural$documents$texts
# 
# 
# scores <- rake(innaug[1:2])
# 
# candidates_test <- candidate_phrases(innaug[1:3])
# 
# tokens <- get_unique_tokens(candidates_test[[3]])
# token_list <- quanteda::tokenize(candidates_test[[3]])
# 
# get_scores <- function(token, phrase_tokens) {
#     purrr::keep(phrase_tokens, function(x) fastmatch::fmatch(token, x))
#   }
# 
# token_scores <- purrr::map(tokens, get_scores, phrase_tokens = token_list)
# 
# 
# dt <- data.table::data.table(as.matrix(quanteda::dfm(candidates_test[[3]])))
# 
# dt2 <- dt[first > 0, rowSums(), by = names(dt) ]
# 
# dt <- dt[, .(freq = .N, degree = sum(rowsums())),  ]
# 
# 
# ?data.table(# 