fortify_stopwords <- function(x, 
                              stopwords = smart_stop_words(), 
                              n = .97,
                              sample_frac = 1){
  
  vec_to_count_df <- function(x){
    x <-table(x)
    data_frame(
      value = names(x),
      count = unname(x)
    )
  }
  
  len_x <- length(x)
  x <- x[sample(1:len_x, size = round(len_x * sample_frac))]
  
  phrases <- candidate_phrases(x, remove_numbers = T)
  
  tokens <- purrr::as_vector(tokenizers::tokenize_words(x))
  phrase_tokens <- purrr::as_vector(
    tokenizers::tokenize_words(
      purrr::flatten(phrases)
      )
    )
  
  tokens_df <- vec_to_count_df(tokens)
  phrase_tokens_df <- vec_to_count_df(phrase_tokens)
  
  final_df <- left_join(
    tokens_df, 
    phrase_tokens_df, 
    by = "value")
  
  final_df[is.na(final_df)] <- 0 
  final_df$adjacent <- final_df$count.x - final_df$count.y
  final_df$stop_word <- ifelse(final_df$adjacent > final_df$count.y, T, F)
  
  temp <- final_df %>% 
    filter(
      stop_word == T
    ) %>% 
    filter(count.x > quantile(count.x, n)) %>% 
    arrange(desc(count.x)) %>% 
    pull(value)
  
  union(stopwords, temp)
}





