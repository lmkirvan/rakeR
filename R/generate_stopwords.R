#' Improve an existing stopword list by finding stopwords using rake
#' 
#' This function takes an existing stopword list and then finds words that are
#' used frequently but appear infrequently in keyphrases. it then adds these
#' non-keywords to you stopword list. If the words are more frequently
#' adjacent to keywords than they are in keywords, they are selected. 
#' 
#' @export
#' @param x this is the vector of texts that you want to use to generate 
#'   additionaly stopwords
#' @param stopwords this is the list of stopwords you want to enrich
#' @param n is the percentage of the total number of words that you want to 
#'   consider when looking for common words. It ranges from 0 to 1 but 
#'   should always be set to a relatively high number to ensure that
#'   only commonly used words are added to the stop list
#' @param sample_frac this is the percentage of documents in x you want 
#'   to consider. Provided for big datasets. 
#' @return Returns a vector of fortified stopwords    
fortify_stopwords <- function(x, 
                              stopwords = smart_stop_words(), 
                              n = .97,
                              sample_frac = 1){
  
  vec_to_count_df <- function(x){
    x <-table(x)
    dplyr::data_frame(
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
  
  final_df <- dplyr::left_join(
    tokens_df, 
    phrase_tokens_df, 
    by = "value")
  
  final_df[is.na(final_df)] <- 0 
  final_df$adjacent <- final_df$count.x - final_df$count.y
  final_df$stop_word <- ifelse(final_df$adjacent > final_df$count.y, T, F)
  
  temp <- final_df %>% 
    dplyr::filter(
      stop_word == T
    ) %>% 
    dplyr::filter(count.x > quantile(count.x, n)) %>% 
    dplyr::arrange(desc(count.x)) %>% 
    dplyr::pull(value)
  
  union(stopwords, temp)
}






