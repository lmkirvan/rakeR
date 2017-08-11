test_text <- c("Compatibility of systems of linear constraints over the set of natural numbers
Criteria of compatibility of a system of linear Diophantine equations, strict inequations, and nonstrict inequations are considered. Upper bounds for components of a minimal set of solutions and algorithms of construction of minimal generating sets of solutions for all types of systems are given. These criteria and the corresponding algorithms for constructing a minimal supporting set of solutions can be used in solving all the considered types of systems and systems of mixed types.")

basic_punct <-function() {
    c("(?!['-])[[:punct:]]")
} 

stop_words <- function(){
  words <-  c("i've", "we've", "i", "me", "my", "myself", "we", "our", "ours", "ourselves", 
              "you", "your", "yours", "yourself", "yourselves", "he", "him", 
              "his", "himself", "she", "her", "hers", "herself", "it", "its", 
              "itself", "they", "them", "their", "theirs", "themselves", "what", 
              "which", "who", "whom", "this", "that", "these", "those", "am", "is", 
              "are", "was", "were", "be", "been", "being", "have", "has", "had", 
              "having", "do", "does", "did", "doing", "would", "should", "could",
              "ought", "i'm", "you're", "he's", "she's", "it's", "we're", "they're",
              "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", 
              "we'd", "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", 
              "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", 
              "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't",
              "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", 
              "what's", "here's", "there's", "when's", "where's", "why's", "how's", 
              "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", 
              "while", "of", "at", "by", "for", "with", "about", "against", "between",
              "into", "through", "during", "before", "after", "above", "below", "to",
              "from", "up", "down", "in", "out", "on", "off", "over", "under", "again",
              "further", "then", "once", "here", "there", "when", "where", "why", "how",
              "all", "any", "both", "each", "few", "more", "most", "other", "some", "such",
              "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "will",
              "like", "can")
  words
}

# only want to generate stopwords once when getting candidates from a vector
prep_stop_words <- function(split_words = stop_words(), split_punct = basic_punct()) {
  if (is.null(split_words) && is.null(split_punct)) {
    stop("Please provide a vector of stop words or punctuation or use provided stopwords")
  } 

  words <- stringr::str_c("\\b(", split_words, ")(?![\\w-])", sep = "")
  names <- c(words, split_punct)
  splits <- rep("*", length(names))
  
  purrr::set_names(splits, names)
}




