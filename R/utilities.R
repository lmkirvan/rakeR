test_text <- c("Compatibility of systems of linear constraints over the set of natural numbers
Criteria of compatibility of a system of linear Diophantine equations, strict inequations, and nonstrict inequations are considered. Upper bounds for components of a minimal set of solutions and algorithms of construction of minimal generating sets of solutions for all types of systems are given. These criteria and the corresponding algorithms for constructing a minimal supporting set of solutions can be used in solving all the considered types of systems and systems of mixed types.")

basic_punct <-function() {
  c("\\.", 
    "\\?",
    ",",
    "!",
    "\\\\",
    ":",
    ";",
    "\\n",
    "\\\\",
    "\\(",
    "\\)", 
    '"',
    "/",
    "$",
    "&",
    "@",
    "-",
    "\\:",
    "<",
    ">",
    "=",
    "~",
    "…",
    's/\\\\0//g',
    "\\r",
    "\\n",
    "\\$\\$",
    "[[:punct:]]")
} 

basic_words <- c(quanteda::stopwords(), "can", 'that’s', "will", "one", "two", "three", "four", "five", "six", "seven", 
                 "eight", "nine", "ten","eleven", "mother", "father", "wife", "husband", "grandmother", "grandfather",
                 "mothers", "fathers", "mother's", "father's", "together", "live", "us", "new", "ones", "per", "via")

basic_words <- basic_words[order(nchar(basic_words), decreasing = T)]

stop_words <- function(){
  basic_words
}


# tokenize with words and punctuation -------------------------------------

# only want to generate stopwords once when getting candidates from a vector
prep_stop_words <- function(split_words = stop_words(), split_punct = basic_punct()) {
  if (is.null(split_words) && is.null(split_punct)) {
    stop("Please provide a vector of stop words or punctuation or use provided stopwords")
  } 

  
  words <- stringr::str_c("\\b(", split_words, ")\\b", sep = "")
  names <- c(words, split_punct)
  splits <- rep("*", length(names))
  
  purrr::set_names(splits, names)
}




