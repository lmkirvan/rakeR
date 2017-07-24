
candidates <- stringr::str_replace_all(test_text, splits, replacement = "*")
candidates

splits <- rep("^", length(splitters))

names(splits) <- splitters

short_words <- stopwords[nchar(stopwords) < 3 ]


tokenize(test_text, what = "sentence")
padded str_pad(stopwords, nchar(stopwords) + 2, side = "both")

