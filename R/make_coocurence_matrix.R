examples <- readr::read_csv("Book1.csv")

candidates <- candidate_phrases(examples$`What Happened`)

stringr::str_c(candidates[[97]], collapse = " ")

examples$`What Happened`[1]
