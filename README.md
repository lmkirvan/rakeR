---
output:
  html_document: default
  pdf_document: default
---
# rakeR
an implimentation of rapid automatic keyword extraction 

This is an R implementation of the algorithm as mentioned in paper [Automatic keyword extraction from individual documents by Stuart Rose, Dave Engel, Nick Cramer and Wendy Cowley](https://www.researchgate.net/publication/227988510_Automatic_Keyword_Extraction_from_Individual_Documents)

At the document level, documents are tokenized using a list of stopwords. Each token is scored by counting the frequency of all co-occuring words (note that words co-occur with themselves for the purpose of this algorithm, this is referred to as degree) and dividing the by overall frequency of the words. Phrases are then scored by summing each tokens score. Additionally a function is provided to enrich an existing stop word list by looking across all the keywords for a group of documents. 





