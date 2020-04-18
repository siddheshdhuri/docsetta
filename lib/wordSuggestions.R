#######################################################################################
###################################   - Word Suggestions  -   #########################
#######################################################################################

library(vwr)

getSuggestionsForWord <- function(word, corpus, taxonomy.terms) {
  
  #' make word lowercase
  word <- tolower(word)
  
  #' make taxonomy terms to lowercase
  taxonomy.terms <- tolower(taxonomy.terms)
  
  #' prepare text corpus for processing.
  corpus <-   corpus %>% 
      #' remove existing taxonomy terms from corpus
      str_replace_all(paste(paste0('\\b', taxonomy.terms ,'\\b'),collapse = '|'),'') %>%
      #' collapse multiple spaces, remove worders shorter than 4 char, standalone numbers
      #' function from topic modeling file
      prepareText
      #' get tokens from text corpus
      
  tokens <- splitIntoTokens(corpus) %>%
    unlist %>%
    unique
    
  #' remove taxonomy terms from tokens
  tokens <- setdiff(tokens,taxonomy.terms)
  
  #'get possible mispells
  neighbors <- vwr::levenshtein.neighbors(xsource = word, targets = tokens)
  
  #' select upto 4 neighbours
  closest.neighbors <- c(neighbors[[1]],neighbors[[2]],neighbors[[3]])[1:4]
  
  #' keep only words where the first character is same as word
  closest.neighbors <- closest.neighbors[substr(word, 1, 1) == substr(closest.neighbors, 1, 1)]
  
  return(closest.neighbors[!is.na(closest.neighbors)])
  
}