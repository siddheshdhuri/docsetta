library(stringr)
library(text2vec)
library(tm)
library(dplyr)

#' #####################################################################
#' Function to prepare text for analysis
#' 
#' @param text a text vector
#' @return text vector cleaned for text analysis
#' 
#' @export
prep_text = function(text) {
  text %>% 
      # make text lower case
      str_to_lower %>% 
      # remove non-alphanumeric symbols
      str_replace_all("[^[:alnum:]]", " ") %>% 
      # collapse multiple spaces
      str_replace_all("\\s+", " ")
}


#' #####################################################################
#' Function to get Document Term Matrix from document data.frame
#' 
#' @param document_df data.frame
#' @return dtm document term matrix
#' 
#' @export
getDocumentTermMatrix <- function(document_df, textcol, idcol) {
  
  #' single word tokenizer
  tok_fun = word_tokenizer
  
  #' iterator for text tokenization
  iter = itoken(document_df[[textcol]], 
                    preprocessor = prep_text, 
                    tokenizer = tok_fun, 
                    ids = document_df[[idcol]], 
                    progressbar = FALSE)
  
  #' remove stop words from vocabulary
  stop_words <- tm::stopwords("SMART")
  vocab = create_vocabulary(iter, stopwords = stop_words)
  
  #Prune vocabulary
  pruned_vocab = prune_vocabulary(vocab, 
                                  term_count_min = 5, 
                                  doc_proportion_max = 0.5,
                                  doc_proportion_min = 0.001)
  
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm <- create_dtm(iter, vectorizer)
  
  #' normalize Tf-idf
  dtm_tfidf <- normalizeDTM(dtm)
  
}

#' #####################################################################
#' function to TF-idf normalize dtm
#'
#' @param dtm document term matrix to be normalized
#'
#' @return Tf-idf normalized dtm
#'
#' @export
normalizeDTM <- function(dtm) {
  
  # define tfidf model
  tfidf = TfIdf$new()
  # fit model to train data and transform train data with fitted model
  dtm_tfidf = fit_transform(dtm, tfidf)
  
}

#' #####################################################################
#' function to compare two document term matrices
#'
#' @param dtm1 dtm2 document term matrices to be compared
#'
#' @return cosine similarity matrix
#'
#' @export
cosineSimilatiry <- function(dtm1, dtm2) { 

  d1_d2_cos_sim = sim2(dtm1, dtm2, method = "cosine", norm = "l2")
    
}
