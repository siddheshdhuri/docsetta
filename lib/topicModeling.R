#'#######################################################################################
#' ################################ - TOPIC Modeling - ##################################
#'#######################################################################################


#'#####################################################################################
#' Function to get n gram tokens
#' 
#' @param text.vector vector of text to be tokenized
#' @param min minimum number of words in token
#' @param max maximum number of words in token
#' 
#' @return tdm TermDocumentMatrix
#' 
getNGramTokensTDM <- function(text.vector, min.len, max.len){
  
  text.vector <- as.character(text.vector)
  text.vector <- text.vector[!is.na(text.vector)]
  
  corpus = tm::Corpus(tm::VectorSource(text.vector))
  
  BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = min.len, max = max.len))
  
#   tdm <- tm::TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer,
#                                                    stopwords = TRUE))
  tdm <- tm::TermDocumentMatrix(corpus, control = list(stopwords = TRUE))
  
  return(tdm)
  
}


#'#####################################################################################
#' Function to get words tokenized by splitting
#' 
#' @param text.vector vector of text to be tokenized
#' 
#' @return word.vector
#' 
splitIntoTokens <- function(text.vector){
  
  
  text.vector <- gsub("'", "", text.vector)  # remove apostrophes
  text.vector <- gsub("[[:punct:]]", " ", text.vector)  # replace punctuation with space
  text.vector <- gsub("[[:cntrl:]]", " ", text.vector)  # replace control characters with space
  text.vector <- gsub("^[[:space:]]+", "", text.vector) # remove whitespace at beginning of documents
  text.vector <- gsub("[[:space:]]+$", "", text.vector) # remove whitespace at end of documents
  text.vector <- tolower(text.vector)  # force to lowercase
  
  #tokenize on space and output as a list:
  doc.list <- strsplit(text.vector, "[[:space:]]+")
  
  return(doc.list)
  
}



#'#######################################################################################
#' Function to get the Topic modelling model for further processing 
#' @param comments comments vectro
#' 
#' @return model topic modelling model developed using lda.collapsed.gibbs.sampler
#' 
getTopicModelandViz <- function(comments){
  
  comments[is.na(comments) | comments == "NA" | comments == "na"] <- ""
  
  stop_words <- tm::stopwords("SMART")
  stop_words <- c(stop_words,c("thing","things"))
  
  # Get n gram tokens from the comments
  tdm <- getNGramTokensTDM(comments,1,2)
  
  #' Remove words that occur across all documents too frequently
  removeCommonTerms(tdm, .80)
  
  tdm <- as.matrix(tdm)

  #Term and frequency table
  tdm.term.table <- as.array(rowSums(tdm))
  
  #Get words tokens by splitting
  doc.list <- splitIntoTokens(comments)
  
  # Term and Frequency table
  doc.term.table <- table(unlist(doc.list))
  
  #remove words common between n gram token and split word tokens
  common.words <- intersect(names(tdm.term.table), names(doc.term.table))
  del <- names(doc.term.table) %in% common.words
  doc.term.table <- doc.term.table[!del]
  
  # merge n gram token and split word tokens
  term.table <- c(tdm.term.table, doc.term.table)
  term.table <- sort(term.table, decreasing = TRUE)
  
  
  # remove terms that are stop words or occur fewer than 3 times:
  del <- names(term.table) %in% stop_words | term.table < 3
  term.table <- term.table[!del]
  vocab <- names(term.table)
  
  #' create documents lapplying createDocument function
  documents <- lapply(comments, createDocument, vocab = vocab)
  
  #' #############################################################
  #' fit model with lda
  #' #############################################################
  
  
  # Compute some statistics related to the data set:
  D <- length(documents)  # number of documents 
  W <- length(vocab)  # number of terms in the vocab 
  doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
  N <- sum(doc.length)  # total number of tokens in the data 
  term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus 
  
  
  # MCMC and model tuning parameters:
  K <- 5 # number of topics to generate
  G <- 10000 # number of iterations to compute (more = better quality and more time)
  alpha <- 0.02
  eta <- 0.02
  
  # Fit the model:
  library(lda)
  set.seed(666)
  
  fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                     num.iterations = G, alpha = alpha, 
                                     eta = eta, initial = NULL, burnin = 0,
                                     compute.log.likelihood = TRUE)
  
  
  # Create LDA Visualization
  lda.viz <- getTopicModelVisualization(model = fit, doc.length = doc.length, 
                                        vocab = vocab, term.frequency = term.frequency, 
                                        alpha = 0.02, eta = 0.02, comments)
  
  rtn.list <- list(model = fit, 
                   viz = lda.viz)
  
  return(rtn.list)
}


#'#######################################################################################
#' Function to get the taxonomy suggestions from a developed model
#' @param model topic modelling model developed using function getTopicModel()
#' 
#' @return topic_interpretation data.frame for topic and words suggestions
#' 
getTaxonomySuggestion <- function(model){
  
  top.words <- lda::top.topic.words(model$topics, 20, by.score=FALSE)
  options(stringsAsFactors = FALSE)
  #' prepare topic.and.words 
  topic_interpretation <- data.frame(words = apply(top.words, 2, function(x)paste(x, collapse = "; ")))
  topic_interpretation$topic_id <- 1:nrow(topic_interpretation)
  topic_interpretation$topic <- top.words[1,] # serves as initial topic name
  
  return(topic_interpretation)
  
}


# #############################################################
# visualize using LDAvis
# #############################################################
getTopicModelVisualization <- function(model, doc.length, vocab, term.frequency, alpha = 0.02, eta = 0.02, comments){
  
  theta <- t(apply(model$document_sums + alpha, 2, function(x) x/sum(x)))
  phi <- t(apply(t(model$topics) + eta, 2, function(x) x/sum(x)))
  
  comments.list<- list(phi = phi,
                       theta = theta,
                       doc.length = doc.length,
                       vocab = vocab,
                       term.frequency = term.frequency)
  
  
  library(LDAvis)
  
  # create the JSON object to feed the visualization:
  json <- createJSON(phi = comments.list$phi, 
                     theta = comments.list$theta, 
                     doc.length = comments.list$doc.length, 
                     vocab = comments.list$vocab, 
                     term.frequency = comments.list$term.frequency)
  
  # ser <- serVis(json, out.dir = 'visualization', open.browser = TRUE)
  
  return(json)
}


#'#######################################################################################
#' Function to prepare text by removing space and special charaters
#' @param comments vector
#' 
#' @return cleaned text
#' 
prepareText = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ") %>%
    #remove words shorter than 4 characters
    str_replace_all('\\b\\w{1,4}\\b','') %>%
    #remove worlds standalone numbers
    str_replace_all('\\b\\d+\\b','')
}

#'#######################################################################################
#' Function to get the taxonomy suggestions from a developed model
#' @param comments vector for the questionId
#' 
#' @return tax.suggestion topics suggestions table
#' 
getTopicsWordsAndViz <- function(comments){
  
  #' prepare text
  comments <- prepareText(comments)
  
  model.and.viz <- getTopicModelandViz(comments)
  
  topic.and.words <- getTaxonomySuggestion(model.and.viz$model)
  
  return(list(topics = topic.and.words,
              viz = model.and.viz$viz))
}


#'#######################################################################################
#' now put the documents into the format required by the lda package:
#' @param text.vector 
#' @param vocab 
#' 
#' @return document matrix
#' 
createDocument <- function(text.vector, vocab){
  
  match.counts <- stringr::str_count(text.vector,vocab)
  
  vocab.row <- 0:(length(vocab)-1)
  
  int.matrix <- rbind(as.integer(vocab.row), as.integer(match.counts))
  
  return(int.matrix)
}

#'#######################################################################################
#' Exclude words that are too common across all documents
#' @param x document term matrix 
#' @param pct percent threshold that the word occurs across documents 
#' 
#' @return document matrix
#' 
removeCommonTerms <- function (x, pct) {
  stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")), 
            is.numeric(pct), pct > 0, pct < 1)
  m <- if (inherits(x, "DocumentTermMatrix")) 
    t(x)
  else x
  t <- table(m$i) < m$ncol * (pct)
  termIndex <- as.numeric(names(t[t]))
  if (inherits(x, "DocumentTermMatrix")) 
    x[, termIndex]
  else x[termIndex, ]
}

########################################################################################
# Function to run a supervised LDA model
# @param corpus
# @param taxonomy
# 
# @return model
# 
get_topic_model_output <- function(comments_df, comments_col, taxonomy=NULL){
  
  # Create corpus
  corp <- quanteda::corpus(comments_df, text_field = comments_col)
  # remove html tags if any
  corp <- gsub("</?[^>]+>", "", corp)
  
  # tokenise text
  toks <- quanteda::tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE, remove_url = TRUE)
  toks <- quanteda::tokens_remove(toks, pattern = stopwords("en")) #remove stop words
  
  dfmt <- quanteda::dfm(toks) %>%
    quanteda::dfm_remove(stopwords('en'), min_nchar = 2) %>%
    quanteda::dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile",
             max_docfreq = 0.1, docfreq_type = "prop")
  
  if(is.null(taxonomy)){
    model <- seededlda::textmodel_lda(head(dfmt, 450), 6)
  }else{
    taxonomy <- data.tree::ToDataFrameTypeCol(taxonomy)
    taxonomy <- taxonomy %>% select(-level_1) %>% replace(is.na(.), "") %>% unite(terms, -level_2, sep = "|") %>% group_by(level_2) %>% summarize(terms = paste(terms, collapse = '|'))
    taxonomy <- split(unlist(stringi::stri_split_fixed(taxonomy$terms, pattern = "|", omit_empty = TRUE)), taxonomy$level_2)
    
    dict <- dictionary(taxonomy)
    
    model <- seededlda::textmodel_seededlda(dfmt, dict, residual = TRUE, min_termfreq = 10)
  }
  
  return_list = list(phi = model$phi, 
                     theta = model$theta, 
                     doc.length = ntoken(dfmt), 
                     vocab = featnames(dfmt), 
                     term.frequency = colSums(dfmt),
                     terms = seededlda::terms(model),
                     topics = seededlda::topics(model)
                     )
  
  return_list
  
}
