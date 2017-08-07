library(tm)
require(httr)
require(XML)
library(tidyr)


#setwd("/Users/sid/Projects/twittermkt")

getWordLemma <- function(word){
  
  url <- "http://devadorner.northwestern.edu/maserver/lemmatizer"
  
  try({
    
    response <- GET(url,query=list(spelling=word,standardize="",
                                   wordClass="",wordClass2="",
                                   corpusConfig="ncf",    # Nineteenth Century Fiction
                                   media="xml"))
    
  })
  
  
  content <- content(response,type="text")
  
  xml     <- xmlInternalTreeParse(content)
  
  lemma <- xmlValue(xml["//lemma"][[1]])
  
  return(lemma)
}


getMisspelTable <- function(answers, words.in.ans, num.misspell.suggests){
  
  taxonomy.dict <- readLines("~/a2qda/app_survey_analyzer/utils/manual_dictionary.txt")
  full.dict <- c(qdapDictionaries::GradyAugmented, taxonomy.dict)
  
  misspel.table <- which_misspelled(answers, suggest = T, 
                                    n.suggests = num.misspell.suggests, 
                                    dictionary = full.dict)
  
  single.correction <- misspel.table[,c(2,3)]
  colnames(single.correction) <- c("words", "corrections") 
  
  more.corrections <- misspel.table[,4]
  
  for(i  in 1:nrow(misspel.table)){
    
    word <- single.correction[i,1]
    
    corrections = more.corrections[[i]][1:(num.misspell.suggests-1)]
    
    for(correction in corrections){
      if(any(grepl(correction, words.in.ans))){
        
        correction.row <- data.frame(words = word, corrections = correction)
        
        single.correction <- rbind(single.correction,correction.row)
      }
    }
  }
  
  triple.corrections <- single.correction[order(single.correction$words),]
  
  return(triple.corrections)
  
}



getWordLemmaTable <- function(questionId, answer_vector, num.misspell.suggests){
  
  answers <- paste(answer_vector, collapse = " ")
  
  
  
  words <- stringr::str_split(answers,'[^[:alpha:]]')[[1]]
  #words <- stringr::str_split(answers,'[[:space:]]|[\\.;:\\"\\(\\)/')[[1]]

  words <- unique(words)
  
  words <- words[nchar(words) > 3]
  
  # Get mispelled words table
  misspel.table <- getMisspelTable(answers, words, num.misspell.suggests)
  
  words.correction.table <- merge(x = as.data.frame(words), y = misspel.table, by = "words", all = TRUE)
  
  words.correction.table <- unique(words.correction.table)
  
  to.lemmatize <- words.correction.table[,1]
  
  to.lemmatize[which(complete.cases(words.correction.table))] <- words.correction.table[which(complete.cases(words.correction.table)),2]
  
  lemmas <- sapply(to.lemmatize,FUN = getWordLemma)
  
  word.lemma.table <- as.data.frame(cbind(words.correction.table, lemmas))
  word.lemma.table$questionId <- questionId
  
  return(word.lemma.table)
  
}


getWordsForLemma <- function(word, word.lemma.table){
  
  lemmas <- word.lemma.table[(tolower(word) == tolower(word.lemma.table$words)),]$lemmas
  
  similar.words <- c()
  #correction.words <- c()
  for(lemma in lemmas){
    similar.word <- word.lemma.table[(tolower(lemma) == tolower(word.lemma.table$lemmas)),]$words
    similar.words <- c(similar.words,similar.word)
    
    #correction.word <- word.lemma.table[(tolower(lemma) == tolower(word.lemma.table$lemmas)),]$corrections
    #correction.words <- c(correction.words,correction.word)
  }
  
  
  #return(list(similar.words = similar.words, correction.words = correction.words))
  
  return(similar.words)
}





xlsx.addTitle<-function(sheet, rowIndex, title){
  rows <- getRows(sheet,rowIndex=rowIndex)
  titlecell <-getCells(rows, colIndex=2)
  setCellValue(titlecell[[1]], title)
}

xlsx.setHeaderStyle <- function(wb){
  
  sheets <- getSheets(wb)
  
  TITLE_STYLE <- CellStyle(wb)+ 
                        Fill(backgroundColor="gray", foregroundColor="gray",
                        pattern="SOLID_FOREGROUND") +
                        Font(wb,  heightInPoints=10, color="white", isBold=TRUE)
  
  for(each.sheet in sheets){
    rows <- getRows(each.sheet, rowIndex=1)
    headerCells <- getCells(rows)
    for(each.cell in headerCells){
      setCellStyle(each.cell,TITLE_STYLE)
    }
    
  }
  
}



getTopicModel <- function(answers){
  
  stop_words <- stopwords("SMART")
  
  # pre-processing:
  answers <- gsub("'", "", answers)  # remove apostrophes
  answers <- gsub("[[:punct:]]", " ", answers)  # replace punctuation with space
  answers <- gsub("[[:cntrl:]]", " ", answers)  # replace control characters with space
  answers <- gsub("^[[:space:]]+", "", answers) # remove whitespace at beginning of documents
  answers <- gsub("[[:space:]]+$", "", answers) # remove whitespace at end of documents
  answers <- tolower(answers)  # force to lowercase
  
  # tokenize on space and output as a list:
  doc.list <- strsplit(answers, "[[:space:]]+")
  
  # compute the table of terms:
  term.table <- table(unlist(doc.list))
  term.table <- sort(term.table, decreasing = TRUE)
  
  # remove terms that are stop words or occur fewer than 5 times:
  del <- names(term.table) %in% stop_words | term.table < 5
  term.table <- term.table[!del]
  vocab <- names(term.table)
  
  # now put the documents into the format required by the lda package:
  get.terms <- function(x) {
    index <- match(x, vocab)
    index <- index[!is.na(index)]
    rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
  }
  documents <- lapply(doc.list, get.terms)
  
  
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
  G <- 500000 # number of iterations to compute (more = better quality and more time)
  alpha <- 0.02
  eta <- 0.02
  
  # Fit the model:
  library(lda)
  set.seed(666)
  #t1 <- Sys.time()
  fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                     num.iterations = G, alpha = alpha, 
                                     eta = eta, initial = NULL, burnin = 0,
                                     compute.log.likelihood = TRUE)
  #t2 <- Sys.time()
  #t2 - t1  # about 24 minutes on laptop
  
}

getTaxonomySuggestion <- function(model){
  #' #############################################################
  #' build taxonomy
  #' #############################################################
  
  top.words <- top.topic.words(model$topics, 10, by.score=TRUE)
  
  #' export to excel for interpretation
  topic_interpretation <- data.frame(words = apply(top.words, 2, function(x)paste(x, collapse = ";")))
  topic_interpretation$topic_id <- 1:nrow(topic_interpretation)
  topic_interpretation$topic <- top.words[1,] # serves as initial topic name
  
  return(topic_interpretation)
  
}



getSuggestionsFromTopicModel <- function(questionId, answers){
    
  model <- getTopicModel(answers)
  
  tax.suggestion <- getTaxonomySuggestion(model)
  print(nrow(tax.suggestion))
  tax.suggestion$questionId <- questionId
  
  return(tax.suggestion)
}


paste.df <- function(df, rows){
  options(stringsAsFactors = FALSE)
  
  col.names <- colnames(df)
  
  df <- rbind(df,rows)
  
  colnames(df) <- col.names
  
  return(df)
}


# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(textvector) {
  
  myCorpus = Corpus(VectorSource(textvector))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

########################################################################################
# custom wordcloud function to avoid NAMESPACE conflicts
#
getWordCloud <- function (words, freq, scale = c(4, 0.5), min.freq = 3, max.words = Inf, 
          random.order = TRUE, random.color = FALSE, rot.per = 0.1, 
          colors = "black", ordered.colors = FALSE, use.r.layout = FALSE, 
          fixed.asp = TRUE, ...) 
{
  if (!fixed.asp && rot.per > 0) 
    stop("Variable aspect ratio not supported for rotated words. Set rot.per=0.")
  tails <- "g|j|p|q|y"
  last <- 1
  nc <- length(colors)
  if (missing(freq)) {
    if (!require("tm")) 
      stop("freq must either be non-missing, or the tm package must be available")
    if (is.character(words) || is.factor(words)) {
      corpus <- Corpus(VectorSource(words))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, function(x) removeWords(x, 
                                                       stopwords()))
    }
    else corpus <- words
    tdm <- TermDocumentMatrix(corpus)
    freq <- slam::row_sums(tdm)
    words <- names(freq)
  }
  if (ordered.colors) {
    if (length(colors) != 1 && length(colors) != length(words)) {
      stop(paste("Length of colors does not match length of words", 
                 "vector"))
    }
  }
  if (min.freq > max(freq)) 
    min.freq <- 0
  overlap <- function(x1, y1, sw1, sh1) {
    if (!use.r.layout) 
      return(wordcloud:::.overlap(x1, y1, sw1, sh1, boxes))
    s <- 0
    if (length(boxes) == 0) 
      return(FALSE)
    for (i in c(last, 1:length(boxes))) {
      bnds <- boxes[[i]]
      x2 <- bnds[1]
      y2 <- bnds[2]
      sw2 <- bnds[3]
      sh2 <- bnds[4]
      if (x1 < x2) 
        overlap <- x1 + sw1 > x2 - s
      else overlap <- x2 + sw2 > x1 - s
      if (y1 < y2) 
        overlap <- overlap && (y1 + sh1 > y2 - s)
      else overlap <- overlap && (y2 + sh2 > y1 - s)
      if (overlap) {
        last <<- i
        return(TRUE)
      }
    }
    FALSE
  }
  ord <- rank(-freq, ties.method = "random")
  words <- words[ord <= max.words]
  freq <- freq[ord <= max.words]
  if (ordered.colors) {
    colors <- colors[ord <= max.words]
  }
  if (random.order) 
    ord <- sample.int(length(words))
  else ord <- order(freq, decreasing = TRUE)
  words <- words[ord]
  freq <- freq[ord]
  words <- words[freq >= min.freq]
  freq <- freq[freq >= min.freq]
  if (ordered.colors) {
    colors <- colors[ord][freq >= min.freq]
  }
  thetaStep <- 0.1
  rStep <- 0.05
  plot.new()
  op <- par("mar")
  par(mar = c(0, 0, 0, 0))
  if (fixed.asp) 
    plot.window(c(0, 1), c(0, 1), asp = 1)
  else plot.window(c(0, 1), c(0, 1))
  normedFreq <- freq/max(freq)
  size <- (scale[1] - scale[2]) * normedFreq + scale[2]
  boxes <- list()
  for (i in 1:length(words)) {
    rotWord <- runif(1) < rot.per
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    x1 <- 0.5
    y1 <- 0.5
    wid <- strwidth(words[i], cex = size[i], ...)
    ht <- strheight(words[i], cex = size[i], ...)
    if (grepl(tails, words[i])) 
      ht <- ht + ht * 0.2
    if (rotWord) {
      tmp <- ht
      ht <- wid
      wid <- tmp
    }
    isOverlaped <- TRUE
    while (isOverlaped) {
      if (!overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, 
                   ht) && x1 - 0.5 * wid > 0 && y1 - 0.5 * ht > 
          0 && x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) {
        if (!random.color) {
          if (ordered.colors) {
            cc <- colors[i]
          }
          else {
            cc <- ceiling(nc * normedFreq[i])
            cc <- colors[cc]
          }
        }
        else {
          cc <- colors[sample(1:nc, 1)]
        }
        graphics::text(x1, y1, words[i], cex = size[i], offset = 0, 
             srt = rotWord * 90, col = cc, ...)
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, 
                                        y1 - 0.5 * ht, wid, ht)
        isOverlaped <- FALSE
      }
      else {
        if (r > sqrt(0.5)) {
          warning(paste(words[i], "could not be fit on page. It will not be plotted."))
          isOverlaped <- FALSE
        }
        theta <- theta + thetaStep
        r <- r + rStep * thetaStep/(2 * pi)
        x1 <- 0.5 + r * cos(theta)
        y1 <- 0.5 + r * sin(theta)
      }
    }
  }
  par(mar = op)
  invisible()
}



getEntities <- function(text, kind){
  
  annotator <- Maxent_Entity_Annotator(kind = kind)
  
  pipeline <- list(sent_ann,
                   word_ann,
                   annotator)
  text <- as.String(text)
  annotations <- annotate(text, pipeline)
  
  annotated_doc <- AnnotatedPlainTextDocument(text, annotations)
  
  entities <- tryCatch({
    extractEntities(annotated_doc, kind)
  },error = function(e) {
    return("No entities found")
  })
  
  return(entities)
  
}



extractEntities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  
  if(hasArg(kind)) {
    
    k <- sapply(a$features, `[[`, "kind")
    
    s[a[k == kind]]
    
  } else {
    s[a[a$type == "entity"]]
  }
}



#################################################
# dynamic button that is used for Topic models page
# dynamic numner of topics are generated and this buttons 
# is used for adding selected topics and words to taxonomy

addToTax <- function(inputId, value = "Button") {
  tagList(
    singleton(tags$head(tags$script(src = "script.js"))),
    tags$button(id = inputId,
                class = "increment btn btn-default",
                type = "button",
                as.character(value))
  )
}






#'############################################################
#' Function to transform topics suggestions from LDA model
#' into ui components
#' 
getLDASuggestionUIComponents <- function(tax.suggestions) {
  
  #' list that will containt the UI components: textbox, checkboxes and button
  #' for selecting and adding topic words
  suggestions.list <- list()
  
  #' for every topic in the model create ui components
  for(i in 1:nrow(tax.suggestions)){
    
    topic <- unlist(tax.suggestions[i,3])
    
    words <- tax.suggestions[i,1]
    
    words <- unlist(strsplit(words, split = ";"))
    
    words <- words[words != "NA"]
    words <- words[!is.na(words)]
    
    
    check.boxes <- checkboxGroupInput(inputId =  paste0("tax",i), label =  NULL,
                                      choices =  words, inline = TRUE)
    
    topic.name <- textInput(inputId =  paste0("taxname",i), label =  NULL,
                            value = topic, width = 100)
    
    
    
    addButton <- addToTax(paste0("addToTax",i), "Add")
    
    
    suggestions.list[[i]] <- fluidRow(
      column(2,
             topic.name
      ),
      column(8,
             check.boxes
      ),
      column(2,
             addButton
      )
    )
    
  }
  
  return(suggestions.list)
} 


# submitAsyncJob <- function(selected.word, corpus){
#   
#   #
#   # Launch doFork in the asyncProcssor.R
#   #
#   doFork(
#     refreshRateSeconds = 10,
#     maxTimeSeconds = 20000, 
#     expr = {
#       #
#       # Here goes the code to evaluate in the fork
#       #
#       
#       # Pass a list to the message function. 
#       doForkMessage(
#         list(
#           text="Please wait"
#         )
#       )
#       
#       # Do something expensive
#       #
#       
#       similar.words <- getSuggestionsForWord(selected.word, corpus, reactive.values$taxwords)
#       
#       # Return something
#       return(topicsandviz)
#       
#     },
#     onMessage = function( msg ){
#       #
#       # This function will handle the messages send during computation
#       #
#       updateActionButton(session,
#                          inputId="learnTopics",
#                          label=msg$text
#       )
#     },
#     onFeedback = function( result ){
#       #
#       # This function will handle the results (result$data)
#       #
#       
#       #' get suggestion from model
#       tax.suggestions <- result$data$topics
#       suggestion.list <- getLDASuggestionUIComponents(tax.suggestions)
#       #' display suggestion 
#       output$suggestedTopics <- shiny::renderUI({suggestion.list})
#       
#       #' display the LDA visualuzation graph
#       visual <- result$data$viz
#       output$ldaviz <- LDAvis::renderVis({ visual })
#       
#       output$notificationMenu <- renderMenu({
#         dropdownMenu(type = "notifications", notificationItem(
#           text = "Topics available",
#           icon = icon("exclamation-triangle"),
#           status = "success"
#         ))
#       })
#       
#       
#       
#       updateActionButton(session,
#                          inputId="learnTopics",
#                          label="Click here"
#       )
#     })
#   
# }
