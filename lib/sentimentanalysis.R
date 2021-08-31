#######################################################################################
################################### Sentiment Analysis        #########################
#######################################################################################


getSentimentAnalysis <- function(texts.df, time.break = "day"){

  texts <- texts.df$text
  created <- texts.df$textcreated

  # Read the dictionary for valence computation
  dictionary <- read.csv("utils/dictionary.csv")
  #VALENCE: 1= sad, 9= happy
  #AROUSAL: 1=calm, 9=excited
  #DOMINANCE: 1=controlled, 9=in control

  #Now recode all columns so that neutral equals 0
  dictionary[,2:4] <- sapply(dictionary[,2:4],function(x) x-5)

  #'@TODO remove loop and vectorise function
  # calcualte valence score for each text
  scoretext <- numeric(length(texts))
  for (i in 1:length(texts)){

    textsplit <- tryCatch({

      strsplit(texts[i],split=" ")[[1]]

    },error = function(e){
        print(e)
    })

    #find the positions of the words in the text in the dictionary
    m <- match(textsplit, dictionary$Word)

    #which words are present in the dictionary?
    present <- !is.na(m)

    #of the words that are present, select their valence
    wordvalences <- dictionary$VALENCE[m[present]]

    #compute the total valence of the text
    scoretext[i] <- mean(wordvalences, na.rm=TRUE)
    
    if (is.na(scoretext[i])) scoretext[i] <- 0 else scoretext[i] <- scoretext[i]

  }

  #Group in minutes and take the average per hour / day / min
  #handle time zone
  created.time <- as.POSIXct(created, format="%Y-%m-%d %H:%M:%S",tz="UTC")
  attributes(created.time)$tzone <- "GMT"

  # time.breaks <- round.POSIXt(created.time, units=time.break)
  time.breaks <- lubridate::ceiling_date(created.time, unit=time.break)

  #' Set options as stringAsFactors = FALSE
  options(stringsAsFactors = FALSE)
  #paste text, sentiment and timebreak together into a dataframe
  sentiment.df <- as.data.frame(cbind(texts = texts, scoretext = as.numeric(scoretext),
                                      timebreak = as.character(time.breaks)))

  sentiment.df <- sentiment.df[!is.na(sentiment.df$timebreak),]
  time.breaks <- na.omit(time.breaks)

  # typecast scoretext as numeric.
  sentiment.df$scoretext <- as.numeric(sentiment.df$scoretext)

  # Aggregate the sentiment score over time breaks
  sentiment.agg <- aggregate(scoretext ~ timebreak,
                             data = sentiment.df,
                             mean,
                             na.action = na.omit)

  # Compute text frequency table
  text.freq <- table(sentiment.df$timebreak)

  #convert timebreak into POSIXlt time format
  sentiment.agg$timebreak <- as.POSIXlt(sentiment.agg$timebreak)
  sentiment.agg$text.freq <- text.freq

  # Getting sentiment data into time series data that can be used for plotting
  # create time_index to order data chronologically
  time_index <- seq(from = min(time.breaks),
                    to = max(time.breaks), by = time.break)

  if(nrow(sentiment.agg) != length(time.break)){
    #missing data so we need to interpolate using zoo
    # sentiment time series
    sentiment.ts <- zoo(sentiment.agg$scoretext, order.by = time_index)
    # frequnecy time series
    freq.ts <- zoo(sentiment.agg$text.freq, order.by = time_index)

  }else{
    # sentiment time series
    sentiment.ts <- xts(sentiment.agg$scoretext, order.by = time_index)
    # frequnecy time series
    freq.ts <- xts(sentiment.agg$text.freq, order.by = time_index)
  }

  #paste sentiment and freq time series
  plot.data <- cbind(sentiment.data = sentiment.ts,freq.data = freq.ts)

  return(plot.data)

}


classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("data/subjectivity.csv.gz",header=FALSE)
  
  counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(positive=0,negative=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      index <- pmatch(word,lexicon[,1],nomatch=0)
      if (index > 0) {
        entry <- lexicon[index,]
        
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        
        if (verbose) {
          print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
        }
        
        scores[[category]] <- scores[[category]]+score
      }		
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio==1) best_fit <- "neutral"
    documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
    if (verbose) {
      print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  
  colnames(documents) <- c("POS","NEG","POS/NEG","BEST_FIT")
  return(documents)
}



create_matrix <- function(textColumns, language="english", minDocFreq=1, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf) {
  
  stem_words <- function(x) {
    split <- strsplit(x," ")
    return(wordStem(split[[1]],language=language))
  }
  
  control <- list(language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stripWhitespace=stripWhitespace,minWordLength=minWordLength,stopwords=removeStopwords,minDocFreq=minDocFreq,weighting=weighting)
  
  if (stemWords == TRUE) control <- append(control,list(stemming=stem_words),after=6)
  
  trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
  trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")
  
  corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
  matrix <- DocumentTermMatrix(corpus,control=control);
  if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
  
  gc()
  return(matrix)
}

classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("data/emotions.csv.gz",header=FALSE)
  
  counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[,2]==key),]
        index <- pmatch(word,emotions[,1],nomatch=0)
        if (index > 0) {
          entry <- emotions[index,]
          
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          
          score <- 1.0
          if (algorithm=="bayes") score <- abs(log(score*prior/count))
          
          if (verbose) {
            print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
          }
          
          scores[[category]] <- scores[[category]]+score
        }
      }
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
  }
  
  colnames(documents) <- c("ANGER","DISGUST","FEAR","JOY","SADNESS","SURPRISE","BEST_FIT")
  return(documents)
}



# options(stringsAsFactors = FALSE)
# #Get the texts
# setwd("/Users/sid/Projects/twittermkt")
# texts <- readRDS("data/texts.RDS")
#
# text <- texts$text
# created <- texts$date
#
# #Group in minutes and take the average per hour / day / min
# #handle time zone
# time <- as.POSIXct(created, format="%Y-%m-%d %H:%M:%S",tz="UTC")
# attributes(time)$tzone <- "GMT"
#
# time.breaks <- round.POSIXt(time, units="days")
#
# sentiment.df <- as.data.frame(cbind(text = text, sentiment = scoretext,
#                                     timebreak = as.character(time.breaks)))
#
# sentiment.agg <- aggregate(scoretext ~ timebreak, data = sentiment.df, mean)
# text.freq <- table(sentiment.df$timebreak)
#
# sentiment.agg$timebreak <- as.POSIXlt(sentiment.agg$timebreak)
# sentiment.agg$text.freq <- text.freq
#
# time_index <- seq(from = min(time.breaks),
#                  to = max(time.breaks), by = "days")
# sentiment.data <- xts(sentiment.agg$scoretext, order.by = time_index)
# freq.data <- xts(sentiment.agg$text.freq, order.by = time_index)
#
# plot.data <- cbind(sentiment.data = sentiment.data,freq.data = freq.data)
#
# dygraph(plot.data, main = "Sentiment Analysis") %>%
#   dySeries("sentiment.data", drawPoints = TRUE, color = "blue", strokeWidth=3) %>%
#   dySeries("freq.data", stepPlot = TRUE, fillGraph = FALSE, color = "green", axis = "y2", strokeWidth=2)


#Assignment

#Up to now we have determined the sentiment of a text over time by looking at single words.
#These are called unigrams. We only looked at the valence. One could also determine the sentiment
#of a text over time by looking at combinations of two words. These are called bigrams.

#Determine the sentiment of a corpus of 500 texts. Once only based on unigrams and once
#only based on bigrams. Plot both curves in the same plot. Also compute the correlation between
#both curves.

#Do this for valence, arousal and dominance. In total you should have 3 plots with each two curves.

#To help you, here is how you extract bigrams from a string. You also need to change the dictionary.



#' Function to prepare text for sentiment analysis
#' removing special characters, punctuation, numbers links etc..
#'
#' @param textvector vector of text to perform sentiment analysis on
#'
#' @export textvector cleaned text vector
prepareTextForAnalysis <- function(textvector) {

  # remove & ampersand symbols
  textvector = gsub("&amp", "", textvector)
  # remove retweet entities
  textvector = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", textvector)
  # remove at people
  textvector = gsub("@\\w+", "", textvector)
  # remove punctuation
  textvector = gsub("[[:punct:]]", "", textvector)
  # remove numbers
  textvector = gsub("[[:digit:]]", "", textvector)
  # remove html links
  textvector = gsub("http\\w+", "", textvector)
  # remove unnecessary spaces
  textvector = gsub("[ \t]{2,}", "", textvector)
  textvector = gsub("^\\s+|\\s+$", "", textvector)

  # define "tolower error handling" function
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply
  textvector = sapply(textvector, try.error)

  # remove NAs in textvector
  textvector = textvector[!is.na(textvector)]
  names(textvector) = NULL

  return(textvector)

}


#' function to get sentiment data.frame from cleaned text vector
#'
#' @param textvector a text vetor with clened text data
#'
#' @export sent_df data.frame of text comments with sentiment analysis columns
getEmotionAnalysisDF <- function(textvector) {
  
  # classify emotion
  class_emo = classify_emotion(textvector, algorithm="bayes", prior=1.0)
  # get emotion best fit
  emotion = class_emo[,7]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] = "unknown"

  # classify polarity
  class_pol = classify_polarity(textvector, algorithm="bayes")
  # get polarity best fit
  polarity = class_pol[,4]


  # data frame with results
  sent_df = data.frame(text=textvector, emotion=emotion,
                       polarity=polarity, stringsAsFactors=FALSE)

  # sort data frame
  sent_df = within(sent_df,
                   emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

  return(sent_df)

}


#' function to get comparion wordcloud of sentiment analysis
#'
#' @param sent_df data.frame of sentiment analysed content with emotion
#' and polarity columns
#'
#' @export cloud comparison wordcloud to be plot
getSentimentAnalysisWordCloud <- function(sent_df){

  # separating text by emotion
  emos = levels(factor(sent_df$emotion))
  nemo = length(emos)
  emo.docs = rep("", nemo)
  for (i in 1:nemo)
  {
    tmp = sent_df[sent_df$emotion == emos[i], ]$emotion
    emo.docs[i] = paste(tmp, collapse=" ")
  }

  # remove stopwords
  emo.docs = removeWords(emo.docs, stopwords("english"))
  # create corpus
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos

  # comparison word cloud
  cloud <- comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                            scale = c(3,.5), random.order = FALSE, title.size = 1.5)

  return(cloud)
}






#' getSentimentAnalysisPlotly <- function(df_texts) {
#'   # color palette
#'   cols <- c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a")
#' 
#'   #' format textcreated to date time format
#'   #df_texts$textcreated <- as.POSIXct(df_texts$textcreated, format="%Y-%m-%d %H:%M:%S",tz="UTC")
#' 
#'   set.seed(932)
#'   samp_ind <- sample(c(1:nrow(df_texts)), nrow(df_texts) * 0.1) # 10% for labeling
#' 
#'   df_texts$retextCount <- as.numeric(df_tweets$retweetCount)
#'   df_tweets$favCount <- as.numeric(df_tweets$favCount)
#'   df_tweets$reach <- as.numeric(df_tweets$reach)
#'   
#'   sizevector <- scales::rescale(df_tweets$retweetCount,to = c(1,30))
#'   
#'   x <- list(
#'     tickangle=20
#'   )
#'   
#'   plot_ly(df_tweets, x = ~tweetcreated, y = ~sentiment, color = ~sentiment, size= sizevector,
#'                     type = "scatter", mode="marker", 
#'                     #colors = c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a"),
#'                     hoverinfo = 'text', height = 500, 
#'                     text = ~paste(paste(substr(tweet, 1,140),"..."),
#'                                   '<br> user: ', user)) %>%
#'   layout(xaxis = x)
#'   
#'   
#'   # plotting
#'   # x <- ggplot(df_tweets, aes(x = tweetcreated, y = sentiment, color = sentiment))
#'     # theme_minimal() +
#'     # scale_color_gradientn(colors = cols, limits = c(0, 1),
#'     #                       breaks = seq(0, 1, by = 1/4),
#'     #                       labels = c("0", round(1/4*1, 1), round(1/4*2, 1), round(1/4*3, 1), round(1/4*4, 1)),
#'     #                       guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
#'     # geom_point(aes(color = sentiment), alpha = 0.8) +
#'     # geom_hline(yintercept = 0.65, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
#'     # geom_hline(yintercept = 0.35, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
#'     # geom_smooth(size = 1.2, alpha = 0.2) +
#'     # ggrepel::geom_label_repel(data = df_tweets[samp_ind, ],
#'     #                           aes(label = round(sentiment, 2)),
#'     #                           fontface = 'bold',
#'     #                           size = 2.5,
#'     #                           max.iter = 100) +
#'     # theme(legend.position = 'bottom',
#'     #       legend.direction = "horizontal",
#'     #       panel.grid.major = element_blank(),
#'     #       panel.grid.minor = element_blank(),
#'     #       plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
#'     #       axis.title.x = element_text(size = 16),
#'     #       axis.title.y = element_text(size = 16),
#'     #       axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
#'     #       axis.text.x = element_text(size = 8, face = "bold", color = 'black')) +
#'     # ggtitle("Tweets Sentiment rate (probability of positiveness)")
#' 
#'   # return(x)
#' }
