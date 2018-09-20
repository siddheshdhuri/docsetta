shinyServer(function(input, output, session) {
  
  #updateTabItems(session, "tabsmenu", selected = "annotatetab")
  
  #' Hide Menubar by default
  addClass(selector = "body")
  
  reactive.values <- reactiveValues()
  
  #' reactive data.frame storing tweets
  reactive.values$tweets.df <- NULL
  
  #' reactive data.frame storing tweets data.frame
  # reactive.values$taxonomy.df <- data.frame()
  
  #' reactive data.frame taxonomy shiny.tree
  reactive.values$shiny.tree <- toShinyTreeList(tax.data.tree)
  
  #' reactive character vector of all taxonomy words
  reactive.values$taxwords <- character()
  
  reactive.values$selectedTweet <- data.frame()
  
  reactive.values$user.info <- NULL
  reactive.values$user.tweets <- NULL
  reactive.values$user.review.list <- c()
  
  
  ################################# - Load Data Page - #################################################
  
  observeEvent(input$resetData,{
    reactive.values$tweets.df <- global.comments.df
  })
  
  csvdata <- data.frame()
  output$contents <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$inputFile
    
    filedata <- NULL
    
    if (! is.null(inFile)){
      #' read in csv file
      filedata <- read.csv(inFile$datapath, header=input$header, sep=input$sep,
                           quote=input$quote, encoding = 'UTF-8', fileEncoding = 'ISO8859-1',
                           stringsAsFactors = FALSE)
      
      #' replace NA patterns with empty strings
      filedata <- as.data.frame(lapply(filedata, function(x) gsub(x,pattern = na.patterns, replacement = "")))
      
      #' get csv file column names to be mapped with framework template
      filecols <- colnames(filedata)
      #' set the csv column names in all the select inputs to map with framework template
      for(i in 1:length(columnnames)){
        updateSelectInput(session, columnnames[i], choices = c("None",filecols))
      }
      #' set csv global variable (this code needs to be redone)
      csvdata <<- filedata
      
    }
    #' else reading PDF files
    else if(!is.null(input$inputPDF)){
      
      #' get files selected in data.object
      filestoload <- input$inputPDF
      filestxt <- NULL
      numfiles <- nrow(filestoload)
      
      if(numfiles > 0) {
        
        withProgress(message = "in progress",{
          #' loop through each file and extact text.
          for( i in 1:numfiles) {
            #' update message 
            incProgress(1/numfiles, message = paste(round(100*i/numfiles,0)," % Complete"))
            #' read text from file
            txt <- pdftools::pdf_text(filestoload$datapath[i])
            txt <- paste(txt, collapse = " ")
            #' append text to files text vector
            filestxt <- c(filestxt, txt)
          }
          
        })
        
      }
      
      filedata <- data.frame(tweetid = seq(1:numfiles),
                            user = filestoload$name,
                            tweet = filestxt)
      reactive.values$tweets.df <- filedata
      
    }else{
      filedata <- reactive.values$tweets.df
    }
    #' trim the display text to 300 chars
    if(!is.null(filedata)) filedata$tweet <- paste(strtrim(filedata$tweet, 300),"...")
    
    return(filedata)
    
  })
  
  
  #'#############################################################################################
  #' useFile button uploads the file into the system for analysis
  #'
  observeEvent(input$useFile, {
    #'
    #'
    #'
    #'
    #'
    #' #%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%
    #' change this code to access data from data.table rather than setting global variable
    #' *********************************************************************************************
    #' 
    #' 
    #' 
    #' 
    #' 
    #' 
    
    tweettext <- as.character(csvdata[,input$comment])
    # replace &amp; with & symbol
    tweettext = gsub("&amp;", "&", tweettext)
    tweetid <- ifelse(input$ID == "None",seq(1:nrow(csvdata)),csvdata[,input$ID])
    tweetcreated <- ifelse(input$datecreated == "None","",csvdata[,input$datecreated])
    tweetlat <- ifelse(input$latitude == "None","",csvdata[,input$latitude])
    tweetlon <- ifelse(input$longitude == "None","",csvdata[,input$longitude])
    tweetuser <- ifelse(input$user == "None","",csvdata[,input$user])
    favCount <- 0
    retweetCount <- 0
    # assigning higher weight to retweet as it will proliferate trend
    reach <- 0
    
    
    tweets.df = data.frame(cbind(tweetid= tweetid, tweet=tweettext,tweetcreated=tweetcreated,
                                 lat=tweetlat,lon=tweetlon, user=tweetuser,
                                 favCount = favCount, retweetCount = retweetCount, reach = reach),
                           stringsAsFactors = FALSE)
    
    output$contents <- renderTable(head(tweets.df))
    
    reactive.values$tweets.df <- tweets.df
    
    #'set global comments df
    global.comments.df <<- tweets.df
  })
  
  #############################################################################
  #' data tables of previous searches
  output$previousSearches <- renderDataTable({
    
    files.df <- data.frame( Previous_Searches = list.files("data/discussions") )
    
    return(files.df)
    
  })
  
  #############################################################################
  #' observerEvent for the loadData action button
  observeEvent(input$loadData,{
    #' get the row selected files
    files.selected <- isolate(input$previousSearches)
    
    #'path to directory
    path <- "data/discussions/"
    
    files.to.load <- paste0(path,files.selected)
    
    #load for the first file
    tweets.df <- readRDS(files.to.load[1])
    
    #' if more than one file selected load data for other files
    i <- 2
    while(i <= length(files.to.load)){
      tweets.df <- rbind(tweets.df,
                         readRDS(files.to.load[i]))
      i <- i+1
    }
    
    # replace &amp; with & symbol
    tweets.df$tweet = gsub("&amp;", "&", tweets.df$tweet)
    
    #' if tweetid column is null set id column as 1 to num of documents
    if(is.null(tweets.df$tweetid)) tweets.df$tweetid <- 1:nrow(tweets.df)
    
    reactive.values$tweets.df <- tweets.df
    
  })
  
  
  ################################# - END Load Data Page - #################################################
  
  ################################# - Annotate Page - #################################################
  
  
  #'#############################################################################################
  #' Functionality for the annotate page
  #'
  output$annotate <- renderUI({
    
    tweets.df <- reactive.values$tweets.df
    
    if(nrow(tweets.df) < 1)
      return("No Tweets were found.")
    ia <- inputAnnotate("selectable", value = HTML(tagged_verbatim()))
    
    return(ia)
  })
  
  #'#############################################################################################
  #' Highlight selected text for display
  #'
  tagged_verbatim <- reactive({
    
    taxwords <- reactive.values$taxwords
    
    starts <- c()
    ends <- c()
    
    for(taxword in taxwords){
      
      tolocate <- paste0("\\b", stringr::str_trim(taxword),"\\b")
      locations <- data.frame(stringr::str_locate_all(currentVerbatim(),stringr::regex(tolocate,ignore_case=TRUE))[[1]])
      
      
      starts <- c(starts,(locations$start-1))
      ends <- c(ends,locations$end)
      
    }
    
    #modifiedText <- apply_insertions(currentVerbatim(), insertions = c("<mark><b>"= starts, "</b></mark>"= ends), tags.df = tagwords)
    modifiedText <- inputAnnotate::insert_tags(currentVerbatim(), insertions = c("<mark><b>"= starts, "</b></mark>"= ends))
    return(unique(modifiedText))
  })
  
  
  #'#############################################################################################
  #' Add button fucntionality to add annotated word to taxonomy
  #'
  observeEvent(input$add, {
    
    selected.word <- input$selected
    
    # Stop processing if selected word is empty
    if(nchar(selected.word) < 1) return(NULL)
    
    # check if word is already present in taxonomy
    if(length(intersect(selected.word, reactive.values$taxwords)) > 0) return(NULL)
    
    #Get if any item was checked
    #checked.item <- input$tagscheck
    selected.path <- getSelectedPath()
    
    #data.tree is pass by reference
    TaxonomyTree::addTerm(tax.data.tree, selected.word, selected.path)
    #temp(tax.data.tree, selected.word, selected.path)
    
    #update the tax.tree list
    reactive.values$shiny.tree <- toShinyTreeList(tax.data.tree)
    
    #update taxwords vector this will highlight words in the corpus
    reactive.values$taxwords <- as.character(tax.data.tree$Get('name'))
    
    #' get suggestion for word
    corpus <- paste(reactive.values$tweets.df$tweet, collapse = "")
    
    output$wordSuggestions <- renderUI({ 
      
      sug.list <- withProgress(message = "Finding suggestions", {
        
        similar.words <- getSuggestionsForWord(selected.word, corpus, isolate(reactive.values$taxwords))
        
        list(
          disabled(textInput("selected.word",label="",value = selected.word)),
          checkboxGroupInput("wordSuggestionsCheckbox", label = "", choices = similar.words, 
                             inline = FALSE, selected = similar.words),
          actionButton("addWordSuggestions", "Add")
        )
        
      })
      sug.list
      
    })
    
    output$asyncOutput <- renderText({ similar.words })
    
    #submit asynchronous job to identify suggestions
    if(input$add == 1){
      print("inside iffffff")
      # submitting new job for the first time
      
      #       Q.push(Q,{
      #         txt <- ""
      #         for(i in 1:8){
      #           Sys.sleep(1)
      #           txt <- paste(txt,i)
      #         }
      #         txt
      #       })
      
    }
    
    
  })
  
  
  
  #'#############################################################################################
  #' Delete button fucntionality to delete selected term(s) from taxonomy
  #'
  observeEvent(input$deleteTerm, {
    
    selected.path <- getSelectedPath()
    
    path.string <- paste0(root.name,"/",paste(selected.path,collapse = "/"))
    
    tax.data.tree$Prune(function(x) x$pathString != path.string)
    
    #update taxonomy list
    reactive.values$shiny.tree <- toShinyTreeList(tax.data.tree)
    
    #update taxwords vector
    reactive.values$taxwords <- as.character(tax.data.tree$Get('name'))
    
  })
  
  
  
  #'#############################################################################################
  #' function to filter comments for the selected topics
  #'
  observeEvent(input$viewTopicComments, {
    
    selected.nodes <- get_selected(input$tree)
    
    #' data.tree is pass by reference
    key.words.list <- TaxonomyTree::getSelectedNodeChildren(taxtree = tax.data.tree,
                                                            selected.nodes = selected.nodes)
    #' named characted list of all the words inside topics selected
    key.words <- unlist(key.words.list)
    
    #' find selected words in comments 
    #' add word boundaries between words
    pattern <- paste(key.words, collapse = "\\b|\\b")
    pattern <- paste0("\\b",pattern,"\\b")
    index <- grepl(pattern = pattern, reactive.values$tweets.df$tweet, ignore.case = T)
    
    #' keep only comments discussing about selected topics
    reactive.values$tweets.df <- reactive.values$tweets.df[index,]
    
  })
  
  
  #'#############################################################################################
  #' observer that updates the tree on input change.
  #'
  observe({
    
    jsInput = TaxonomyTree::toTreeJSON(reactive.values$shiny.tree)
    
    js$changeTree(jsInput)
    # 0.5 second gap is necessary due to asyncronous nature of javascript
    # i don't really know javascript so couldn't think of another solution
    # would appreciate a suggestion.
    
    delay(500,{
      js$open()
      js$deselect()
    })
  })
  
  
  #'#############################################################################################
  #' output box to show the selected text on the annotate page.
  #'
  observe({
    
    start <- input$selectable$start
    end <- input$selectable$end
    txt <- input$selectable$txt
    
    sel <- substr(txt, start, end)
    
    updateTextInput(session, "selected", value = sel)
    
  })
  
  output$selected2 <- renderText({
    
    start <- input$selectablepopup$start
    end <- input$selectablepopup$end
    txt <- input$selectablepopup$txt
    
    sel <- substr(txt, start, end)
    
    return("in here")
    
  })
  
  #' ###########################################################################
  #' observe Event for adding word suggestions to taxonomy
  #' 
  observeEvent(input$addWordSuggestions,{
    
    selected.word <- input$selected.word
    
    suggestions.to.add <- input$wordSuggestionsCheckbox
    
    
    #' Find Node that is selected.word
    node.selected <- FindNode(tax.data.tree,selected.word)
    
    for(suggestion in suggestions.to.add){
      node.selected$AddChild(suggestion)
    }
    #' add the selected word as a leaf
    node.selected$AddChild(selected.word)
    
    #update the tax.tree list
    reactive.values$shiny.tree <- toShinyTreeList(tax.data.tree)
    #update taxwords vector this will highlight words in the corpus
    reactive.values$taxwords <- as.character(tax.data.tree$Get('name'))
    
    
    output$wordSuggestions <- renderUI(NULL)
    
  })
  
  # Asynchronous job output
  #   output$asyncOutput <- renderText({
  # 
  #     invalidateLater(5000, session)
  # 
  #     job = Q.collect.all(Q)
  # 
  #     if(length(job)<1){
  #       return(" Still Waiting")
  #     }else{
  #       results <- ""
  #       for(i in 1:length(job)){
  #         results <- c(results,job[[i]]$value)
  #       }
  # 
  #       # submitting new async job in the queue
  #       Q.push(Q,{
  #         txt <- ""
  #         for(i in 1:8){
  #           Sys.sleep(1)
  #           txt <- paste(txt,i)
  #         }
  #         txt
  #       })
  # 
  #       return(results)
  #     }
  # 
  #   })
  
  #'#############################################################################################
  #' Display tree on initializaing
  #'
  output$tree <- shinyTree::renderTree({
    
    return(isolate(reactive.values$shiny.tree))
    
  })
  
  
  
  
  
  #'#############################################################################################
  #' observer event to save Taxonomy
  #'
  observeEvent(input$saveTaxonomy, {
    
    tax.data.tree <<- TaxonomyTree::fromShinyTreeList(input$tree)
    taxonomyname <- input$taxonomyName
    
    treedf <- ToDataFrameTable(tax.data.tree, "pathString", "term","freq")
    #treedf$term <- unlist(lapply(treedf$pathString, function(x) unlist(strsplit(x,"/"))[length(unlist(strsplit(x,"/")))] ))
    
    corpus <- paste(reactive.values$tweets.df$tweet,collapse = " ")
    
    treedf <- TaxonomyTree::computeFrequency(treedf, corpus)
    
    filename <- ifelse(grepl(".+\\.RDS$",taxonomyname),taxonomyname,paste0(taxonomyname,".RDS"))
    
    saveRDS(treedf,paste0("./taxonomies/",filename))
    
    updateSelectInput(session,"loadTaxonomy", label = "",
                      choices = c("--Select--",list.files("./taxonomies", pattern = ".RDS")))
    
    print("File Saved")
    
  })
  
  #'#############################################################################################
  #' observer when loadTaxonomy selection is changed
  #'
  observe({
    
    taxFileToLoad <- input$loadTaxonomy
    
    if("--Select--" == input$loadTaxonomy){
      
    }else{
      
      taxFile <- readRDS(paste0("./taxonomies/",taxFileToLoad))
      
      tax.data.tree <<- data.tree::FromDataFrameTable(taxFile, pathName = "pathString")
      
      #update taxonomy list
      reactive.values$shiny.tree <- toShinyTreeList(tax.data.tree)
      
      #update taxwords vector
      reactive.values$taxwords <- as.character(tax.data.tree$Get('name'))
      
      #update sunburst csv
      csv.df <- ToDataFrameTable(tax.data.tree, "pathString", "freq")
      csv.df$pathString <- gsub("/","-",csv.df$pathString)
      csv.df$pathString <- gsub(paste0(root.name,"-"),"",csv.df$pathString)
      
      write.table(csv.df,"./www/visit-sequences.csv", row.names = FALSE, col.names = FALSE, sep = ",")
      
    }
    
    #update save taxonomy text input
    updateTextInput(session, "taxonomyName", value = taxFileToLoad)
    
    
  })
  
  
  #'#############################################################################################
  #' Function to get the full path of the selected term
  #'
  #' @return vector of selected node and its ancestors
  getSelectedPath <- function(){
    
    x <- get_selected(input$tree)
    
    last.index <- length(x)
    
    if(length(x) > 0){
      leaf <- x[[last.index]]
      ancestors <- attr(leaf, "ancestry")
      
      return(c(ancestors,leaf))
    }
    else return(NULL)
    
  }
  
  
  ############################# - End Annotate - #################################################
  
  
  
  ############################## - Start Tweet Map - #############################
  #' Map
  
  output$map <- renderLeaflet({
    
    tweets.with.location <- reactive.values$tweets.df
    
    if (length(tweets.with.location) == 0)
      return(NULL)
    
    # Show only selected directions
    complete.coordinates <- complete.cases(tweets.with.location[,c("lon","lat")])
    
    if(length(complete.coordinates) < 1) return (NULL)
    
    tweets.with.location <- tweets.with.location[complete.coordinates,]
    
    radius <- (as.numeric(tweets.with.location$retweetCount) + as.numeric(tweets.with.location$favCount))
    radius <- scales::rescale(radius,to = c(1,100))
    
    tweets.with.location$lon <- as.numeric(tweets.with.location$lon)
    tweets.with.location$lat <- as.numeric(tweets.with.location$lat)
    
    map <- leaflet(tweets.with.location) %>%
      addTiles() %>%
      setView(lng = -0.0, lat = 51.5, zoom = 4) %>%
      addMarkers(lng = ~lon, lat= ~lat, layerId = ~tweetid, label = ~user)
    
    # map <- leaflet(tweets.with.location) %>%
    #   addTiles('http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png') %>%
    #   setView(lng = -0.0, lat = 51.5, zoom = 4) %>%
    #   addCircleMarkers(
    #     lng = ~lon,
    #     lat = ~lat,
    #     color = "blue",
    #     opacity = 0.8,
    #     radius = radius,
    #     layerId = ~tweetid
    #   )
    
    
    return(map)
  })
  # END MAP
  
  # When map is clicked, show a popup with tweet info
  observe({
    
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showTweetPopup(event$id, event$lat, event$lng)
    })
  })
  
  
  #'#################################################################################################
  #'Function to generate popup content when user clicks on map
  #'
  showTweetPopup <- function(tweetid, lat, lng) {
    
    isolate(tweets.df <- reactive.values$tweets.df)
    
    selectedTweet <- tweets.df[tweets.df$tweetid == tweetid,]
    
    content <- as.character(tagList(
      #tags$h4(selectedTweet$tweet),
      inputAnnotate("selectablepopup", selectedTweet$tweet),
      tags$strong(HTML(sprintf("%s",
                               selectedTweet$user
      ))),
      tags$br(),
      sprintf("Favourited: %s", selectedTweet$favCount), tags$br(),
      sprintf("Retweeted: %s", selectedTweet$retweetCount), tags$br()
    ))
    
    leafletProxy("map") %>% addPopups(lng, lat, content)
    
    reactive.values$selectedTweet <- selectedTweet
  }
  
  
  
  
  
  #############################- START Twitter Analysis #######################################
  
  ############################# - End Tweet Map - ###############################
  
  #' Function to get the text for current display
  #'
  currentVerbatim <- function(){
    
    page.num <- input$pagenum
    step.size <- input$pagesize
    
    #' update page numner numeric input max limit to nrow / step size
    updateNumericInput(session, "pagenum", max = nrow(reactive.values$tweets.df) / step.size)
    
    last.row <- page.num*step.size
    first.row <- last.row - (step.size - 1)
    
    last.row <- min(last.row, nrow(reactive.values$tweets.df))
    
    text <- reactive.values$tweets.df$tweet[first.row:last.row]
    
    display.text <- paste(text, collapse = "<br><hr><br>")
    
    return(display.text)
  }
  
  ############################## - Start Sentiment Analysis - ######################################
  
  output$sentimentplot <- renderDygraph({
    
    tweets.df <- reactive.values$tweets.df
    time.break <- input$chrono
    sentimentplot.data <- getSentimentAnalysis(tweets.df, time.break = time.break)
    
    dygraph(sentimentplot.data, main = "Sentiment Analysis") %>%
      dySeries("sentiment.data", drawPoints = TRUE, color = "blue", strokeWidth=3) %>%
      dySeries("freq.data", stepPlot = TRUE, fillGraph = TRUE, color = "green", axis = "y2", strokeWidth=2) %>%
      dyHighlight(highlightCircleSize = 5,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE) %>%
      dyRangeSelector()
    
  })
  
  
  #' plotting sentiment wordcloud
  #' 
  output$sentiment_cloud <- renderPlot({
    
    #' get tweets vector from the reactive tweets data.frame
    tweetsvector <- reactive.values$tweets.df$tweet
    #' prepare text by remove special charaters, punctuation, etc..
    tweetsvector <- prepareTextForAnalysis(tweetsvector)
    #' perform sentiment analysis and get data.frame
    sent.df <- getSentimentAnalysisDF(tweetsvector)
    
    output$sent_df <- renderTable(sent_df)
    
    getSentimentAnalysisWordCloud(sent_df)
    
  })
  
  
  #' scatter plot for sentiment analysis of tweets
  #' 
  output$sentimentScatterPlot <- renderPlot({
    
    sent_df <- reactive.values$tweets.df
    
    #' format tweetcreated to date time format
    sent_df$tweetcreated <- as.POSIXct(sent_df$tweetcreated, format="%Y-%m-%d %H:%M:%S",tz="UTC")
    
    #' evaluate tweets sentiments using glmnet model
    sent_df <- withProgress({
      setProgress(message = "Evaluating tweets with glmnet model..."
      )
      sentimentAnalysis::getSentimentAnalysisDF(df_tweets = sent_df)
    })
    
    sent_df_global <<- sent_df
    
    #' get tweets sentiments plot
    plot_sent <- withProgress({
      setProgress(message = "Plotting sentiments..."
      )
      sentimentAnalysis::getSentimentAnalysisPlot(sent_df_global)
    })
    
    return(plot_sent)
  })
  
  output$brushed_points <- renderTable({
    
    #' keep only brushed subset
    df <- brushedPoints(sent_df_global, input$sentimentPlot_brush)
    
    return(df)
    
  })
  
  
  ############################# - End Sentiment Analysis - #####################
  
  
  ############################## - Start Taxonomy Matrix Analysis - ############
  
  output$taxonomyMatrix <- DT::renderDataTable({
    #comments <- reactive.values$tweets.df$tweet
    
    #' function from taxonomyMatrix.R
    tax.matrix <- withProgress({
      setProgress(message = "Processing ...")
      
      non.zero.df <<- getDocTaxLongDF(tax.data.tree, reactive.values$tweets.df)
      
      updateSelectInput(session, "vertical", label = "Vertical", choices = colnames(non.zero.df), selected = "tweetid")
      updateSelectInput(session, "horizontal", label = "Horizontal", choices = colnames(non.zero.df), selected = "LEVEL1")
      
      docTermMatrix <- reshape2::dcast(data = non.zero.df, 
                                       formula = tweetid ~ LEVEL1, 
                                       value.var = "variable", 
                                       fun.aggregate = getFunction("paste.unique")
      )
      docTermMatrix
    })
    
    return(tax.matrix)
    
  })
  
  observe({
    
    dependent <- input$horizontal
    factors <- input$vertical
    
    print(dependent)
    print(factors)
    
    if(dependent == "") return(NULL)
    
    formula.var <- as.formula(paste( paste(factors,collapse = "+"), paste("~",dependent)))
    
    if("tweetid" %in% c(factors)) {
      docTermMatrix <- reshape2::dcast(data = non.zero.df, 
                                       formula = formula.var, 
                                       value.var = "variable", 
                                       fun.aggregate = getFunction("paste.unique")
      )
    }else{
      docTermMatrix <- reshape2::dcast(data = non.zero.df, 
                                       formula = formula.var, 
                                       value.var = "tweetid", 
                                       fun.aggregate = length
      )
    }
    
    output$taxonomyMatrix <- DT::renderDataTable({
      
      docTermMatrix
      
    })
    
  })
  
  
  
  ############################# - End Taxonomy Matric Analysis - ################
  
  
  ############################## - Start Taxonomy Pivot Analysis - ############
  
  output$taxPivot <- pivottabler::renderPivottabler({
    #comments <- reactive.values$tweets.df$tweet
    
    #' function from taxonomyMatrix.R
    tax.matrix <- withProgress({
      setProgress(message = "Processing ...")
      
      non.zero.df <<- getDocTaxLongDF(tax.data.tree, reactive.values$tweets.df)
      
      updateSelectInput(session, "pivot_rows", label = "Group Rows By",
                        choices = colnames(non.zero.df), selected = "LEVEL2")
      updateSelectInput(session, "pivot_columns", label = "Group Columns By", 
                        choices = colnames(non.zero.df), selected = "LEVEL1")
      
      pt$addData(non.zero.df)
      #pt$addColumnDataGroups(input$pivot_columns)
      #pt$addRowDataGroups(input$pivot_rows)
      pt$defineCalculation(calculationName="NumComments", summariseExpression="n()")
      pt$evaluatePivot()
      pivottabler(pt)
      
      
    })
    
    return(tax.matrix)
    
  })
  
  observeEvent(input$drawPivot,{
    
    pivot.cols <- input$pivot_columns
    pivot.rows <- input$pivot_rows
    
    pt <- PivotTable$new()
    pt$addData(non.zero.df)
    
    for(pivot.row in pivot.rows){
      pt$addRowDataGroups(pivot.row)
    }
    
    for(pivot.col in pivot.cols){
      pt$addColumnDataGroups(pivot.col)
    }
    
    pt$defineCalculation(calculationName="NumComments", summariseExpression="n()")
    
    pt$evaluatePivot()
    
    output$taxPivot <- pivottabler::renderPivottabler({
      pivottabler(pt)
    })
    
    
  })
  
  
  ############################# - End Taxonomy Pivot Analysis - ################
  
  
  
  ############################## - Start Taxonomy Heatmap Analysis - ############
  
  output$taxonomyHeatmap <- rCharts::renderChart2({
    
    #' function from taxonomyMatrix.R
    heatmapList <- withProgress({
      setProgress(message = "Processing ...")
      
      if(is.null(non.zero.df)) non.zero.df <<- getDocTaxLongDF(tax.data.tree, reactive.values$tweets.df)
      
      #' get wide data.frame of taxonomy and document mapping
      doc.tax.wide.df <- getDocTaxWideDF("LEVEL1",tax.data.tree, reactive.values$tweets.df, non.zero.df)
      
      #' get data.frame to plot
      
      #' start with first and second columns as x and y axes respectively
      xcol <- colnames(doc.tax.wide.df)[2]
      ycol <- colnames(doc.tax.wide.df)[4]
      
      heatmapList <- getHeatmapDF(doc.tax.wide.df, xcol, ycol)
      
      selectLevels <- colnames(non.zero.df)
      selectLevels <- selectLevels[! selectLevels %in% c("tweet","tweetid","value")]
      updateSelectInput(session, "select_level", label = "Select Level",
                        choices = selectLevels, selected = "LEVEL1")
      
      axesLevels <- colnames(doc.tax.wide.df)
      axesLevels <- axesLevels[!grepl("_FREQ|tweetid\\b",axesLevels, ignore.case = T)]
      updateSelectInput(session, "heatmap_xcol", label = "X Column",
                        choices = axesLevels, selected = xcol)
      updateSelectInput(session, "heatmap_ycol", label = "Y Column",
                        choices = axesLevels, selected = ycol)
      heatmapList
    })
    
    plot.data <- heatmapList$heatmap_df
    xvar.df <- heatmapList$xvar.df
    yvar.df <- heatmapList$yvar.df
    
    
    heatmap_plot <- Highcharts$new()
    heatmap_plot$chart(zoomType = "x", type = 'heatmap')
    
    heatmap_plot$series(name = 'Number of posts',
                        data = toJSONArray2(plot.data[,c("x","y","value")], json=FALSE),
                        color = "#cccccc",
                        dataLabels = list(
                          enabled = TRUE,
                          color = 'black',
                          style = list(
                            textShadow = 'none',
                            HcTextStroke = NULL
                          )
                        )
    )
    #print(xvar.df)
    heatmap_plot$xAxis(categories = xvar.df$xvar)
    #print(yvar.df)
    heatmap_plot$yAxis(categories = yvar.df$yvar,
                       title=list(text = ""))
    
    heatmap_plot$addParams(colorAxis =
                             list(
                               min = 0,
                               minColor='#FFFFFF',
                               maxColor='#7cb5ec'
                             )
    )
    
    # custom tooltip
    heatmap_plot$tooltip(formatter = "#! function() { return this.point.value + ' posts on <br>' +
                         '<b>' + this.series.xAxis.categories[this.point.x] + '</b> and <br>' +
                         '<b>' + this.series.yAxis.categories[this.point.y] + '</b>'; } !#")
    
    
    # set width and height of the plot and attach it to the DOM
    heatmap_plot$addParams(height = 400, width=1000, dom="heatmap")
    
    #' we need to print heatmap for it to be plotted appropriately.
    print(heatmap_plot)
    
})
  
  observeEvent(input$drawHeatmap,{
    
    selectedLevel <- input$select_level
    xcol <- input$heatmap_xcol
    ycol <- input$heatmap_ycol
    
    print(xcol)
    print(ycol)
    
    output$taxonomyHeatmap <- rCharts::renderChart2({
      
      #' function from taxonomyMatrix.R
      heatmapList <- withProgress({
        setProgress(message = "Processing ...")
        #' get wide data.frame of taxonomy and document mapping
        doc.tax.wide.df <- getDocTaxWideDF(selectedLevel,tax.data.tree, reactive.values$tweets.df)
        
        #' get data.frame to plot
        heatmapList <- getHeatmapDF(doc.tax.wide.df, xcol, ycol)
        heatmapList
      })
      
      plot.data <- heatmapList$heatmap_df
      xvar.df <- heatmapList$xvar.df
      yvar.df <- heatmapList$yvar.df
      
      heatmap_plot <- Highcharts$new()
      heatmap_plot$chart(zoomType = "x", type = 'heatmap')
      
      heatmap_plot$series(name = 'Number of posts',
                          data = toJSONArray2(plot.data[,c("x","y","value")], json=FALSE),
                          color = "#cccccc",
                          dataLabels = list(
                            enabled = TRUE,
                            color = 'black',
                            style = list(
                              textShadow = 'none',
                              HcTextStroke = NULL
                            )
                          )
      )
      
      heatmap_plot$xAxis(categories = xvar.df$xvar)
      
      heatmap_plot$yAxis(categories = yvar.df$yvar,
                         title=list(text = ""))
      
      heatmap_plot$addParams(colorAxis =
                               list(
                                 min = 0,
                                 minColor='#FFFFFF',
                                 maxColor='#7cb5ec'
                               )
      )
      
      # custom tooltip
      heatmap_plot$tooltip(formatter = "#! function() { return this.point.value + ' posts on <br>' +
                           '<b>' + this.series.xAxis.categories[this.point.x] + '</b> and <br>' +
                           '<b>' + this.series.yAxis.categories[this.point.y] + '</b>'; } !#")
      
      
      # set width and height of the plot and attach it to the DOM
      heatmap_plot$addParams(height = 400, width=1000, dom="heatmap")
      
      
      print(heatmap_plot)
      
  })
    
    
    
    
})
  
  
  ############################# - End Taxonomy Heatmap Analysis - ################
  
  ############################## - Start Load Taxonomy - ########################
  
  #'######################################################################################
  #' Observer to load taxonomy from CSV
  #'
  csvtax <- NULL
  observe({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$inputTaxonomyFile
    
    if (!is.null(inFile)){
      filedata <- read.csv(inFile$datapath, header=input$header, sep=input$sep,
                           encoding = 'UTF-8', fileEncoding = 'ISO8859-1',
                           stringsAsFactors = FALSE)
      
      
      # filedata <- as.data.frame(lapply(filedata, function(x) gsub(x,pattern = na.patterns, replacement = "")))
      filecols <- colnames(filedata)
      
      output$taxonomyTable <- renderTable(filedata)
      
      tax.file.col.names <- c("conceptSelectInput", "termSelectInput")
      
      csvtax <<- filedata
      
      for(i in 1:length(tax.file.col.names)){
        updateSelectInput(session, tax.file.col.names[i], choices = c("None",filecols))
      }
      
    }
  })
  
  
  observeEvent(input$useTaxonomyFile,{
    
    concept.col <- isolate(input$conceptSelectInput)
    term.col <- isolate(input$termSelectInput)
    
    tax.df <- csvtax[,c(concept.col,term.col)]
    
    output$taxonomyTable <- renderTable(head(tax.df))
    
    #' Create pathstring for converting data.frame to data.tree
    tax.df$pathString <- paste("Taxonomy",
                               tax.df[,concept.col],
                               tax.df[,term.col],
                               sep = "/")
    
    tax.tree <- data.tree::as.Node(tax.df)
    
    output$taxonomyView <- renderPrint(print(tax.tree, limit=20))
    
  })
  
  observeEvent(input$applyTaxonomy,{
    
    concept.col <- isolate(input$conceptSelectInput)
    term.col <- isolate(input$termSelectInput)
    
    tax.df <- csvtax[,c(concept.col,term.col)]
    
    #' Create pathstring for converting data.frame to data.tree
    tax.df$pathString <- paste("Taxonomy",
                               tax.df[,concept.col],
                               tax.df[,term.col],
                               sep = "/")
    
    tax.tree <- data.tree::as.Node(tax.df)
    
    #' set the global variable tax.data.tree as data.tree is pass by reference object
    tax.data.tree <<- tax.tree
    
    #'set the reactive varirable taxtree which holds the ShinyTree
    reactive.values$shiny.tree <- toShinyTreeList(tax.data.tree)
    
    #update taxwords vector this will highlight words in the corpus
    reactive.values$taxwords <- as.character(tax.data.tree$Get('name'))
    
  })
  
  
  ############################# - End Load Taxonomy - ###########################
  
  
  
  ############################## - Start Raw Data  - ############################
  
  output$datatable <- DT::renderDataTable({
    
    #       df <- full.data %>%
    #         mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '" data-zip="', AccountID, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #       action <- DT::dataTableAjax(session, df)
    
    df <- reactive.values$tweets.df
    
    DT::datatable(df)
    
  })
  
  ############################# - End Raw Data - ###############################
  
  
  
  ############################## - Start Analysis Page  - ###########################
  
  
  #'######################################################################################
  #'Taxonomy Network View
  #'
  output$taxonomyNetwork <- networkD3::renderDiagonalNetwork({
    
    #Add dependency to reactive data.tree
    reactive.values$shiny.tree
    
    useRtreeList <- ToListExplicit(tax.data.tree, unname = TRUE)
    
    dn <- diagonalNetwork(useRtreeList, fontSize = 10, opacity = 0.9)
    
    return(dn)
  })
  
  
  
  ########################################################################
  #' Visualize Taxonomy as a TreeMap
  #' 
  output$taxTreeMap <- renderText({
    
    # #Add dependency to reactive data.tree
    # reactive.values$shiny.tree
    
    tax.data.tree$Set(freq = c(function(self) sum(sapply(self$children,
                                                         function(child) GetAttribute(child, "freq", format = identity)))),
                      filterFun = isNotLeaf)
    
    df <- ToDataFrameNetwork(tax.data.tree, "freq")
    
    df <- paste.df(df, c(NA,root.name,sum(df$freq)))
    
    df$freq <- as.numeric(df$freq)
    
    #compute variable for color parameter
    df$freq.log=log(df$freq)
    df$freq.log[df$freq.log == -Inf] <- 0
    
    #df$to <- gsub("Taxonomy/","",df$to)
    #df$from <- gsub("Taxonomy/|Taxonomy","",df$from)
    
    xx <- gvisTreeMap(df, "to", "from", "freq", "freq",
                      options=list(width="100%", height=500,
                                   fontSize=10,
                                   minColor='#EDF8FB',
                                   midColor='#66C2A4',
                                   maxColor='#006D2C',
                                   headerHeight=20,
                                   fontColor='black',
                                   showScale=TRUE))
    
    return(xx$html$chart)
    
  })
  
  ############################# - End Analysis Page - ###############################
  
  
  
  ############################# - Start Entity Extraction - #########################
  
  
  output$personsValueBox <- renderValueBox({
    
    text <- reactive.values$tweets.df$tweet %>%
      paste(collapse = " ")
    
    people <- places <- orgs <- NULL
    withProgress({
      people <- getEntities(text, 'person')
      people <- as.data.frame(table(people))
      people <- people[order(people$Freq, decreasing = T),]
      
      places <- getEntities(text, 'location')
      places <- as.data.frame(table(places))
      places <- places[order(places$Freq, decreasing = T),]
      
      orgs <- getEntities(text, 'organization')
      orgs <- as.data.frame(table(orgs))
      orgs <- orgs[order(orgs$Freq, decreasing = T),]
    }, message = "Extracting Entities")
    
    
    #' set places
    output$placesValueBox <- renderValueBox(
      valueBox(
        nrow(people), "Places Found", icon = icon("globe"),
        color = "yellow"
      )
    )
    
    #' set organizations
    output$organizationsValueBox <- renderValueBox(
      valueBox(
        nrow(orgs), "Organizations Found", icon = icon("bank"),
        color = "blue"
      )
    )
    
    output$peopleDonut <- plotly::renderPlotly({
      #'keep top 30
      peoplePie <- people[1:min(nrow(people),30),]
      p <- peoplePie %>%
        plot_ly(labels = ~people, values = ~Freq) %>%
        add_pie(hole = 0.3)
    })
    output$peopleExtracted <- DT::renderDataTable(people)
    
    output$placesDonut <- plotly::renderPlotly({
      #'keep top 30
      placesPie <- places[1:min(nrow(places),30),]
      p <- placesPie %>%
        plot_ly(labels = ~places, values = ~Freq) %>%
        add_pie(hole = 0.3)
    })
    output$placesExtracted <- DT::renderDataTable(places)
    
    output$orgsDonut <- plotly::renderPlotly({
      #'keep top 30
      orgsPie <- orgs[1:min(nrow(orgs),30),]
      p <- orgsPie %>%
        plot_ly(labels = ~orgs, values = ~Freq) %>%
        add_pie(hole = 0.3)
    })
    output$organizationsExtracted <- DT::renderDataTable(orgs)
    
    
    #' set people
    valueBox(
      nrow(people), "People Found", icon = icon("user-circle"),
      color = "teal"
    )
    
  })
  
  
  ############################# - End Entity Extraction - ###########################
  
  
  
  ############################# - Start LDA Topic Modeling - #########################
  
  observeEvent(input$learnTopics,{
    
    #' get current text vector
    text <- reactive.values$tweets.df$tweet
    
    #
    # Launch doFork in the asyncProcssor.R
    #
    doFork(
      refreshRateSeconds = 10,
      maxTimeSeconds = 20000, 
      expr = {
        #
        # Here goes the code to evaluate in the fork
        #
        
        # Pass a list to the message function. 
        doForkMessage(
          list(
            text="Please wait"
          )
        )
        
        # Do something expensive
        #
        corpus <- paste(reactive.values$tweets.df$tweet, collapse = " ")
        topicsandviz <- getTopicsWordsAndViz(corpus)
        
        # Return something
        return(topicsandviz)
        
      },
      onMessage = function( msg ){
        #
        # This function will handle the messages send during computation
        #
        updateActionButton(session,
                           inputId="learnTopics",
                           label=msg$text
        )
      },
      onFeedback = function( result ){
        #
        # This function will handle the results (result$data)
        #
        
        #' get suggestion from model
        tax.suggestions <- result$data$topics
        suggestion.list <- getLDASuggestionUIComponents(tax.suggestions)
        #' display suggestion 
        output$suggestedTopics <- shiny::renderUI({suggestion.list})
        
        #' display the LDA visualuzation graph
        visual <- result$data$viz
        output$ldaviz <- LDAvis::renderVis({ visual })
        
        output$notificationMenu <- renderMenu({
          dropdownMenu(type = "notifications", notificationItem(
            text = "Topics available",
            icon = icon("exclamation-triangle"),
            status = "success"
          ))
        })
        
        
        
        updateActionButton(session,
                           inputId="learnTopics",
                           label="Click here"
        )
      })
    
    
  })
  
  
  #' ###########################################################################
  #' render topics and words as textbox and checkboxes
  #'
  
  
  ##############################################################################
  #Observe for all buttons on the Topic suggestions
  
  
  observe({
    
    #' javascript script.js changes addToTax buton to clickedbutton once its is clicked
    if(is.null(input$clickedbutton)){
      return(NULL)
    }
    
    # get index number of the button clicked
    button.clicked <- stringr::str_sub(input$clickedbutton,-1)
    button.clicked <- as.numeric(button.clicked)
    
    #' create names of the dyname concept textbox and checkboxes
    concept.box <- paste0("taxname",button.clicked)
    terms.checkbox <- paste0("tax",button.clicked)
    
    concept <- input[[concept.box]]
    terms <- input[[terms.checkbox]]
    
    terms <- setdiff(terms,concept)
    
    #' data.tree is pass by reference
    #' Add the concept term to taxonomy tree
    TaxonomyTree::addTerm(tax.data.tree, concept)
    
    #' add the checked items as childred of the
    for(term in terms){
      #' data.tree is pass by reference
      TaxonomyTree::addTerm(tax.data.tree, term, concept)
    }
    
    #' update the tax.tree list
    reactive.values$shiny.tree <- toShinyTreeList(tax.data.tree)
    
    #' update taxwords vector this will highlight words in the corpus
    reactive.values$taxwords <- as.character(tax.data.tree$Get('name'))
    
    
  })
  
  
  
  ############################# - End LDA Topic Modeling - #########################
  
  ############################# - Start Taxonomy Word Suggestions - #########################
  
  output$similarWordSuggestions <- renderUI({
    
    #' find suggestions  from document for selected word
    output$wordSuggestionsMenu <- renderMenu({
      menuItem("Word Suggestions", icon = icon("lightbulb-o"), tabName = "word_suggestions_page",
               badgeLabel = "New", badgeColor = "green"
      )
    })
    
    list(
      
      fluidRow(
        column(2,
               disabled(
                 shiny::textInput("suggestions_for", label = "", value = selected.word)
               )
        ),
        column(8,
               checkboxGroupInput("wordSuggestionsCheckbox", label = "", choices = similar.words, 
                                  inline = TRUE, selected = similar.words)
        ),
        column(2,
               actionButton("addWordSuggestions", "Add")
        )
        
      )
      
    )
    
  })
  
  ############################# - End Taxonomy Word Suggestions - ###########################
  
  
  ################################## - Start Influencer Mine - ##############################
  
  output$userDonut <- plotly::renderPlotly({
    
    userdf <- reactive.values$tweets.df$user %>%
      table() %>%
      as.data.frame()
    
    colnames(userdf) <- c("User","Freq")
    userdf <- userdf[order(userdf$Freq,decreasing = T),]
    userdf <- userdf[1:50,]
    
    #userdf <- userdf[userdf$Freq > quantile(userdf$Freq, 0.25), ]
    
    p <- userdf %>%
      #group_by(user) %>%
      #summarize(count = n()) %>%
      plot_ly(labels = ~User, values = ~Freq, height="700px" ,colors = "Blues") %>%
      add_pie(hole = 0.3) %>%
      layout(title = "Influencers in this discussion",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  ################################## - Start Influencer Mine - ##############################
  
  ################################## - Start Tags Mine - ##############################
  
  output$hashtagDonut <- plotly::renderPlotly({
    
    hashtags.tab <- reactive.values$tweets.df$tweet %>%
      str_extract_all("#[:alnum:]+") %>%
      unlist() %>%
      table()
    
    #' removing the most frequest term as this is the search term itself
    hashtags <- names(hashtags.tab[order(hashtags.tab,decreasing = TRUE)])[-1]
    
    hashtags.tab <- as.data.frame(hashtags.tab)
    
    colnames(hashtags.tab) <- c("Hashtag","Freq")
    hashtags.tab <- hashtags.tab[order(hashtags.tab$Freq,decreasing = T),]
    hashtags.tab <- hashtags.tab[1:50,]
    
    p <- hashtags.tab %>%
      #group_by(user) %>%
      #summarize(count = n()) %>%
      plot_ly(labels = ~Hashtag, values = ~Freq, height="700px" ,colors = "Blues") %>%
      add_pie(hole = 0.3) %>%
      layout(title = "#Hashtags in this discussion",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  ################################## - End Tags Mine - ##############################
  
  
  ################################## - Start Document Comparison - ##############################
  
  #'##############################################################
  #' Function to observe compareDocs action button event
  #' 
  observeEvent(input$compareDocs, {
    
    withProgress(message="Creating Heatmap...", {
      
      tweets.df <- reactive.values$tweets.df
      
      if(input$compareDocsBy == "User") {
        
        tweets.df <- tweets.df %>%
          select(user, tweet) %>%
          group_by(user) %>%
          summarise(doc = paste(tweet,collapse=" "))
        
      }else{
        
      }
      
      print(colnames(tweets.df))
      
      #' function from LatentSematicAnalysis.R lib file
      dtm <- getDocumentTermMatrix(tweets.df, "doc", "user")
      
      doc_similarity <- cosineSimilatiry(dtm, dtm)
      
      #' set diagnol to zero
      diag(doc_similarity) <- 0
      
      #' draw heatmap
      output$docHeatmap <- renderPlotly({
        
        doc_similarity <- as.matrix(doc_similarity)
        
        x <- heatmaply(doc_similarity, dendrogram="none", colors = Blues(10)) %>% layout(margin = list(l = 130, b = 40))
      })
      
    })
    
  })
  
  
  ################################## - End Document Comparison - ##############################
  
  
})
