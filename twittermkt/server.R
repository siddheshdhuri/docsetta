shinyServer(function(input, output, session) {
  
  #' Hide Menubar by default
  addClass(selector = "body", class = "sidebar-collapse")
  
  reactive.values <- reactiveValues()
  
  #' reactive data.frame storing tweets
  reactive.values$tweets.df <- data.frame()
  
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
  
  # reactive.values$word.suggestions <- NULL
  
  #   #' initializing Queue for parellel processing
  #   queue = Q.make()
  #   #' send local R object
  #   Q.sync(queue, getTopicsWordsAndViz)
  #   Q.sync(queue, prepareText)
  #   Q.sync(queue, getTopicModelandViz)
  #   Q.sync(queue, getTaxonomySuggestion)
  
  
  # Make the wordcloud drawing predictable during a session
  #wordcloud_rep <- repeatable(wordcloud)
  
  
  
  ################################# - Load Data Page - #################################################
  
  observeEvent(input$resetData,{
    reactive.values$tweets.df <- global.comments.df
  })
  
  
  csvdata <- data.frame()
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$inputFile
    
    if (! is.null(inFile)){
      filedata <- read.csv(inFile$datapath, header=input$header, sep=input$sep,
                           quote=input$quote, encoding = 'UTF-8', fileEncoding = 'ISO8859-1',
                           stringsAsFactors = FALSE)
      
      
      filedata <- as.data.frame(lapply(filedata, function(x) gsub(x,pattern = na.patterns, replacement = "")))
      
      
      filecols <- colnames(filedata)
      
      for(i in 1:length(columnnames)){
        updateSelectInput(session, columnnames[i], choices = c("None",filecols))
      }
      
      csvdata <<- filedata
    }else{
      filedata <- reactive.values$tweets.df
    }
    
    return(head(isolate(filedata)))
    
  })
  
  
  #'#############################################################################################
  #' useFile button uploads the file into the system for analysis
  #'
  observeEvent(input$useFile, {
    
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
  
  #'#############################################################################################
  #' go button searches for tweets on Twitter
  #'
  observeEvent(input$go,{
    
    options(encoding="utf-8")
    
    search.term <- input$search
    search.term <- gsub("[,;\\.]","|", search.term)
    no.of.tweets <- 100
    if(flag){
      
      #Get the tweets
      #       tweets <- searchTwitter(search.term, n=no.of.tweets,  lang="en",
      #                               geocode= '53.38,-3.02,700mi')
      
      tweets <- withProgress({
        setProgress(message = "Fetching Tweets..."
        )
        searchTwitter(search.term, n=no.of.tweets,  lang="en"
                      , geocode= '53.38,-3.02,700mi')
        
      })
      
      #' Remove retweets
      # tweets <- twitteR::strip_retweets(tweets, strip_manual = TRUE, strip_mt = FALSE)
      
      #create a clean UTF-8 encoding data.frame from the tweets object
      
      tweets.df <- withProgress({
        setProgress(message = "Getting Locations..."
        )
        twitterEx:::getTweetsDataFrame(tweets)
        
      })
      
      tweetlat = NA
      tweetlon = NA
      
      ######## - start code for Fork async process -###########
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
              text="Getting Locations for Users"
            )
          )
          
          # Do something expensive
          #
          result = tryCatch({
            
            gc <- twitterEx::getUserLocation(tweets.df$user)
            
            list(tweetlat=as.numeric(gc$lat),tweetlon=as.numeric(gc$lon))
            
          }, error = function(e) {
            print(e)
          })
          
          # Return something
          return(result)
          
        },
        onMessage = function( msg ){
          #
          # This function will handle the messages send during computation
          #output$map <- HTML("Fething User Location this can take long.. check in some time")
        },
        onFeedback = function( result ){
          print("Feedback Received")
          #
          # This function will handle the results (result$data)
          reactive.values$tweets.df$lat <- result$data$tweetlat
          reactive.values$tweets.df$lon <- result$data$tweetlon
          
        })
      
      ######## - end code for Fork async process -###########
      
      #' Add the searched term as a column
      tweets.df$search_term <- search.term
      #' save search as RDS
      filename = paste0(paste0("data/discussions/",search.term,"_"),gsub(":","",format(Sys.time(),"%d%b%Y_%X")),".RDS")
      saveRDS(tweets.df, filename)
      
      # tweets.df <- twitterEx:::getTweetsDataFrame(tweets)
      
    }
    #saveRDS(tweets.df, "data/tweets.RDS")
    
    if(!flag){
      tweets.df <- readRDS("data/discussions/brexit_17Jul2017_100142.RDS")
      
      # sqlite    <- dbDriver("SQLite")
      # exampledb <- dbConnect(sqlite,"data/newexample.db")
      # 
      # register_sqlite_backend("data/newexample.db")
      # #tweets.df <- load_tweets_db(as.data.frame = TRUE, table_name = "tweets")
      # tweets.df <- RSQLite::dbReadTable(exampledb,"tweets")
      
      tweets.df$reach <- (tweets.df$favCount + tweets.df$retweetCount)
      
      step.size <- input$pagesize
      
      max.lim <- ifelse(mod(nrow(tweets.df),step.size)==0,(nrow(tweets.df)/step.size),(nrow(tweets.df)/step.size)+1)
      
      updateNumericInput(session, "pagenum", value = 1, min = 1, max = max.lim, step = 1)
    }
    
    # replace &amp; with & symbol
    tweets.df$tweet = gsub("&amp;", "&", tweets.df$tweet)
    
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
    
    #' get the row selected on data.table
    rows.selected <- isolate(input$previousSearches_rows_selected)
    
    #' convert to numeric
    rows.selected <- as.numeric(rows.selected)
    
    #'get list of files already stored
    path <- "data/discussions/"
    files.list <- list.files(path)
    
    files.to.load <- files.list[rows.selected]
    
    #load data for selected rows
    tweets.df <- readRDS(paste0(path,files.to.load[1]))
    
    i <- 2
    while(i <= length(files.to.load)){
      tweets.df <- rbind(tweets.df,
                         readRDS(paste0(path,files.list[i])))
      i <- i+1
    }
    
    # replace &amp; with & symbol
    tweets.df$tweet = gsub("&amp;", "&", tweets.df$tweet)
    
    reactive.values$tweets.df <- tweets.df
    
  })
  
  
  
  #'#############################################################################################
  #' getPosts button gets Posts from website
  #'
  observeEvent(input$getPosts,{
    
    posts.df <- readRDS("data/MS_Scraped_Data.RDS")
    
    # posts.df <- posts.df[nchar(posts.df$post) > 200,]
    # posts.df <- posts.df[1:1000,]
    
    #Flag set to false while testing and development
    if(flag){
      #'Scrape posts from forum
      posts.df <- withProgress({
        setProgress(message = "Scraping Forum Posts...")
        scrapePosts(input$link.to.scrape)
      })
    }
    
    tweettext <- posts.df$post
    tweetid <- seq(1:nrow(posts.df))
    tweetcreated <- posts.df$post.time
    tweetlat <- ""
    tweetlon <- ""
    tweetuser <- posts.df$post.user
    favCount <- 0
    retweetCount <- 0
    reach <- 0
    
    tweets.df = data.frame(cbind(tweetid= tweetid, tweet=tweettext,tweetcreated=tweetcreated,
                                 lat=tweetlat,lon=tweetlon, user=tweetuser,
                                 favCount = favCount, retweetCount = retweetCount, reach=reach),
                           stringsAsFactors = FALSE)
    
    
    #set content for display table
    output$contents <- renderTable(head(tweets.df))
    
    # replace &amp; with & symbol
    tweets.df$tweet = gsub("&amp;", "&", tweets.df$tweet)
    reactive.values$tweets.df <- tweets.df
    
    #'set global comments df
    global.comments.df <<- tweets.df
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
    print(reactive.values$taxwords)
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
  
  
  #'#################################################################################################
  #'Observe event for button to get user summary
  #'
  #'
  observeEvent(input$getUserSummary, {
    
    user.info <- twitterEx::getUserInfo(reactive.values$selectedTweet$user)
    
    reactive.values$user.info <- user.info
    
    if(user.info$screenName %in% reactive.values$user.review.list){
      addRemoveButton <- actionButton("removeUserFromReview", icon("minus-square"))
    }else{
      addRemoveButton <- actionButton("addUserToReview", icon("plus-square"))
    }
    
    output$userSummary <- renderUI({
      
      x <- list(
        tags$img(src = user.info$profileImageUrl),
        tags$br(),
        actionLink("userDetails",user.info$name),
        addRemoveButton,
        tags$br(),tags$br(),tags$b("Home Location:"),
        tags$em(user.info$location),
        tags$br(),tags$br(),tags$b("About :"),
        tags$em(iconv(user.info$description, 'UTF-8', 'latin1', 'byte')),
        tags$br(),tags$br(),tags$b("# Tweets:"),
        tags$em(user.info$statusesCount) ,
        tags$br(),tags$br(),tags$b("# Followers:"),
        tags$em(user.info$followersCount),
        tags$br()
      )
      
      return(x)
    })
    
  })
  
  
  #'###############################################################################################
  #'Observe event for button to add user to review list
  #'
  observeEvent(input$addUserToReview, {
    
    username <- reactive.values$user.info$screenName
    
    reactive.values$user.review.list <- c(reactive.values$user.review.list, username)
    
    output$userSummary <- renderUI(NULL)
    
  })
  
  #'###############################################################################################
  #'Observe event for button to remove user from review list
  #'
  observeEvent(input$removeUserFromReview, {
    
    username <- reactive.values$user.info$screenName
    
    reactive.values$user.review.list <- reactive.values$user.review.list[!username %in% reactive.values$user.review.list]
    
  })
  
  
  #############################- START Twitter Analysis ########################################
  
  # output$tweetsBubble <- plotly::renderPlotly({
  #   
  #   tweetsdf <- reactive.values$tweets.df
  #   plot_ly(tweetsdf, x = ~tweetcreated, y = ~reach, size = ~retweetCount, 
  #           type = "scatter", mode="marker",
  #           hoverinfo = 'text',
  #           text = ~paste(paste(substr(tweet, 1,140),"..."), 
  #                         '<br> user: ', user))
  #   
  # })
  
  output$tweetsBubble <- plotly::renderPlotly({
    
    sent_df <- reactive.values$tweets.df
    
    #' format tweetcreated to date time format
    #sent_df$tweetcreated <- as.POSIXct(sent_df$tweetcreated, format="%Y-%m-%d %H:%M:%S",tz="UTC")
    
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
      #sentimentAnalysis::getSentimentAnalysisPlot(sent_df_global)
      getSentimentAnalysisPlotly(sent_df_global)
    })
    
    p <- ggplotly(plot_sent)
    
    return(p)
    
    
  })
  
  
  
  #' display a word cloud of the selected tweets
  #'
  output$tweets_word_cloud <- renderPlot({
    
    df <- isolate(reactive.values$tweets.df)
    
    #' keep only brushed subset
    df <- brushedPoints(df, input$tweetsPlot_brush)
    
    words <- unlist(str_extract_all(df$tweet, "[:alnum:][:alnum:][:alnum:][:alnum:]+"))
    words.tab <- table(words)
    #' removing the most frequest term as this is the search term itself
    words <- names(words.tab[order(words.tab,decreasing = TRUE)])[-1]
    
    words <- paste(words, collapse = " ")
    
    dtm <- withProgress({
      setProgress(message = "Processing corpus...")
      getTermMatrix(words)
    })
    
    #' terms <- names(dtm)
    #' terms.tab <- table(terms)
    #' #' removing the most frequest term as this is the search term itself
    #' terms <- names(terms.tab[order(terms.tab,decreasing = TRUE)])[-1]
    #' print(names(terms.tab[order(terms.tab,decreasing = TRUE)])[1])
    wc <- getWordCloud(names(dtm), dtm, scale=c(4,0.5),
                       min.freq = 5, max.words=50,
                       colors=brewer.pal(8, "Dark2"))
    
    return(wc)
    
  })
  
  
  #' display a word cloud of the hashtags in selected tweets
  #'
  output$tweets_tag_cloud <- renderPlot({
    
    df <- isolate(reactive.values$tweets.df)
    
    #' keep only brushed subset
    df <- brushedPoints(df, input$tweetsPlot_brush)
    
    hashtags <- unlist(str_extract_all(df$tweet, "#[:alnum:]+"))
    hashtags.tab <- table(hashtags)
    #' removing the most frequest term as this is the search term itself
    hashtags <- names(hashtags.tab[order(hashtags.tab,decreasing = TRUE)])[-1]
    
    hashtags <- paste(hashtags, collapse = " ")
    
    dtm <- withProgress({
      setProgress(message = "Processing corpus...")
      getTermMatrix(hashtags)
    })
    
    wc <- getWordCloud(paste0("#",names(dtm)), dtm, scale=c(4,0.5),
                       min.freq = 1, max.words=50,
                       colors=brewer.pal(8, "Dark2"))
    
    
    # wc <- wordcloud::wordcloud(words = names(tag.freq), freq = as.numeric(tag.freq), min.freq = 1,
    #                            max.words=200, random.order=FALSE, rot.per=0.35, 
    #                            colors=brewer.pal(8, "Dark2"))
    
    return(wc)
    
  })
  
  
  #' display a word cloud of the user handles of selected tweets
  #'
  output$user_handle_cloud <- renderPlot({
    
    df <- isolate(reactive.values$tweets.df)
    
    #' keep only brushed subset
    df <- brushedPoints(df, input$tweetsPlot_brush)
    
    userhandles <- paste(df$user, collapse = " ")
    
    dtm <- withProgress({
      setProgress(message = "Processing corpus...")
      getTermMatrix(userhandles)
    })
    
    wc <- getWordCloud(paste0("@",names(dtm)), dtm, scale=c(4,0.5),
                       min.freq = 1, max.words=50,
                       colors=brewer.pal(8, "Dark2"))
    
    
    # wc <- wordcloud::wordcloud(words = names(tag.freq), freq = as.numeric(tag.freq), min.freq = 1,
    #                            max.words=200, random.order=FALSE, rot.per=0.35, 
    #                            colors=brewer.pal(8, "Dark2"))
    
    return(wc)
    
  })
  
  
  #' display a word cloud of the user handles mentioned in selected tweets
  #'
  output$user_handle_in_tweets_cloud <- renderPlot({
    
    df <- isolate(reactive.values$tweets.df)
    
    #' keep only brushed subset
    df <- brushedPoints(df, input$tweetsPlot_brush)
    
    userhandles <- paste(unlist(str_extract_all(df$tweet, "@[:alnum:]+")), collapse = " ")
    
    dtm <- withProgress({
      setProgress(message = "Processing corpus...")
      getTermMatrix(userhandles)
    })
    
    wc <- getWordCloud(paste0("@",names(dtm)), dtm, scale=c(4,0.5),
                       min.freq = 1, max.words=50,
                       colors=brewer.pal(8, "Dark2"))
    
    
    # wc <- wordcloud::wordcloud(words = names(tag.freq), freq = as.numeric(tag.freq), min.freq = 1,
    #                            max.words=200, random.order=FALSE, rot.per=0.35, 
    #                            colors=brewer.pal(8, "Dark2"))
    
    return(wc)
    
  })
  
  
  
  #' display a barplot of the hashtags in selected tweets
  #'
  output$hastagBarPlot <- renderPlot({
    
    df <- isolate(reactive.values$tweets.df)
    
    #' keep only brushed subset
    df <- brushedPoints(df, input$tweetsPlot_brush)
    
    hashtags <- unlist(str_extract_all(tolower(df$tweet), "#[:alnum:]+"))
    
    hashtag.freq.tab <- table(hashtags)
    
    hashtag.freq.tab <- hashtag.freq.tab[order(hashtag.freq.tab, decreasing = T)]
    
    par(las=2) # make label text perpendicular to axis
    par(mar=c(5,8,4,2)) # increase y-axis margin.
    
    #' exclude first item as the most frequent item is the search term which is obvious word
    hashtag.freq.tab <- hashtag.freq.tab[2:11]
    
    hashtag_barplot <- barplot(as.numeric(hashtag.freq.tab),
                               horiz=TRUE, 
                               names.arg=names(hashtag.freq.tab), 
                               cex.names=0.8)
    
    return(hashtag_barplot)
    
  })
  
  
  
  #' render user tweets plot
  #'
  #'
  output$tweetsPlot <- renderPlot({
    
    #add dependecy to refresh button
    #input$refreshPlot
    
    df <- reactive.values$tweets.df
    print(str(df))
    radius <- scales::rescale(df$retweetCount,to = c(1,50))
    ggplot(df, aes(tweetcreated, reach)) + geom_point(colour = 'red', size = radius)
  })
  
  
  #' table showing points brushed on tweets plot
  #' display tweets select by brushed points
  #'
  output$brushedTweets <- renderTable({
    
    df <- isolate(reactive.values$tweets.df)
    
    x <- NULL
    
    if(!is.null(input$tweetsPlot_brush)){
      x <- brushedPoints(df, input$tweetsPlot_brush)
    }else if(!is.null(input$tweetsPlot_click)){
      x <- nearPoints(df, input$tweetsPlot_click, addDist = TRUE)
    }
    
    return(x[,c(2,6)])
  })
  
  
  #### End- user Map Code - #####
  
  
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
  
  ############################# - End Tweet Map - ###############################
  
  
  
  ############################## - Start USER Analysis - #############################
  
  # When map is clicked, show a popup with tweet info
  observeEvent(input$userDetails, {
    
    user.info <- isolate(reactive.values$user.info)
    
    updateTextInput(session, "username", value = user.info$getScreenName())
    
    updateTabItems(session, "tabsmenu", selected = "usermap")
    
  })
  
  
  #' render user tweets plot
  #'
  #'
  output$userTweetsPlot <- renderPlot({
    
    #add dependecy to refresh button
    input$refreshPlot
    #add dependecy to userDetail link
    input$userDetails
    
    #isolate from other events as redrawing plot takes time.
    user.info <- isolate(reactive.values$user.info)
    
    user.list <- isolate(reactive.values$user.review.list)
    
    selected.user.list <- isolate(input$usersForReviewTable_rows_selected)
    
    if(!is.null(selected.user.list)){
      user.list <- selected.user.list
    }else{
      
    }
    
    df <- withProgress({
      setProgress(message = "Processing..."
      )
      twitterEx:::getUserTweetsDataFrame(user.list)
    })
    
    reactive.values$user.tweets <- df
    
    radius <- scales::rescale(df$retweetCount,to = c(1,50))
    ggplot(df, aes(tweetdate, reach)) + geom_point(colour = 'red', size = radius)
  })
  
  
  #' display a word cloud of the selected tweets
  #'
  output$word_cloud <- renderPlot({
    
    input$generatewc
    
    df <- isolate(reactive.values$user.tweets)
    
    x <- brushedPoints(df, isolate(input$userTweetsPlot_brush))
    
    dtm <- withProgress({
      setProgress(message = "Processing corpus...")
      getTermMatrix(x$tweet)
    })
    
    wc <- getWordCloud(names(dtm), dtm, scale=c(4,0.5),
                       min.freq = 5, max.words=50,
                       colors=brewer.pal(8, "Dark2"))
    
    return(wc)
    
  })
  
  # display tweets select by brushed points
  output$brush_info <- renderTable({
    
    df <- isolate(reactive.values$user.tweets)
    
    if(!is.null(input$userTweetsPlot_brush)){
      x <- brushedPoints(df, input$userTweetsPlot_brush)
    }else if(!is.null(input$userTweetsPlot_click)){
      x <- nearPoints(df, input$userTweetsPlot_click, addDist = TRUE)
    }
    
    return(x[,c(1,2)])
  })
  
  
  
  
  #' render user summary on the user tweets analysis page
  #'
  output$userSummary2 <- renderUI({
    
    username <- input$usersForReviewTable_row_last_clicked
    
    if(is.null(username)) return(NULL)
    
    user.info <- twitterEx::getUserInfo(username)
    reactive.values$user.info <- user.info
    
    x <- list(
      tags$img(src = user.info$profileImageUrl),
      tags$strong(HTML(sprintf("%s",
                               user.info$name
      ))),
      tags$br(),tags$b("Home Location:"),
      tags$em(user.info$location),
      tags$br(),tags$b("About :"),
      tags$em(iconv(user.info$description, 'UTF-8', 'latin1', 'byte')),
      tags$br(),tags$b("# Tweets:"),
      tags$em(user.info$statusesCount) ,
      tags$br(),tags$b("# Followers:"),
      tags$em(user.info$followersCount),
      tags$br()
    )
    
    return(x)
  })
  
  output$usersForReviewTable = DT::renderDataTable({
    
    df <- as.data.frame(reactive.values$user.review.list,
                        stringsAsFactors = FALSE)
    colnames(df) <- "username"
    
    # only display the table, and nothing else
    dt <- datatable(df, rownames = FALSE,
                    options = list(dom = 't'))
    
    return(dt)
    
  },
  server = TRUE)
  
  
  
  
  #' Render User tweets on Map
  output$usermap <- renderUI({
    
    user.info <- isolate(reactive.values$user.info)
    
    tweets <- twitteR::userTimeline(user.info, 100)
    
    user.tweets.df <- twitterEx:::getTweetsDataFrame(tweets)
    
    locations <- user.tweets.df
    
    
    if (length(locations) == 0)
      return(NULL)
    
    
    # Show only selected directions
    complete.coordinates <- complete.cases(locations[,c("lon","lat")])
    #locations <- locations[complete.coordinates,]
    
    locations <- locations[complete.coordinates,]
    
    radius <- (as.numeric(locations$retweetCount) + as.numeric(locations$favCount) + 1)
    
    map <- leaflet(locations) %>%
      addTiles('http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png') %>%
      setView(lng = -0.0, lat = 51.5, zoom = 10) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        color = "blue",
        opacity = 0.8,
        radius = radius,
        layerId = ~tweetid
      )
    
    return(map)
    
  })
  
  
  
  
  #' observe event to get user details for the username provided
  #'
  observeEvent(input$fetchUserDetails, {
    
    username <- stringr::str_trim(input$username)
    
    tweets <- twitteR::userTimeline(username, 100)
    
    tweettext=sapply(tweets, function(x) x$getText())
    tweetcreated <- unlist(lapply(tweets, function(x) as.character(x$getCreated())))
    tweetdate <- as.Date(tweetcreated)
    favCount <- as.numeric(unlist(sapply(tweets, function(x) x$getFavoriteCount())))
    retweetCount <- as.numeric(unlist(sapply(tweets, function(x) x$getRetweetCount())))
    
    reach <- favCount + retweetCount
    
    options(stringsAsFactors = FALSE)
    
    df <- as.data.frame(cbind(tweet=tweettext,
                              tweetcreated=tweetcreated,
                              tweetdate = tweetdate,
                              favCount = favCount,
                              retweetCount = retweetCount,
                              reach = reach))
    
    df$tweetdate <- as.Date(tweetdate)
    df$favCount <- as.numeric(df$favCount)
    df$retweetCount <- as.numeric(df$retweetCount)
    df$reach <- as.numeric(df$reach)
    
    reactive.values$user.review.list <- c(reactive.values$user.review.list, username)
    
    reactive.values$user.tweets <- df
    
  })
  
  #' user network display
  #' 
  # output$userNetwork <- renderSimpleNetwork({
  # 
  #   user.info <- isolate(reactive.values$user.info)
  # 
  #   followers <- twitterEx::getUserFollowers(user.info$screenName, n=10)
  # 
  #   user.nw.df <- data.frame(user.info$screenName, followers)
  #   print("$$$$$$$$$$$$$$$$$-$$$$$$$$$$$$$$$-$$$$$$$$$$$$$$$-$$$$$$$$$$$$$$$-$$$$$$$$$$$")
  #   print(user.nw.df)
  #   return(simpleNetwork(user.nw.df))
  # })
  
  
  getNetworkNodesDF <- function(tweets.df, links.df){
    #' get nodes df from links.df
    nodes <- as.data.frame(table(c(c(paste0("@",tweets.df$user), links.df$target))))
    num.nodes <- nrow(nodes)
    
    # nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
    # nodes$color.border <- "black"
    # nodes$color.highlight.background <- "orange"
    # nodes$color.highlight.border <- "darkred"
    options(stringsAsFactors = FALSE)
    network.nodes.df <- data.frame(name=nodes$Var1,
                                   size=nodes$Freq,
                                   shape='dot',
                                   shadow = TRUE,
                                   #title=nodes$Var1, #' causes page to refresh
                                   label = nodes$Var1,
                                   borderWidth = 1,
                                   group=0,
                                   id=0:(num.nodes-1)
    )
    
    return(network.nodes.df)
  }
  
  getNetworkLinksDF <- function(links.df, network.nodes.df){
    
    #' get links by node id from 
    links <- merge(links.df, network.nodes.df, by.x = "source", by.y = "name")
    
    links <- merge(links, network.nodes.df, by.x="target", by.y="name")
    
    #' change column names
    colnames(links) <- c("target","source","target.size","target.group","target.id","source.size","source.group","source.id")
    
    #'links.df for creating D3Network chart
    # network.links.df <- data.frame(source=links$source.id,
    #                                target=links$target.id,
    #                                value=1)
    
    #'links.df for creating visNetwork chart
    network.links.df <- data.frame(from=links$source.id,
                                   to=links$target.id,
                                   value=1)
    
    return(network.links.df)
  }
  
  #' code to render D3Network user network
  #' 
  # output$userNetwork <- renderPrint({
  #   
  #   tweets.df <- reactive.values$tweets.df
  #   
  #   links.df <- twitterEx::getUserLinksFromTweets(tweets.df)
  #   
  #   network.nodes.df <- getNetworkNodesDF(tweets.df,links.df)
  #   network.links.df <- getNetworkLinksDF(links.df,network.nodes.df)
  #   
  #   d3Network::d3ForceNetwork(
  #     Nodes = network.nodes.df,
  #     Links = network.links.df,
  #     Source = "source", Target = "target",
  #     Value = "value", NodeID = "name",
  #     Group = "group",
  #     opacity = 0.6, standAlone = FALSE,
  #     parentElement = '#userNetwork',
  #     charge = input$attraction, zoom = TRUE
  #   )
  #   
  # })
  
  #' Code to render user network using visNetwork library
  #' 
  output$userNetwork <- visNetwork::renderVisNetwork({
    
    tweets.df <- reactive.values$tweets.df
    
    links.df <- twitterEx::getUserLinksFromTweets(tweets.df)
    
    network.nodes.df <<- getNetworkNodesDF(tweets.df,links.df)
    network.links.df <- getNetworkLinksDF(links.df,network.nodes.df[,c("name","size","group","id")])
    network.links.df$value <- NULL
    
    #print(head(network.nodes.df))
    #print(head(network.links.df))
    
    #visNetwork(network.nodes.df, network.links.df, width="100%", height="100%", main="User Network")
    
    visNetwork(network.nodes.df, network.links.df, height = "500px") %>%
      visInteraction(hover = TRUE) %>%
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
})
  
  #' observe network click event for input$current_node_id
  observe({
    
    user <- network.nodes.df[network.nodes.df$id == input$current_node_id,
                             "name"]
    user <- as.character(user)
    
    if(length(user) < 1) {
      output$networkUserSummary <- renderUI(NULL)
    }
    else {
      
      #' get selected user info
      user.info <- twitterEx::getUserInfo(user)
      
      reactive.values$user.info <- user.info
      
      if(user.info$screenName %in% reactive.values$user.review.list){
        addRemoveButton <- actionButton("removeUserFromReview", icon("minus-square"))
      }else{
        addRemoveButton <- actionButton("addUserToReview", icon("plus-square"))
      }
      
      output$networkUserSummary <- renderUI({
        
        x <- list(
          tags$img(src = user.info$profileImageUrl),
          tags$br(),
          actionLink("userDetails",user.info$name),
          addRemoveButton,
          tags$br(),tags$br(),tags$b("Home Location:"),
          tags$em(user.info$location),
          tags$br(),tags$br(),tags$b("About :"),
          tags$em(iconv(user.info$description, 'UTF-8', 'latin1', 'byte')),
          tags$br(),tags$br(),tags$b("# Tweets:"),
          tags$em(user.info$statusesCount) ,
          tags$br(),tags$br(),tags$b("# Followers:"),
          tags$em(user.info$followersCount),
          tags$br()
        )
        
        return(x)
      })
      
    }
    
  })
  
  ##################################### - End User Analysis - ###########################################
  
  
  
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
    
    # print(" X Range ")
    # print(range(plot.data$x))
    # print(" Y Range ")
    # print(range(plot.data$y))
    # print(tail(plot.data, 20))
    
    
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
  
  
  
  
  
  
  
  
  
  ############################## - Start Explore Trends - ########################
  
  observe({
    
    selected.country <- input$trendCountry
    
    indices <- which(availableTrendLocs$country == selected.country)
    
    regions <- availableTrendLocs$name[indices]
    
    updateSelectInput(session, "trendRegion", choices = regions)
    
  })
  
  
  # observeEvent(input$exploreTrend,{
  # 
  #   selected.region <- input$trendRegion
  #   selected.country <- input$trendCountry
  # 
  #   woeid <- 1 #worldwide trends
  # 
  #   indices <- which(availableTrendLocs$country == selected.country)
  # 
  #   sub.df <- availableTrendLocs[indices,]
  # 
  #   woeid <- sub.df[sub.df$name == selected.region, "woeid"]
  # 
  #   trends <- twitteR::getTrends(woeid)
  #   
  #   print(ggmap::geocode(trendCountry))
  # 
  #   #output$trendsTable <- renderTable(trends)
  # 
  # })
  
  #' output$trendsMap <- renderLeaflet({
  #'   
  #'   t1 <- Sys.time()
  #'   
  #'   #' do operation
  #'   # trends.df <- getTrends(1)[1,]
  #'   # trends.df$city <- "worldwide"
  #'   # trends.df$country <- "worldwide"
  #'   # coordinates <- geocode("worldwide")
  #'   # trends.df$lon <- coordinates$lon
  #'   # trends.df$lat <- coordinates$lat
  #'   
  #'   trendLocations <- availableTrendLocations()  
  #'   #trendLocations <- subset(trendLocations.world, trendLocations.world$country=="United Kingdom")
  #'   
  #'   withProgress(message = "Getting Twitter Trends", {
  #'   cities <- NULL
  #'   longitudes <- NULL
  #'   latitudes <- NULL
  #'     for(i in 1:nrow(trendLocations)){
  #'       
  #'       #trends <- getTrends(trendLocations[i,"woeid"])[1,]
  #'       city <- trendLocations[i,"name"]
  #'       country <- trendLocations[i,"country"]
  #'       coordinates <- geocode(paste(trendLocations[i,"name"],trendLocations[i,"country"]))
  #'       
  #'       
  #'       longitudes <- c(longitudes, coordinates$lon)
  #'       trends$lat <- c(latitudes, coordinates$lat)
  #'       
  #'       trends.df <- rbind(trends.df,trends)
  #'       
  #'     }
  #'     
  #'   })
  #'   
  #'   
  #'   
  #'   t2 <- Sys.time()
  #'   
  #'   print(t2 - t1)
  #'   
  #'   output$trendsTable <- renderTable(trends.df)
  #'   
  #'   #' plot map
  #'   map <- leaflet(trends.df) %>%
  #'     addTiles() %>%
  #'     setView(lng = -0.0, lat = 51.5, zoom = 5) %>%
  #'     addMarkers(lng = ~lon, lat= ~lat, layerId = ~woeid, label = ~name)
  #'   
  #'   return(map)
  #'   
  #' })
  
  output$trendsMap <- renderLeaflet({
    
    # extract the locations from where the trending tweets are originated
    #trendLocations <- availableTrendLocations()
    # we get a  data.frame with three columns -  name, country and woeid
    
    #locations <- paste(trendLocations$name, trendLocations$country)
    
    # ggmap librarys' geocode() returns lon-lat information
    # add two more columns to the data.frame - lon-lat value of each city
    
    # trendLocations<-cbind(trendLocations,
    #                       lon=geocode(locations)[1],
    #                       lat=geocode(locations)[2])
    # 
    # saveRDS(trendLocations, "data/trendLocations.RDS")
    
    trendLocations <- readRDS("data/trendLocations.RDS")
    
    #icons <- makeAwesomeIcon(icon = 'twitter', library = 'fa')
    
    twitterIcon <- makeIcon(
      iconUrl = "www/img/Twitter_bird_logo_2012.svg",
      iconWidth = 20, iconHeight = 20
    )
    
    map <- leaflet(trendLocations) %>%
      addTiles() %>%
      setView(lng = -0.0, lat = 34, zoom = 3) %>%
      addMarkers(lng = ~lon, lat= ~lat, icon = twitterIcon,
                 layerId = ~woeid, label = ~name, 
                 clusterOptions = markerClusterOptions())
    #addMarkers(lng = ~lon, lat= ~lat, layerId = ~woeid, label = ~name)
    
    return(map)
    
  })
  
  # When map is clicked, show a popup with tweet info
  observe({
    
    leafletProxy("trendsMap") %>% clearPopups()
    event <- input$trendsMap_marker_click
    
    if (is.null(event))
      return()
    #print(event$id)
    isolate({
      showTrendPopup(event$id, event$lat, event$lng)
    })
  })
  
  
  #'#################################################################################################
  #'Function to generate popup content when user clicks on map
  #'
  showTrendPopup <- function(woeid, lat, lng) {
    
    trends <- withProgress(message = "Getting Twitter Trends", {
      twitteR::getTrends(woeid)
    })
    
    content <- as.character(tagList(
      tags$p("Top 10 trends right now"),
      tags$table(
        tags$tr(
          tags$td(tags$a(trends$name[1], style="cursor: pointer;",
                         onclick= paste0("trendQuickView('",trends$url[1],"');"))),
          tags$td(addTrendForSearch(trends$name[1]))
          #tags$td(trendQuickView(trends$url[1]))
        ),
        tags$tr(
          tags$td(tags$a(trends$name[2], style="cursor: pointer;",
                         onclick= paste0("trendQuickView('",trends$url[1],"');"))),
          tags$td(addTrendForSearch(trends$name[2]))
        ),
        tags$tr(
          tags$td(tags$a(trends$name[3], style="cursor: pointer;",
                         onclick= paste0("trendQuickView('",trends$url[1],"');"))),
          tags$td(addTrendForSearch(trends$name[3]))
        ),
        tags$tr(
          tags$td(tags$a(trends$name[4], style="cursor: pointer;",
                         onclick= paste0("trendQuickView('",trends$url[1],"');"))),
          tags$td(addTrendForSearch(trends$name[4]))
        ),
        tags$tr(
          tags$td(tags$a(trends$name[5], style="cursor: pointer;",
                         onclick= paste0("trendQuickView('",trends$url[1],"');"))),
          tags$td(addTrendForSearch(trends$name[5]))
        ),
        tags$tr(
          tags$td(tags$a(trends$name[6], style="cursor: pointer;",
                         onclick= paste0("trendQuickView('",trends$url[1],"');"))),
          tags$td(addTrendForSearch(trends$name[6]))
        ),
        tags$tr(
          tags$td(tags$a(trends$name[7], style="cursor: pointer;",
                         onclick= paste0("trendQuickView('",trends$url[1],"');"))),
          tags$td(addTrendForSearch(trends$name[7]))
        ),
        tags$tr(
          tags$td(tags$a(trends$name[8], style="cursor: pointer;",
                         onclick= paste0("trendQuickView('",trends$url[1],"');"))),
          tags$td(addTrendForSearch(trends$name[8]))
        ),
        tags$tr(
          tags$td(tags$a(trends$name[9], style="cursor: pointer;",
                         onclick= paste0("trendQuickView('",trends$url[1],"');"))),
          tags$td(addTrendForSearch(trends$name[9]))
        ),
        tags$tr(
          tags$td(tags$a(trends$name[10], style="cursor: pointer;",
                         onclick= paste0("trendQuickView('",trends$url[1],"');"))),
          tags$td(addTrendForSearch(trends$name[10]))
        )
      )
    ))
    
    leafletProxy("trendsMap") %>% addPopups(lng, lat,content)
  }
  
  #' Observer to track users selected trends to be searched on twitter
  observe({
    #' the selected trend terms are stored in the search field seperaeted by comma
    #' access these trend terms and set them in the checkbox group
    items <- unlist(str_split(input$search,","))[-1]
    
    output$selectedTrendsPanel <- shiny::renderUI({
      if(length(items) > 0) {
        return(list(
          checkboxGroupInput(inputId="selectedTrends", label = "", choices = items, selected = items),
          actionButton("searchTrendsOnTwitter", label="", icon = icon("search"))
        ))
      }else {
        return(NULL)
      }
      
    })
    
    
  })
  
  
  #' View Trend timeline on twitter
  output$trendTimeline <- renderUI({
    
    library(xml2)
    
    url <- input$hiddenTrendURL
    
    if(is.null(url)) return(NULL)
    
    html_object = read_html(url)
    write_xml(html_object, file="www/temp.html")
    
    HTML(paste0('<i class="fa fa-plus-circle" id="close-trend-view" 
                onclick="closeTrendView()" style="cursor: pointer; text-align: right; color: red;"></i>
                <iframe id="trend-quick-view" src="temp.html" width="100%" height="600"></iframe>'))
    
  })
  
  ############################# - End Explore Trends - #####################
  
  
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
  
  ########################################################################
  #' visualise taxonomy as a tree
  #' 
  # output$taxonomyTree <- renderPlot({
  #   
  #   #global variable storing taxonomy as data.tree
  #   #return(plot(tax.data.tree))
  #   
  # })
  
  #'######################################################################################
  #' Taxonomy pie chart
  #'
  #' output$taxPie <- rCharts::renderChart2({
  #'   
  #'   #tax.data.tree <<- data.tree::FromDataFrameTable(x, pathName = "pathString")
  #'   
  #'   #Add dependency to reactive data.tree
  #'   reactive.values$shiny.tree
  #'   
  #'   taxwidedf <- ToDataFrameTypeCol(tax.data.tree, 'freq')
  #'   
  #'   colnames(taxwidedf) <- c("root",colnames(taxwidedf)[1:(length(colnames(taxwidedf))-2)],"freq")
  #'   
  #'   taxwidedf <- taxwidedf[,2:ncol(taxwidedf)]
  #'   
  #'   #' fill NAs with value of previous level
  #'   prevcol <- colnames(taxwidedf)[1]
  #'   for(colname in colnames(taxwidedf)[-1]) {
  #'     
  #'     na.index <- is.na(taxwidedf[[colname]])
  #'     
  #'     taxwidedf[[colname]][na.index] <- taxwidedf[[prevcol]][na.index]
  #'     
  #'     prevcol <- colname
  #'   }
  #'   
  #'   
  #'   taxwidedf$freq <- as.numeric(taxwidedf$freq)
  #'   print(str(taxwidedf))
  #'   
  #'   # print("####################################")
  #'   # n1 <- nPlot(~ level_1, data = taxwidedf, type = 'pieChart')
  #'   # n1
  #'   
  #'   # qry <- sprintf("select %s, count(freq) as freq from taxwidedf group by %s","level_2","level_2")
  #'   # plotdf <- sqldf::sqldf(qry)
  #'   # print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
  #'   # print(str(plotdf))
  #'   # print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
  #'   # pieChart <- hPlot(x = "level_2", y="freq", data=plotdf, type="pie")
  #'   # pieChart
  #'   
  #'   qry <- sprintf("select %s, %s, count(freq) as freq from taxwidedf group by %s, %s","level_1","level_2","level_1","level_2")
  #'   plotdf <- sqldf::sqldf(qry)
  #'   print("53453534-----------------53453453453----------------------45345345------------------------345345")
  #'   print(plotdf)
  #'   # plotdf <- plotdf[plotdf$freq > 0,]
  #'   # h <- rCharts::hPlot(freq ~ level_1, data=plotdf, type='bar', group='level_1', group.na = 'NA\'s')
  #'   # h$addParams(height=700)
  #'   # return(h)
  #'   
  #'   
  #'   h5 <- hPlot(freq ~ level_2, data = plotdf, type = c('column', 'line'), group = 'level_1', radius = 6)
  #'   h5
  #'   
  #'   
  #'   # barChart <- nPlot(freq ~ level_2, group = "level_1", data = taxwidedf, type = "multiBarChart")
  #'   # #barChart$chart(forceY = c(0, max(taxwidedf$freq)))
  #'   # barChart$addParams(width=1000)
  #'   # barChart
  #'   
  #' })
  
  
  output$taxPie <- rCharts::renderChart2({
    
    #tax.data.tree <<- data.tree::FromDataFrameTable(x, pathName = "pathString")
    
    #Add dependency to reactive data.tree
    reactive.values$shiny.tree
    
    taxwidedf <- ToDataFrameTypeCol(tax.data.tree, 'freq')
    
    colnames(taxwidedf) <- c("root",colnames(taxwidedf)[1:(length(colnames(taxwidedf))-2)],"freq")
    
    taxwidedf <- taxwidedf[,2:ncol(taxwidedf)]
    
    #' fill NAs with value of previous level
    prevcol <- colnames(taxwidedf)[1]
    for(colname in colnames(taxwidedf)[-1]) {
      
      na.index <- is.na(taxwidedf[[colname]])
      
      taxwidedf[[colname]][na.index] <- taxwidedf[[prevcol]][na.index]
      
      prevcol <- colname
    }
    
    
    taxwidedf$freq <- as.numeric(taxwidedf$freq)
    print(str(taxwidedf))
    
    # print("####################################")
    n1 <- nPlot(~ level_1, data = taxwidedf, type = 'pieChart')
    print(n1)
    
    # qry <- sprintf("select %s, count(freq) as freq from taxwidedf group by %s","level_2","level_2")
    # plotdf <- sqldf::sqldf(qry)
    # print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
    # print(str(plotdf))
    # print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
    # pieChart <- hPlot(x = "level_2", y="freq", data=plotdf, type="pie")
    # pieChart
    
  })
  
  
  
  
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
    
    print(head(df))
    
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
  
  
  
  
  
  #'#############################################################################################
  #' extract people button to extract person names from text
  #'
  #' observeEvent(input$extract.people, {
  #' 
  #'   output$extractedentities <- renderText("Finding People")
  #' 
  #'   text <- currentVerbatim()
  #' 
  #'   people <- getEntities(text, 'person')
  #' 
  #'   output$extractedentities <- renderText(paste(people, collapse = '; '))
  #' 
  #' })
  #' 
  #' #'#############################################################################################
  #' #' extract places button to extract places from text
  #' #'
  #' observeEvent(input$extract.places, {
  #' 
  #'   text <- currentVerbatim()
  #' 
  #'   places <- getEntities(text, 'location')
  #' 
  #'   output$extractedentities <- renderText(paste(places,collapse = "; "))
  #' 
  #' })
  #' 
  #' #'#############################################################################################
  #' #' extract organisations button to extract organization names from text
  #' #'
  #' observeEvent(input$extract.orgs, {
  #' 
  #'   text <- currentVerbatim()
  #' 
  #'   orgs <- getEntities(text, 'organization')
  #'   print(orgs)
  #'   output$extractedentities <- renderText(paste(orgs,collapse = ";  "))
  #' 
  #' })
  
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
    
    # updateCheckboxGroupInput(session, "peopleExtracted", label = "", 
    #                          choices = people, inline = TRUE)
    # 
    # updateCheckboxGroupInput(session, "extractedPlaces", label = "", 
    #                          choices = places, inline = TRUE)
    # 
    # updateCheckboxGroupInput(session, "organizationsExtracted", label = "", 
    #                          choices = orgs, inline = TRUE)
    
    
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
    print(hashtags)
    
    hashtags.tab <- as.data.frame(hashtags.tab)
    print(table(hashtags.tab))
    
    
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
  
  ################################## - Start Tags Mine - ##############################
  
  
  
})
