library(RSelenium)
library(XML)

scrapePosts <- function(link){
  
  #' phantomjs.binary.path path mentioned in global.R file
  eCap <- list(phantomjs.binary.path = phantomjs.binary.path)
  
  #'initialise the remote driver with phantomjs
  remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = eCap, port=4444L)
  remDr$open()
  
  
  #' last page link:
  #' http://ms-people.com/forum/forum/80-general-discussion-about-ms/page__prune_day__100__sort_by__Z-A__sort_key__last_post__topicfilter__all__st__5500
  
  posts.df <- crawlRecurive(remDr, link)
  saveRDS("data/MS_Scraped_Data.RDS")
  
#   #'parent link that contains listing of all topics
#   remDr$navigate(link)
#   
#   #'Scrape the topics elements
#   topics.element <- remDr$findElements(using = 'class', value = 'col_f_content')
#   
#   #'Extract the links to the discussions page from the element
#   links <- lapply(topics.element, function(x) getTopicLinks(x))
#   links <- unique(unlist(links))
#   
#   #'Extract posts from the discussion page
#   posts.df.list <- lapply(links, function(x) getPostsDf(remDr,x))
#   
#   #'initialise empty df
#   posts.df <- posts.df.list[[1]][0,]
#   
#   posts.df <- rbind(posts.df, do.call(rbind, posts.df.list))
  
  #' Close the remote driver
  remDr$close()
  
  return(posts.df)
}

#'###################################################################################
#'Function to extract topic links from html element
#'
getTopicLinks <- function(topic.element){
  
  topic.html <- unlist(topic.element$getElementAttribute("outerHTML"))
  
  topic.xml <- htmlTreeParse(topic.html, useInternalNodes = T)
  
  topic.link <- unlist(xpathApply(topic.xml, "//a[@href]", xmlGetAttr, "href"))
  
  return(topic.link)
  
}



#'###################################################################################
#'Function to scrape text and details from post
#'
getPostsDf <- function(remDr, link){
  
  remDr$navigate(link)
  
  #' get all post elements on page
  post.elements <- remDr$findElements(using = "xpath", "//div[@class = 'post entry-content ']")
  post <- unlist(lapply(post.elements, function(x) x$getElementText()))
  
  #' get all time.stamp elements on page
  post.time.elements <- remDr$findElements(using = "xpath", "//abbr[@class = 'published']")
  post.time <- unlist(lapply(post.time.elements, function(x) x$getElementText()))
  
  #' convert time to format
  post.time <- as.POSIXct(strptime(post.time, "%d %B %Y - %H:%M %p"))
  post.time <- as.character(post.time)
  
  #' get all user.name elements
  post.user.elements <- remDr$findElements(using = "xpath", "//span[@class = 'author vcard']")
  post.user <- unlist(lapply(post.user.elements, function(x) x$getElementText()))
  
  #' create data.frame from post elements
  posts.df <- data.frame(cbind(post,post.time,post.user), 
                         stringsAsFactors = FALSE)
  
  return(posts.df)
  
}


#'###################################################################################
#'Function to crawl recursively
#'
crawlRecurive <- function(remDr,link) {
  
  remDr$navigate(link)
  
  next.elem = NULL
  
  #' check if page has any hyperlink with text "Next"
  next.elems.list <- tryCatch({
    remDr$findElements(using = "xpath", "//a[contains(text(),'Next')]")
  },error = function(e) {
    print(e)
  })
  
  if(length(next.elems.list) < 1){
     print("inside terminating condition")
     return(scrapeData(remDr))
  }else{
    next.link <- next.elems.list[[1]]$getElementAttribute(attrName = "href")[[1]]
    print(paste0("Scraping ", next.link))
    return(
      rbind(scrapeData(remDr), 
            crawlRecurive(remDr,next.link)
      )
    )
  }
  
  
}


scrapeData <- function(remDr){
  
  #'Scrape the topics elements
  topics.element <- remDr$findElements(using = 'class', value = 'col_f_content')
  
  #'Extract the links to the discussions page from the element
  links <- lapply(topics.element, function(x) getTopicLinks(x))
  links <- unique(unlist(links))
  
  #'Extract posts from the discussion page
  posts.df.list <- lapply(links, function(x) getPostsDf(remDr,x))
  
  #'initialise empty df
  posts.df <- posts.df.list[[1]][0,]
  
  posts.df <- rbind(posts.df, do.call(rbind, posts.df.list))
  
}
