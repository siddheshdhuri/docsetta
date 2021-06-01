library(data.tree)
library(shiny)
library(shinydashboard)
library(stringi)
library(stringr)
library(inputAnnotate)
library(dygraphs)
library(xts)
library(DT)

library(TaxonomyTree)
library(networkD3)
#library(jobqueue)
library(shinyTree)
library(magrittr)
library(htmlwidgets)
#library(sunburstR)
library(googleVis)
library(ggplot2)
library(memoise)
#library(RSQLite)
library(scales)
library(ggmap)
library(lubridate)
library(pdftools)
library(heatmaply)
library(shinycssloaders)

#NLP Libraries
library(rJava)
library(NLP)
library(openNLP)
#' TDM with n gram tokenizer doens work if RWeka is library is loaded use with scope resolution
# library(RWeka)
#library(openNLPmodels.en)
library(tm)
library(text2vec)
library(parallel)
library(pivottabler)
#library(graphTweets)
library(visNetwork)
library(sentimentAnalysis)
#library(tidyverse)
library(text2vec)
#library(caret)
library(glmnet)
#library(ggrepel)
library(purrrlyr)
library(rCharts)
library(rjson)
library(plotly)
library(RCurl)
library(tidytext)
library(quanteda)
rm(list=ls())

options(encoding = 'utf-8')
#######################################################################################
###################################1. Extract Twitter messages#########################
#######################################################################################

#' Flag to enable / disable Twitter fetch and web scrapping using Selenium. FALSE is disabled
flag <- TRUE

#' Flag to invalidate LDA topic reactive element. TRUE mean it will invalidate after every X seconds
#' and reevaluate the expression
# invalidate.flag <- TRUE


source("lib/helpe.R")
source("lib/sentimentanalysis.R")
#source("lib/ms_people_scrape.R")
source("lib/topicModeling.R")
source("lib/asyncProcessor.R")
source("lib/wordSuggestions.R")
source("lib/taxonomyMatrix.R")
source("lib/customUIComponents.R")
source("lib/LatentSemanticAnalysis.R")
# Setup Twitter authorization

#' GLOBAL Full comments DF
global.comments.df <- NULL


# setup SQLite db
# sqlite    <- dbDriver("SQLite")
# exampledb <- dbConnect(sqlite,"newexample.db")
# 
# register_sqlite_backend("newexample.db")


root.name <- "Taxonomy"

tax.data.tree <- Node$new(root.name)

###############################################################################################
# javascript functions that handle updating.
# changeTree will change the nodes of the tree but it does not care about
# the state data provided with the json input. I use the next two functions to
# force all nodes to open and unselect them all. It should be possible to 
# deal with individual nodes but I didn't get into that.
javaScript = "shinyjs.changeTree = function(params){
eval(\"$('#tree').jstree(true).settings.core.data=\"+params);
$('#tree').jstree(true).refresh();
}
shinyjs.open = function(){
//$('#tree').jstree(true).open_all();
$('#tree').jstree(true).find('.jstree-open').open_node();

}
shinyjs.deselect = function(){
$('#tree').jstree(true).deselect_all();
}"

##############################################################################################

############## - set up annotators for NLP entity extraction #################################
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

############# - load file selector input names -##############################################
columnnames <- c("ID", "comment", "datecreated", "longitude", "latitude", "user")

############# - NA Patters to ignore while reding text file - ################################
na.patterns <- "#REF!|#N/A|\\(blank\\)"


############# - Phantom JS Binary Path location - ############################################
phantomjs.binary.path <- "/Users/sid/Projects/utils/phantomjs/bin/phantomjs"

############# - Facebook Token - #############################################################
accesstoken = "EAACEdEose0cBAH1oCOvZBJLkTkkLlM6Sq8zDUwEVGWdIQZBEbtBthoNl6I5GfIWcIkKNG5jI2wBrCDJ8fGMV14J0wbq1ge8Qx2hz0q9YiY9dBjCFmg4tx2OHJ8G524yDTOVGdIGNyU1xxoXy5ZAabQoSgZCLK0onNQC9aMch1QZDZD"

#\options(RCurloptions = list(verbose=FALSE, capath=system.file("CurlSSL","cacert.pem",package="RCurl"),ssl.verifypeer = FALSE))

non.zero.df <- NULL

############# - Global Pivot Table - #########################################################
pt <- PivotTable$new()

############# = User Netwoork Nodes - #################################################################
network.nodes.df <- NULL

sent_df_global <- NULL

############## - Global wide df - ##################################################################
doc.tax.wide.df <- NULL

############## - 
get_memory_allocations <- function(){ return(sort( sapply(ls(),function(x){object.size(get(x))}))) }
