
getDocTaxLongDF <- function(tax.data.tree, tweet.df){
  
  comments <- tweet.df$tweet
  tweedids <- tweet.df$tweetid
  taxwords <- as.character(tax.data.tree$Get('name'))
  
  options(stringsAsFactors = FALSE)
  
  #' convert taxonomy tree pathStrings into columns
  tax.path.string <- data.tree::ToDataFrameTable(tax.data.tree, "pathString")
  tax.path.string <-  as.character(tax.path.string)
  #' split pathStrings on "/" character
  len <-sapply(strsplit(tax.path.string, "/"),FUN = length)
  max.len <- max(len)
  #' number of columns should be max length of pathString
  levels <- paste0("LEVEL",seq(1:(max.len-2)))
  
  #' data.frame to store converted pathStrings into data.frame
  temp.df <- data.frame()
  for(path in tax.path.string){
    
    row <- unlist(strsplit(as.character(path), "/"))
    len.row <- length(row)
    
    diff <- max.len - len.row
    
    if(diff > 0){
      new.row <- c(row, rep(row[len.row],diff))
    }else{
      new.row <- row
    }
    temp.df <- rbind(temp.df,new.row)
    
  }
  temp.df <- temp.df[,2:max.len]
  colnames(temp.df) <- c(levels,"Term")
  temp.df$Term <- tolower(temp.df$Term)
  
  #' #' replace newline character from the posts
  #' comments <- stringr::str_replace_all(comments, "[\r\n]" , " ")
  #' #' convert comments to lower case
  #' comments <- tolower(comments)
  #' 
  #' #' keep unique terms
  #' taxwords <- unique(tolower(taxwords))
  #' remove first word which is the root of the tree: 'taxonomy'
  taxwords <- taxwords[2:length(taxwords)]
  
  docTermDF <- getDocTermDF(tweet.df, taxwords, "tweetid","tweet")
  
  library(reshape2)
  #' melting wide df will result in length(taxwords)*length(comments) row long df
  long.df <- melt(docTermDF, id.vars = c("tweetid", "tweet"))
  
  merged.df <- merge(x=long.df,y=temp.df, by.x = "variable", by.y = "Term")
  
  non.zero.df <- merged.df[merged.df$value > 0,]
  
  return(non.zero.df)
  
}


paste.unique <- function(vector.words) {
  paste(unique(vector.words), collapse = ", ")
}

# xx <- aggregate(variable ~ Docs, data = non.zero.df, paste.unique)
# 
# 
# yy <- getTransposeData(yaxis = "LEVEL2", xaxis = "variable", valuevar = "NumComments", non.zero.df)
  
#agg.cat <- getAggDataCategorical("Docs","LEVEL-1",non.zero.df)



# '########################################################
#' Function to aggregate data frame given aggregate columns
getAggData <- function(aggBy, data){
  
  agg.by <- lapply(aggBy, as.symbol)
  
  agg.data <- data %>%
    group_by_(.dots = agg.by) %>%
    select(Docs) %>%
    summarise(
      NumComments = n_distinct(Docs)
    )
  
  return(agg.data)
}


getAggDataCategorical <- function(aggBy, aggVar, data){

  agg.data<- unique(data, by=aggBy) %>% 
              group_by(aggBy) %>%
              mutate(n=n_distinct(aggVar))
  
  return(agg.data)
  
}


#' ########################################################
#' Function to transpose column given data frame
#'
getTransposeData <- function(xaxis,yaxis,valuevar,data) {
  
  agg.data <- getAggData(c(xaxis,yaxis), data)
  
  dependent <- yaxis
  factors <- setdiff(xaxis,dependent)
  if(length(factors) < 1) factors <- xaxis
  formula.var <- as.formula(paste( paste(factors,collapse = "+"), paste("~",dependent)))
  
  transposed.data <- reshape2::dcast(agg.data, 
                                     formula.var, 
                                     value.var = valuevar , 
                                     fun.aggregate = getFunction("sum"), 
                                     na.rm = TRUE)
  
  return(transposed.data)
}


#' Function to create Doc Term Data.frame for specific dictionary
#' 
#' @param textdf a data.frame with a column from which terms to be 
#' @param dict a characted vector of dictionary terms to be searched
#' @param id_col name of column containing id 
#' @param text_col name of column containing text
#' 
#' @return docTermDF document Term Data Frame.
getDocTermDF <- function(textdf, dict, id_col, text_col){
  
  comments <- textdf[[text_col]]
  
  #' replace newline character from the posts
  comments <- stringr::str_replace_all(comments, "[\r\n]" , " ")
  #' convert comments to lower case
  comments <- tolower(comments)
  
  #' keep unique terms
  dict <- unique(tolower(dict))
  
  #docTermDF <- setNames(data.frame(textdf[[id_col]]),id_col)
  
  for(term in dict){
    
    word.counts <- stringi::stri_count_regex(comments,
                                             paste0("\\b", stringr::str_trim(term),"\\b"))
    print(" IN HRERERERERERERERERERER ---------- >>>>>> ")
    print(id_col)
    print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
    print(str(textdf))
    this.df <- setNames(data.frame(textdf[[id_col]],
                                   word.counts),
                        c(id_col,term)
    )
    
    textdf <- merge(textdf, this.df, by=id_col)
    
  }
  #print(textdf[textdf$tweetid == 677])
  return(textdf)
  
}



#'#############################################################################
#' Function to get Wide data.frame for Document Taxonomy
#' 
getDocTaxWideDF <- function(selected.level, tax.data.tree, tweet.df, non.zero.long.df = NULL) {
  
  if(is.null(non.zero.long.df)){
    non.zero.long.df <- getDocTaxLongDF(tax.data.tree,tweet.df)
  }
  
  #' creating wide data.frame
  levelones <- unique(non.zero.long.df$LEVEL1)
  
  first.levelone <- levelones[1]
  query.str <- sprintf("select tweetid, %s as %s_TERM,
                       sum(value) as %s_TERM_FREQ
                       from 'non.zero.long.df' 
                       where LEVEL1 = '%s' 
                       group by tweetid, %s ;",
                       selected.level,
                       gsub(" ","_",first.levelone),
                       gsub(" ","_",first.levelone),
                       first.levelone,
                       selected.level
  ) 
  
  
  doc.tax.wide.df <- sqldf::sqldf(query.str)
  
  for(this.levelone in levelones[2:length(levelones)]){
    #' subset data for this.levelone
    
    #' define sql style query 
    query.str <- sprintf("select tweetid, %s as %s_TERM,
                         sum(value) as %s_TERM_FREQ
                         from 'non.zero.long.df' 
                         where LEVEL1 = '%s' 
                         group by tweetid, %s ;",
                         selected.level,
                         gsub(" ","_",this.levelone),
                         gsub(" ","_",this.levelone),
                         this.levelone,
                         selected.level)
    
    #' subset data using sql.query
    subset_data <- sqldf::sqldf(query.str)
    
    
    #' merge subset data by Doc id
    doc.tax.wide.df <- merge(x=doc.tax.wide.df,
                             y=subset_data, 
                             by = "tweetid", all = TRUE)
    
  }
  
  return(doc.tax.wide.df)
  
}



#'#############################################################################
#' Function to get Data.frame for plotting data on Highcharts heatmap
#' 
getHeatmapDF <- function(doc.tax.wide.df, xcol, ycol){
  
  if(!grepl("_TERM",xcol)){
    xcol <- paste0(gsub(" ","_",xcol),"_TERM")
    ycol <- paste0(gsub(" ","_",ycol),"_TERM")
  }
  
  heatmap_df <- doc.tax.wide.df[,c("tweetid",xcol, ycol)]
  
  #print(unique(heatmap_df$Side_Effects_TERM))
  #' subset 
  qry <- sprintf("select %s, %s, count(distinct tweetid) as value from heatmap_df group by %s, %s ;",
                 xcol, ycol,xcol, ycol)
  
  heatmap_df <- sqldf::sqldf(qry)
  #' sort df such that NAs are first
  heatmap_df <- heatmap_df[with(heatmap_df, 
                                order(heatmap_df[[xcol]], heatmap_df[[ycol]], na.last = FALSE)), ]
  
  #' highcharts javascript ids are from 0 we need to create custom ids
  #' create ids for first column. 
  xvars <- unique(heatmap_df[[xcol]])
  xvars <- xvars[order(xvars, na.last = FALSE)]
  #' NAs assigned value 0
  xvars.id <- 0:(length(xvars)-1)
  xvar.df <- data.frame(xvar = xvars,
                        x = xvars.id)
  print(xvar.df)
  #' create ids for second column
  yvars <- unique(heatmap_df[[ycol]])
  yvars <- yvars[order(yvars, na.last = FALSE)]
  #' NAs assigned value 0
  yvars.id <- 0:(length(yvars)-1)
  yvar.df <- data.frame(yvar = yvars,
                        y = yvars.id)
  
  #' replace NAs with None
  heatmap_df[is.na(heatmap_df)] <- "None"
  yvar.df[is.na(yvar.df)] <- "None"
  xvar.df[is.na(xvar.df)] <- "None"
  
  heatmap_df <- merge(heatmap_df, xvar.df, by.x = xcol, by.y = "xvar")
  
  heatmap_df <- merge(heatmap_df, yvar.df, by.x = ycol, by.y ="yvar")
  
  return(list(heatmap_df = heatmap_df,
              xvar.df = xvar.df,
              yvar.df = yvar.df)
  )
  
}
