library(RJSONIO)


#' Recursive function to convert paernt child data.frame into parent child list for JSON
makeList <- function(x){
  
  if(ncol(x) > 2){
    
    listSplit <- split(x[-1],x[1],drop = TRUE)
    
    lapply(names(listSplit), function(y){list(name=y,children=makeList(listSplit[[y]]))})
    
  }else{
    
    lapply(seq(nrow(x[1])), function(y) { list(name=x[,1][y], Count=x[,2][y])})
    
  }
  
}

from = c("ram","ram","ram","shyam","shyam")
to=c("abc","def","ghi","pqr","stu")

df <- data.frame(from,to)
df$count <- "5"

jsonOut <- toJSON(list(name="jsonData",children=makeList(df)))
cat(jsonOut)

#save JSON File
write(jsonOut, "./visualisation/display.json")

