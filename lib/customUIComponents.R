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



#'################################################
#' icon button to add selected trend to list
#' 
# addTrendForSearch2 <- function(trendName) {
#   tagList(
#     tags$div(tags$i(class="fa fa-plus-circle", 
#                     title="Add to Search",
#                     onclick= paste0("document.getElementById('search').value = document.getElementById('search').value.concat(',','",
#                                     trendName,"') ;",
#                                     "Shiny.onInputChange('search',document.getElementById('search').value);",
#                                     "this.style.visibility = 'hidden';")
#                     ),
#              style = "padding-left: 3px;")
#     
#   )
#   
# }


#'################################################
#' icon button to add selected trend to list
#' 
addTrendForSearch <- function(trendName) {
  tagList(
    tags$div(tags$i(class="fa fa-plus-circle", 
                    title="Add to Search",
                    id = trendName,
                    onclick= paste0("addTrendToSearch('",trendName,"');")
    ),
    style = "padding-left: 3px;")
    
  )
  
}



#'################################################
#' icon button to open selected trend in a window
#' 
trendQuickView <- function(trendUrl) {
  tagList(
    tags$div(tags$i(class="fa fa-twitter",
                    onclick= paste0("trendQuickView('",trendUrl,"');"),
                    style = "padding-left: 5px;")
    )
  )
}