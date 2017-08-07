
library(shiny)
library(shinyBS)
library(shinydashboard)
library(leaflet)
library(shinyjs)
library(wordcloud)
library(V8)

dashboardPage(
  dashboardHeader(
                  title = span(tagList(icon("wpexplorer"), "tweelescope")), 
                  dropdownMenuOutput("notificationMenu")
                  ),
  
  
  dashboardSidebar(
    
    sidebarMenu(id = "tabsmenu",
                menuItem("Twitter Trends", tabName = "twitterTrends"),
                menuItem("Load Data", icon = icon("comments-o"), tabName = "loadData"),
                menuItem("Annotate", icon = icon("bookmark"), tabName = "annotatetab"),
                menuItem("Twitter Analysis", icon = icon("twitter"),
                         menuItem("Tweets Series", tabName = "tweetsBubbleChart"),
                         menuItem("Tweets Analysis", tabName = "tweetsAnalysis"),
                         menuItem("User Analysis", tabName = "userAnalysis")
                         ),
                # menuItem("User Analysis", tabName = "useranalysis"),
                menuItem("Taxonomy Analysis", icon = icon("area-chart"),
                         menuItem("Dashboard", icon = icon("pie-chart"), tabName = "taxonomyDashboard"),
                         menuItem("Charts", icon = icon("pie-chart"), tabName = "taxonomyAnalysis"),
                         menuItem("Heatmap", icon = icon("table"), tabName = "heatmapAnalysis"),
                         menuItem("Crosstab", icon = icon("table"), tabName = "matrixAnalysis"),
                         menuItem("Pivot", icon = icon("table"), tabName = "pivotAnalysis")
                ),
                # menuItem("Trends", tabName = "exploreTrends"),
                menuItem("Taxonomy Management", icon = icon("list-ul"),
                   menuItem("Load Taxonomy",
                            selectInput("loadTaxonomy", label = NULL, 
                                        choices = c("--Select--",list.files("./taxonomies", pattern = ".RDS"))),
                            menuItem("Load from CSV", tabName = "loadTaxonomyFile")
                   ),
                   menuItem("Save Taxonomy",
                            textInput("taxonomyName", label = ""),
                            actionButton("saveTaxonomy", "Save")
                   )
                         
                ),
                menuItem("A.I. Analysis", icon = icon("android"),
                         menuSubItem("Sentiment Analysis", icon = icon("smile-o"), tabName = "sentimentAnalysis"),
                         menuSubItem("View Learned Topics", icon = icon("book"), tabName = "lda"),
#                        menuItem("Word Suggestions",
#                                      shiny::shinyUI("wordSuggestions")),
                         menuItem("Extract Entities", icon = icon("user-circle"), tabName = "extractedEntities")
                ),
                menuItem("Twitter Mines", icon = icon("hashtag"),
                         menuItem("Tags Mine", icon = icon("hashtag"), tabName = "tagsMine"),
                         menuItem("Influencer Mine", icon = icon("at"), tabName = "influencerMine")
                ),
                # actionButton("learnTopics", "Learn Topics"),
                menuItem("Raw data", tabName = "rawdata"),
                menuItem("Reset Data", 
                         actionButton("resetData", "Reset Data"))
                # menuItemOutput("wordSuggestionsMenu")
    )
  ),
  dashboardBody(
    tags$head(tags$script(src = "./www/sequences.js"),
              tags$script(src = "http://d3js.org/d3.v3.min.js"),
              tags$script(src = "https://code.highcharts.com/highcharts.js"),
              tags$script(src = "https://code.highcharts.com/highcharts-more.js"),
              tags$script(src = "https://code.highcharts.com/modules/exporting.js"),
              tags$script(src = "https://code.highcharts.com/modules/heatmap.js"),
              tags$link(rel = "stylesheet", type="text/css", href="https://fonts.googleapis.com/css?family=Open+Sans:400,600")),
              #tags$link(rel = "stylesheet", type="text/css", href="./www/sequences.css")),
    tabItems(
      
      tabItem("loadData",
              
              tabBox(width = 12, 
                     
                     tabPanel("from Twitter", 
                              fluidRow(
                                column(4,
                                       textInput("search", "SEARCH #")
                                       
                                ),
                                column(2,
                                       actionButton("go", "GO")
                                       
                                )
                              ),
                              fluidRow(
                                column(10,
                                       DT::dataTableOutput("previousSearches"),
                                       actionButton("loadData","Load")
                                )
                              )
                     ),
                     
                     tabPanel("from CSV", 
                              fluidRow(
                                column(2,
                                       fileInput('inputFile', 'Choose CSV File',
                                                 accept=c('text/csv', 
                                                          'text/comma-separated-values,text/plain', 
                                                          '.csv'))
                                ),
                                column(2,
                                       checkboxInput('header', 'Header', TRUE)
                                ),
                                column(4,
                                       radioButtons('sep', 'Separator', inline = TRUE,
                                                    c(Comma=',',
                                                      Semicolon=';',
                                                      Tab='\t'),
                                                    ',')
                                ),
                                column(4,
                                       radioButtons('quote', 'Quote', inline = TRUE,
                                                    c(None='',
                                                      'Double Quote'='"',
                                                      'Single Quote'="'"),
                                                    '"')
                                )
                              ),
                              fluidRow(
                                
                                column(2,
                                       selectInput("ID", label = "ID", choices = c("None"))
                                ),
                                column(2,
                                       selectInput("comment", label = "Comment", choices = c("None"))
                                ),
                                column(2,
                                       selectInput("datecreated", label = "Date Created", choices = c("None"))
                                ),
                                column(2,
                                       selectInput("longitude", label = "Longitude", choices = c("None")) 
                                ),
                                column(2,
                                       selectInput("latitude", label = "Latitude", choices = c("None"))
                                ),
                                column(2,
                                       selectInput("user", label = "User", choices = c("None"))
                                )
                                
                              ),
                              fluidRow(
                                actionButton("useFile", "Okay")
                              )
                              
                     ),
                     tabPanel("from MS People", 
                              fluidRow(
                                column(4,
                                       textInput("link.to.scrape", "Link")
                                       
                                ),
                                column(2,
                                       actionButton("getPosts", "Get All Posts")
                                )
                              )
                     )
#                      ,
#                      tabPanel("from Facebook", 
#                               fluidRow(
#                                 column(4,
#                                        textInput("searchfb", "SEARCH #")
#                                        
#                                 ),
#                                 column(2,
#                                        actionButton("gofb", "GO")
#                                 )
#                               )
#                      )
                     
              ),
              tableOutput("contents")
      ),
      
      tabItem("annotatetab",
              
              #need shinyjs to customize the shinyTree
              #shinyjs needed to call javascript functions within server
              useShinyjs(),
              # allows custom js functions to be called using shinyjs
              extendShinyjs(text = javaScript),
              
              #start of page content
              fluidRow(
                #                 valueBoxOutput("rate"),
                #                 valueBoxOutput("count"),
                #                 valueBoxOutput("users")
                column(width = 2,
                       numericInput("pagenum", label = "Page Number", value = 1, min = 1, step = 1)
                ),
                column(width = 2,
                       numericInput("pagesize", label = "Comments per page", value = 10, min = 10, max = 50, step = 10)
                )
              ),
              fluidRow(
                
                #                 column(width = 4,
                #                   numericInput("pagenum", label = "Page Number", value = 1, min = 1, max = 10, step = 1)
                #                 ),
                #                 column(width = 4,
                #                   numericInput("pagesize", label = "Comments per page", value = 10, min = 1, max = 50, step = 10)
                #                 ),
                
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Tweets",
                  shiny::uiOutput("annotate")
                ),
                absolutePanel(id = "tags", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 50, right = 20, left = "auto", bottom = "auto", 
                              width = 270, height = "auto", style = "overflow-y:scroll; max-height: 600px",
                              
                              
                              textInput("selected", ""),
                              #textOutput("selected"),
                              actionButton("add", "Add to Tree"),
                              
                              shinyTree("tree", dragAndDrop = TRUE),
                              
                              fluidRow(
                                actionButton("deleteTerm", "Delete"),
                                actionButton("viewTopicComments", "View")
                              )
                              
                ),
                absolutePanel(id = "async", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = "auto", right = 50, left = "auto", bottom = 100, 
                              width = 200, height = "auto",
                              
                              
                              shiny::uiOutput("wordSuggestions")
                              
                )
                
              )
      ),

      #' Twitter Bubble Chart
      tabItem("tweetsBubbleChart",
              #rCharts::showOutput("tweetsBubble", lib = "highcharts")
              plotly::plotlyOutput("tweetsBubble"),
              tableOutput("tweetsBubbleTable")
      ),

      # Twitter Tweets Analysis
      tabItem("tweetsAnalysis",
              
              tabBox(width = 12,
                     
                     tabPanel("Wordcloud",
                              fluidRow(
                                column(width = 8, offset = 0,
                                       plotOutput("tweetsPlot", height = 300,
                                                  # Equivalent to: click = clickOpts(id = "plot_click")
                                                  dblclick = "tweetsPlot_click",
                                                  brush = brushOpts(
                                                    id = "tweetsPlot_brush"
                                                  )
                                       )
                                ),
                                column(width=4, offset = 0,
                                       plotOutput("hastagBarPlot", height = 300)
                                )
                                
                              ),
                              fluidRow(
                                column(width = 8, offset = 0,
                                       tabBox(width = 12,
                                         tabPanel(title = "Word Cloud",
                                                  plotOutput("tweets_word_cloud", height = 300)
                                         ),
                                         tabPanel(title = "HashTag Cloud",
                                                  plotOutput("tweets_tag_cloud", height = 300)
                                         ),
                                         tabPanel(title = "Users Cloud",
                                                  plotOutput("user_handle_cloud", height = 300)
                                         ),
                                         tabPanel(title = "Users in tweets Cloud",
                                                  plotOutput("user_handle_in_tweets_cloud", height = 300)
                                         )
                                       )
                                ),
                                column(width = 4, offset = 0,
                                       tableOutput("brushedTweets")
                                ) # end column
                              ) # end fluidrow
                     ), # end tabPanel
                     
                     tabPanel("Tagcloud",
                              HTML("plasdasd")
                              #plotOutput("tweets_tag_cloud")
                     ),
                     
                     tabPanel("tweets map",
                              
                              fluidRow(
                                column(width = 12,
                                       box(width = NULL, solidHeader = TRUE,
                                           leafletOutput("map", height = 900),
                                           
                                           absolutePanel(id = "tagspanel", class = "panel panel-default", fixed = TRUE,
                                                         draggable = TRUE, top = 150, right = 50, left = "auto", bottom = "auto", 
                                                         width = 200, height = "auto", style = "opacity: 0.90",
                                                         
                                                         actionButton("getUserSummary", "User Summary"),
                                                         uiOutput("userSummary")
                                           )
                                       )
                                       
                                )
                              )
                     ),
                     
                     tabPanel("exploreTrends",
                              
                              fluidRow(
                                column(width = 4,
                                       selectInput("trendCountry",label = "Country",
                                                   choices = unique(availableTrendLocs$country))
                                ),
                                column(width = 4,
                                       selectInput("trendRegion",label = "Region",
                                                   choices = unique(availableTrendLocs$name))
                                ),
                                column(width = 4,
                                       actionButton("exploreTrend","Explore Trends")
                                )
                              ),
                              fluidRow(
                                #tableOutput("trendsTable")
                                #leafletOutput("trendsMap", height = 900)
                              )
                     ) 
                     
              ) # end tabbox
      ),

      # Twitter User analysis

      tabItem("userAnalysis",
              
              tabBox(width = 12, height = "900px",
                     
                     tabPanel("user network",
                              #' NetworkD3 visualisation
                              #networkD3::simpleNetworkOutput("userNetwork")
                              
                              #' D3Network visualisation
                              #sliderInput("attraction", label = "", value=-30, min=-100, max=-20),
                              #htmlOutput("userNetwork")
                              
                              #' visNetwork visualisation
                              visNetwork::visNetworkOutput("userNetwork"),
                              absolutePanel(id = "userSummaryPanel", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 150, right = 50, left = "auto", bottom = "auto", 
                                            width = 200, height = "auto", style = "opacity: 0.90",
                                            
                                            uiOutput("networkUserSummary")
                              ),
                              verbatimTextOutput("shiny_return")
                     ),
                     
                     tabPanel("usertweets", 
                              
                              fluidRow(
                                column(width = 8, offset = 0, style='padding:0px;',
                                       plotOutput("userTweetsPlot", height = 300,
                                                  # Equivalent to: click = clickOpts(id = "plot_click")
                                                  dblclick = "userTweetsPlot_click",
                                                  brush = brushOpts(
                                                    id = "userTweetsPlot_brush"
                                                  )
                                       )
                                ),
                                column(width=4, offset = 0, style='padding:0px;',
                                       actionButton("refreshPlot","refresh", icon = icon("refresh")),
                                       #option dom = 't' to display only table
                                       DT::dataTableOutput('usersForReviewTable'),
                                       fluidRow(
                                         column(width = 8,
                                                textInput("username", label =  NULL, placeholder = "Twitter handle")
                                         ),
                                         column(width = 2,
                                                actionButton("fetchUserDetails", "Add")
                                         )
                                       )
                                       
                                )
                                
                              ),
                              fluidRow(
                                column(width = 8, offset = 0, style='padding:0px;',
                                       htmlOutput("userSummary2"),
                                       tabBox(width = 12,
                                         tabPanel("Wordcloud", 
                                                  actionButton("generatewc","word", icon = icon("cloud")),
                                                  plotOutput("word_cloud")
                                         ),
                                         tabPanel("Tagcloud"
                                                  
                                         )
                                       )
                                       
                                       #tableOutput("click_info")
                                       #verbatimTextOutput("click_info")
                                ),
                                column(width = 4, offset=0, style='padding:0px;',
                                       h4("Brushed points"),
                                       tableOutput("brush_info")
                                       #verbatimTextOutput("brush_info")
                                )
                              )
                     )
                     
              )
      ),


      # Sentiment Analysis
      tabItem("sentimentAnalysis",
              
              tabBox(width = 12,
                     tabPanel("Sentiment over time",
                        fluidRow(
                          selectInput("chrono",label = "Analysis By", selected = "month",
                                      choices = c("year","month","week","day","hour")),
                          dygraphOutput("sentimentplot")
                        )
                     ),
                     tabPanel("Sentiment Cloud", 
                        plotOutput("sentiment_cloud")
                     ),
                     tabPanel("Emotion Analysis", 
                        tableOutput("sent_df")      
                     ),
                     tabPanel("Sentiment Analysis", 
                              plotOutput("sentimentScatterPlot", height = 500,
                                          brush = brushOpts(
                                            id = "sentimentPlot_brush"
                                          )
                              ),
                              tableOutput("brushed_points")
                     )
              )
      ),

      tabItem("matrixAnalysis",
              fluidRow(
                column(width = 4,
                       selectInput("vertical",label = "Vertical", choices = "")
                ),
                column(width = 4,
                       selectInput("horizontal",label = "Horizontal", choices = "")
                )
              ),
              fluidRow(
                DT::dataTableOutput("taxonomyMatrix")
              )
      ),

      tabItem("pivotAnalysis",
              fluidRow(
                column(width = 4,
                       selectInput("pivot_rows",label = "Group Rows By", choices = "", multiple = TRUE)
                ),
                column(width = 4,
                       selectInput("pivot_columns",label = "Group Columns By", choices = "", multiple = TRUE)
                ),
                column(width = 4,
                       actionButton("drawPivot", "Pivot")
                )
              ),
              fluidRow(
                pivottabler::pivottablerOutput("taxPivot")
              )
      ),

      #' Heatmap plot  
      tabItem("heatmapAnalysis",
              fluidRow(
                column(width = 3,
                       selectInput("heatmap_ycol",label = "Y Column", choices = "")
                ),
                column(width = 3,
                       selectInput("heatmap_xcol",label = "X Column", choices = "")
                ),
                column(width = 3,
                       selectInput("select_level",label = "Select Level", choices = "")
                ),
                column(width = 3,
                       actionButton("drawHeatmap", "Draw Heatmap")
                )
              ),
              fluidRow(
                rCharts::showOutput("taxonomyHeatmap","highcharts")
              )
      ),


      tabItem("taxonomyAnalysis",
              
              tabBox(width = 12,
                     # tabPanel("Taxonomy Tree", 
                     #          shiny::plotOutput("taxonomyTree")
                     # ),
                     # tabPanel("High Charts", 
                     #          rCharts::showOutput("taxPie", lib = "nvd3")
                     # ),
                     tabPanel("Taxonomy Network",
                              networkD3::diagonalNetworkOutput("taxonomyNetwork")
                     ),
                     tabPanel("Treemap", 
                              htmlOutput("taxTreeMap")
                     ),
                     tabPanel("Sunburst", 
                              includeHTML("./www/index.html")
                     )
              )
              #sunburstOutput("sunburst")
              #htmlOutput("view")
              
      ),
      
      tabItem("taxonomyDashboard",
          rCharts::showOutput("taxPie", lib = "nvd3")
          #htmlOutput("taxPie")
      ),


      tabItem("loadTaxonomyFile",
              
              tabPanel("Load Taxonomy from CSV File",
                       hr(),
                       fluidRow(
                         column(2,
                                fileInput('inputTaxonomyFile', 'Choose CSV File',
                                          accept=c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv'))
                         ),
                         column(2,
                                checkboxInput('header', 'Header', TRUE)
                         ),
                         column(4,
                                radioButtons('sep', 'Separator', inline = TRUE,
                                             c(Comma=',',
                                               Semicolon=';',
                                               Tab='\t'),
                                             ',')
                         )
                       ),
                       fluidRow(
                         
                         column(2,
                                selectInput("conceptSelectInput", label = "Concept", choices = c("None"))
                         ),
                         column(2,
                                selectInput("termSelectInput", label = "Term", choices = c("None"))
                         ),
                         actionButton("useTaxonomyFile", "Okay")
                       ),
                       fluidRow(
                         column(6,
                                tableOutput("taxonomyTable")
                                ),
                         column(6,
                                verbatimTextOutput("taxonomyView"),
                                actionButton("applyTaxonomy","Apply Taxonomy")
                                )
                       )
                       
              )
              
      ),

      tabItem("lda",
              
              tabPanel(title = "Learned Topics", 
                       actionButton("learnTopics", "Learn Topics"),
                       fluidRow(
                         box(width = 12, title = "Learned Topics",
                             status = "primary",solidHeader= TRUE,collapsible = TRUE,
                             shiny::uiOutput("suggestedTopics")
                         )
                       ),
                       fluidRow(
                         box(width = 12, title = "View Topics",
                             status = "info", collapsible = TRUE, collapsed = TRUE,
                             LDAvis::visOutput("ldaviz")
                         )
                       )
              )
              
      ),

      #' Entity Extraction 
      tabItem("extractedEntities",
        fluidRow(
          valueBoxOutput("personsValueBox"),
          valueBoxOutput("placesValueBox"),
          valueBoxOutput("organizationsValueBox")
        ),
        fluidRow(
          column(width = 4,
                 plotly::plotlyOutput("peopleDonut"),
                 DT::dataTableOutput("peopleExtracted")
          ),
          column(width = 4,
                 plotly::plotlyOutput("placesDonut"),
                 DT::dataTableOutput("placesExtracted")
          ),
          column(width = 4,
                 plotly::plotlyOutput("orgsDonut"),
                 DT::dataTableOutput("organizationsExtracted")
          )
          
          
          # tabBox(width = 12,
          #        tabPanel("People",
          #           DT::dataTableOutput("peopleExtracted")
          #        ),
          #        tabPanel("Places", 
          #           DT::dataTableOutput("placesExtracted")
          #        ),
          #        tabPanel("Organizations",
          #           DT::dataTableOutput("organizationsExtracted")
          #        )
          # )
        )
        
      ),
      

      tabItem("influencerMine",
              
        plotly::plotlyOutput("userDonut")
              
      ),

      tabItem("tagsMine",
              
        plotly::plotlyOutput("hashtagDonut")
              
      ),

      tabItem("word_suggestions_page",
              shiny::uiOutput("similarWordSuggestions")
      ),


      tabItem("rawdata",
              
              tabPanel("Data explorer",
                       hr(),
                       DT::dataTableOutput("datatable")
              )
              
      ),
    
      tabItem("twitterTrends",
          leafletOutput("trendsMap", height = 630),
          absolutePanel(id = "twitterTrendPanel", class = "panel panel-default", fixed = TRUE,
                        draggable = TRUE, top = 150, right = 50, left = "auto", bottom = "auto", 
                        width = 200, height = "auto", style = "opacity: 0.40",
                        htmlOutput("inc")
          )
      )

      
    )
  )
)