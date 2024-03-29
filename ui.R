
library(shiny)
library(shinyBS)
library(shinydashboard)
library(leaflet)
library(shinyjs)
library(wordcloud)
library(V8)

dashboardPage(title = "Orox.ai", skin = "black", 
  dashboardHeader(
    title = span(tagList(icon("barcode"), HTML("Dirk"))), 
    dropdownMenuOutput("notificationMenu")
  ),
  
  
  dashboardSidebar(
    
    sidebarMenu(id = "tabsmenu",
                
                menuItem("Load Data", icon = icon("file-upload"), tabName = "loadData"),
                menuItem("Annotate", icon = icon("bookmark"), tabName = "annotatetab"),
                menuItem("Taxonomy Analysis", icon = icon("area-chart"),
                         menuItem("Charts", icon = icon("pie-chart"), tabName = "taxonomyAnalysis"),
                         menuItem("Heatmap", icon = icon("table"), tabName = "heatmapAnalysis"),
                         menuItem("Crosstab", icon = icon("table"), tabName = "matrixAnalysis"),
                         menuItem("Pivot", icon = icon("table"), tabName = "pivotAnalysis")
                ),
                menuItem("Taxonomy Management", icon = icon("list-ul"),
                         menuItem("Load Taxonomy",
                                  selectInput("loadTaxonomy", label = NULL, 
                                              choices = c("--Select--",list.files("./taxonomies", pattern = ".RDS")),
                                              selected = "CoL.RDS"),
                                  menuItem("Load from CSV", tabName = "loadTaxonomyFile")
                         ),
                         menuItem("Save Taxonomy",
                                  textInput("taxonomyName", label = ""),
                                  actionButton("saveTaxonomy", "Save")
                         )
                         
                ),
                menuItem("Computational Linguistics", icon = icon("android"),
                         menuItem("Term Frequency", tabName = "term_freqs"),
                         menuSubItem("Explore Context", icon = icon("search"), tabName= "explore_context"),
                         menuSubItem("Learn Topics", icon = icon("book"), tabName = "lda"),
                         menuSubItem("Sentiment Analysis", icon = icon("smile-o"), tabName = "sentimentAnalysis"),
                         menuItem("Extract Entities", icon = icon("user-circle"), tabName = "extractedEntities"),
                         menuItem("Doc Comparison", icon=icon("cubes"), tabName="documentSimilarity"),
                         menuItem("Doc Gist", icon=icon("compress"), tabName="documentGist")
                ),
                # actionButton("learnTopics", "Learn Topics"),
                menuItem("Raw data", tabName = "rawdata"),
                menuItem("Reset Data", 
                         actionButton("resetData", "Reset Data"))
    )
  ),
  dashboardBody(
    tags$head(tags$script(src = "./www/sequences.js"),
              tags$script(src = "customComponentJS.js"),
              tags$script(src = "http://d3js.org/d3.v3.min.js"),
              tags$script(src = "https://code.highcharts.com/highcharts.js"),
              tags$script(src = "https://code.highcharts.com/highcharts-more.js"),
              tags$script(src = "https://code.highcharts.com/modules/exporting.js"),
              tags$script(src = "https://code.highcharts.com/modules/heatmap.js"),
              tags$link(rel = "stylesheet", type="text/css", href="https://fonts.googleapis.com/css?family=Open+Sans:400,600"),
              tags$style(HTML(".dataTables_filter, .dataTables_info { display: none; }"))
              ,tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")) 
    )
    #tags$link(rel = "stylesheet", type="text/css", href="./www/sequences.css")),
    ,tabItems(
      
      tabItem("loadData",
              
              tabBox(width = 12, 
                     
                     tabPanel("Load data",
                              fluidRow(
                                column(6,
                                       box(width=12,title = "Load data from saved File", status = "info", 
                                           collapsible = TRUE, collapsed = FALSE,
                                           selectInput("previousSearches", label = "Select File(s) to load for analysis", multiple = TRUE,
                                                       choices = list.files("data/discussions"), selected = "Multiple_Sclerosis_Docs.RDS"),
                                           actionButton("loadData","Load")
                                       )
                                )
                              )
                     ),
                     
                     tabPanel("Load data from PDF",
                          
                          fluidRow(
                            column(6,
                                   fileInput('inputPDF', 'Choose PDF File', multiple = TRUE)
                            )
                          ),
                          fluidRow(
                            DT::dataTableOutput("documentsTable")
                          )
                              
                     ),
                     
                     tabPanel("Load data from CSV", 
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
                                       selectInput("comment", label = HTML("Comment<sup>*Required</sup>"), choices = c("None"))
                                ),
                                column(1,
                                       selectInput("datecreated", label = "Date Created", choices = c("None"))
                                ),
                                column(1,
                                       selectInput("longitude", label = "Longitude", choices = c("None")) 
                                ),
                                column(1,
                                       selectInput("latitude", label = "Latitude", choices = c("None"))
                                ),
                                column(2,
                                       selectInput("user", label = "User", choices = c("None"))
                                ),
                                column(2,
                                       selectInput("columns_to_keep", label = "Keep columns", choices = c("None") , multiple=TRUE)
                                )
                                
                              )
                              
                     )
              ),
              actionButton("useFile", "Apply Column Mapping"),
              shinycssloaders::withSpinner(DT::dataTableOutput("contents"))
      ),
      
      tabItem("annotatetab",
              
              #need shinyjs to customize the shinyTree
              #shinyjs needed to call javascript functions within server
              useShinyjs(),
              # allows custom js functions to be called using shinyjs
              extendShinyjs(text = javaScript),
              
              #start of page content
              fluidRow(
                column(width = 2,
                       numericInput("pagenum", label = "Page Number", value = 1, min = 1, step = 1)
                ),
                column(width = 2,
                       numericInput("pagesize", label = "Comments per page", value = 10, min = 1, max = 10, step = 1)
                )
              ),
              fluidRow(
                
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Annotate Text",
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
      
      
      # Sentiment Analysis
      tabItem("sentimentAnalysis",
              
              tabBox(width = 12, height = 600,
                     tabPanel("Word Cloud",
                              plotOutput("word_cloud")
                     ),
                     tabPanel("Sentiment over time",
                              fluidRow(
                                selectInput("chrono",label = "Analysis By", selected = "month",
                                            choices = c("year","month","week","day","hour")),
                                dygraphOutput("sentimentplot")
                              )
                     ),
                     tabPanel("Sentiment Cloud",
                              shinycssloaders::withSpinner(plotOutput("sentiment_cloud"))
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
                column(width = 3,
                       selectInput("vertical",label = "Rows", choices = "", multiple = TRUE, selectize = TRUE)
                ),
                column(width = 3,
                       selectInput("horizontal",label = "Columns", choices = "", selectize = TRUE)
                ),
                column(width = 3,
                       actionButton("createCrossTab", "Create Crosstab")
                ),
                column(width = 3,
                       downloadButton("downloadCrossTab", "Download")
                )
              ),
              fluidRow(
                shinycssloaders::withSpinner(DT::dataTableOutput("taxonomyMatrix"))
              )
      ),
      
      tabItem("pivotAnalysis",
              fluidRow(
                column(width = 3,
                       selectInput("pivot_rows",label = "Group Rows By", choices = "", multiple = TRUE)
                ),
                column(width = 3,
                       selectInput("pivot_columns",label = "Group Columns By", choices = "", multiple = TRUE)
                ),
                column(width = 3,
                       actionButton("drawPivot", "Pivot")
                ),
                column(width = 3,
                       downloadButton("download_taxonomy_pivot", label = "Download")
                )
              ),
              fluidRow(
                
                div(style = 'overflow-x: scroll', 
                    shinycssloaders::withSpinner(pivottabler::pivottablerOutput("taxPivot"))
                    )
                
              )
      ),
      
      #' Heatmap plot  
      tabItem("heatmapAnalysis",
              fluidRow(
                column(width = 3,
                       selectInput("heatmap_ycol",label = "Topic on Y Axis", choices = "")
                ),
                column(width = 3,
                       selectInput("heatmap_xcol",label = "Topic on X axis", choices = "")
                ),
                column(width = 3,
                       selectInput("select_level",label = "Select terms at level", choices = "")
                ),
                column(width = 3,
                       actionButton("drawHeatmap", "Draw Heatmap")
                )
              ),
              fluidRow(
                shinycssloaders::withSpinner(rCharts::showOutput("taxonomyHeatmap","highcharts"))
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
                     # tabPanel("Taxonomy Network",
                     #          networkD3::diagonalNetworkOutput("taxonomyNetwork")
                     # ),
                     tabPanel("Treemap", 
                              htmlOutput("taxTreeMap")
                     ),
                     tabPanel("Sunburst", 
                              sunburstR::sunburstOutput("sunburst_chart")
                     )
              )
              
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
                         actionButton("useTaxonomyFile", "Create Taxonomy")
                       ),
                       fluidRow(
                         column(6,
                                tableOutput("taxonomyTable")
                         ),
                         column(6,
                                verbatimTextOutput("taxonomyView"),
                                actionButton("applyTaxonomy","Apply Taxonomy to text data")
                         )
                       )
                       
              )
              
      ),
      
      tabItem("term_freqs",
              fluidRow(
                column(width=2,
                       selectizeInput("group_by_column", "Group by", choices = c("None"), multiple=TRUE)
                ),
                column(width=2,
                        numericInput("number_of_words_for_term_freq_per_category", "Words per category", value = 10, min = 10, max = 100)
                )
                ,column(width=2,
                        numericInput("ngrams_for_term_freq_per_category", "NGrams", value = 1, min = 1, max = 4)
                )
                ,column(width=2,
                        textAreaInput("words_to_exclude_in_term_freq", "Words to exclude")
                ),
                column(width=2,
                       actionButton("get_term_freq_by_group", "Get Term Freq")   
                )
                
              )
              ,fluidRow(
                div(style = 'overflow-x: scroll', 
                    DT::dataTableOutput('term_freq_table')
                )
              )
              
      ),
      
      
      tabItem("explore_context",
              fluidRow(
                textInput("phrase_to_explore", label = "", placeholder = "Enter word or phrase to explore"),
                actionButton("explore_context_button", "Explore")
              )
              ,fluidRow(
                DT::dataTableOutput('context_output')
              )
              
      ),
      
      #' topic modelling using LDA
      tabItem("lda",
              
              tabPanel(title = "Model Topics", 
                       fluidRow(
                         column(width=2,
                            actionButton("learnTopics", "Model Topics")
                         )
                         ,column(width=2,
                            checkboxInput("supervisedLDA", "Use my taxonomy as seed for topic modelling", TRUE)
                         )
                         ,column(width=2,
                            numericInput("number_of_words_per_topic", "Words per topic", value = 10, min = 1, max = 30)
                         )
                         ,column(width=2,
                                 numericInput("ngrams_for_topic", "NGrams", value = 1, min = 1, max = 4)
                         )
                         ,column(width=4,
                                 textAreaInput("words_to_exclude", "Words to exclude")
                         )
                       )
                       ,fluidRow(
                         box(width = 12, title = "View Topics",
                             status = "info", collapsible = TRUE, collapsed = TRUE,
                             shinycssloaders::withSpinner(LDAvis::visOutput("ldaviz"))
                         )
                       ),
                       fluidRow(
                         box(width = 12, title = "Add to taxonomy",
                             status = "primary",solidHeader= TRUE,collapsible = TRUE,
                             shiny::uiOutput("suggestedTopics")
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
                
              )
              
      ),
      
      #' document comparison using LSA
      tabItem("documentSimilarity",
              fluidRow(
                column(width=4,
                  selectInput("compareDocsBy", label = "Compare Docs By", choices = c("User","Doc id"), selected = "User")
                ),
                column(width=4,
                       actionButton("compareDocs", "Compare Docs")
                )
              ),
              fluidRow(
                #d3heatmap::d3heatmapOutput("docHeatmap")
                plotlyOutput("docHeatmap")
              )
      ),
      
      tabItem("documentGist",
              
              fluidRow(
                column(width=4,
                       selectInput("getDocGistBy", label = "Doc Gist By", choices = c("Overall","User","Doc id"), selected = "Overall")
                ),
                column(width=4,
                       actionButton("getDocGist", "Get Gist")
                )
              ),
              fluidRow(
                HTML("Selected : Overall") 
              ),
              fluidRow(
                column(width = 6, 
                       #DT::dataTableOutput("gistcomments")
                       fluidRow(
                         column(12,
                                dataTableOutput('table')
                         ),
                         column(10, 
                                ""
                         ),
                         column(2,              
                                # adding new page filter
                                uiOutput("pageFilter")
                         )
                       )
                ),
                column(width = 6,
                       plotOutput("gist_cloud")
                )
              )
      ),
      
      tabItem("word_suggestions_page",
              shiny::uiOutput("similarWordSuggestions")
      ),
      
      
      tabItem("rawdata",
              
              tabPanel("Data explorer",
                       hr(),
                       DT::dataTableOutput("datatable")
              )
              
      ) #' end tab Item
      
    ) #' end Tab Items
    
  ) #' end dashboard body
  
) #' end dashboard page