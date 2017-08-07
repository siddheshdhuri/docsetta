# twittermkt
R app to be used for marketing analysis using twitter data


#Setup
#*** -IMP- ****#
# when installing packages on a remote server you need to ensure that you
# install in the root directory so that packages are available to all
# install on the ubuntu / unix console using the following command
# sudo su - -c "R -e \"install.packages('tidyr', repos='http://cran.rstudio.com/')\""

# installing devtools on ubuntu do following step on terminal
# sudo apt-get -y install libcurl4-gnutls-dev libxml2-dev libssl-dev
install.packages("devtools")
install.packages("shiny")
install.packages("shinyBS")
install.packages("shinydashboard")
install.packages("DT")
install.packages("tm")
install.packages("tidyr")
install.packages("leaflet")


install.packages("data.tree")
install.packages("networkD3")
install.packages("dygraphs")

install.packages("twitteR")
install.packages("shinyjs")

# need to install JAVA first 
# sudo apt-get install openjdk-8-jdk
# apt-cache search jdk
# export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
# export PATH=$PATH:/usr/lib/jvm/java-8-openjdk-amd64/bin
install.packages("rJava")
install.packages("RWeka")
install.packages("openNLP")
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("jobqueue", repos="http://R-Forge.R-project.org", type="source")

# dependency on lib-v8 you need to install on terminal using command
# sudo apt-get install libv8-dev
install.packages("V8")
install.packages("lubridate")
install.packages("text2vec")

install.packages("lda")
#' dependencies for LDAvis install proxy
install.packages("proxy")

install.packages("LDAvis")
#install.packages("parallel")
install.packages("pivottabler")
install.packages("visNetwork")
install.packages("glmnet")
install.packages("purrrlyr")

install.packages("digest")
install.packages("googleVis")
install.packages("wordcloud")
#install.packages("RSQLite")
install.packages("RSelenium")
install.packages("vwr")

install.packages("ggmap")

install.packages("rCharts")
install.packages("rjson")
install.packages("plotly")
install.packages("ggrepel")

library(devtools)
devtools::install_github("trestletech/shinyTree")
devtools::install_github("timelyportfolio/sunburstR")
devtools::install_github("siddheshdhuri/inputAnnotate")
devtools::install_github("siddheshdhuri/TaxonomyTree")
devtools::install_github("siddheshdhuri/twitterEx")
devtools::install_github("siddheshdhuri/sentimentAnalysis")

#' you might face error like cannot move '/usr/local/lib/R/site-library/DBI' to #' #'/usr/local/lib/R/site-library/00LOCK-DBI/DBI': Permission denied. 
#' you just need to change permissions.

#' To enable web scrapping you need to install and set up Selenium server
#' and also phanjomjs browser
