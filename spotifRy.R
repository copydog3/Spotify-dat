### Install required packages (you only need to run this once)
install.packages("rstudioapi")
install.packages("jsonlite")
install.packages("data.table")

### Load libraries
library("jsonlite")
library("data.table")

### Convert your data
script.dir = dirname(rstudioapi::getActiveDocumentContext()$path)
json_data = rbindlist(list(
  jsonlite::fromJSON(paste0(script.dir,"/Data/StreamingHistory0.json")),
  jsonlite::fromJSON(paste0(script.dir,"/Data/StreamingHistory1.json")),
  jsonlite::fromJSON(paste0(script.dir,"/Data/StreamingHistory2.json"))))
