###### Install required packages (you only need to run this once)
# install.packages("rstudioapi")
# install.packages("jsonlite")
# install.packages("data.table")

### Load libraries
library("jsonlite")
library("data.table")
library("ggplot2")
library("lubridate")
library("plyr")
library("dplyr")

###### Set your working directory
script.dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script.dir)

### Grab some plot functions we'll use later
ddpcr::quiet(source("plot_fx.R"),all = T)

### Read your json data into R and convert it into a datatable (similar to python pandas dataframe)
# # Note: it's good practice to name the library you're using
# # just before you call a function from the library, eg. data.table::setDT()
# # this is to prevent confusion if two libraries have identical function names

temp_list = base::list.files(paste0(script.dir,"/Data"),
                             pattern = "endsong*")
json_data = data.table()
for(i in seq(1,length(temp_list))){
  json_data = data.table::rbindlist(list(
    json_data,
    jsonlite::fromJSON(paste0(script.dir,"/Data/",temp_list[i]))))
}
keep_cols = c("ts","ms_played","master_metadata_track_name",
              "master_metadata_album_artist_name",
              "master_metadata_album_album_name",
              "reason_start","reason_end","shuffle","skipped")
json_data = json_data[,..keep_cols]
json_data[,ts := as.POSIXct(ts,
                            format = "%Y-%m-%dT%H:%M:%SZ",
                            tz = "UTC")]
attr(json_data$ts,"tzone")
# let's change the timezone into something more understandable
# naming the city you're in works as well
my_tz = "Australia/Perth"
json_data[,ts := lubridate::with_tz(ts,my_tz)
][,year := lubridate::year(ts)]
json_data = json_data[order(ts)]

names(json_data) = gsub(pattern = "master_metadata_|master_metadata_album_","",
                        colnames(json_data))
json_data = json_data[!is.na(track_name)]

###### Analysis
fartist = json_data[,.(fart_time = round(sum(ms_played,na.rm = T)/3.6e6,1)),
                    by = .(artist_name,year)][rev(order(fart_time))]
fartist = fartist[fartist[,.I[1], by = year]$V1][order(year)]
print(fartist)

# strip off one song faves by artist
fart_song = json_data[,.(h_tot = sum(ms_played,na.rm = T)/3.6e6),
                      by = .(track_name,artist_name,album_name)
][rev(order(h_tot))]
fart_song = fart_song[fart_song[,.I[1], by = artist_name]$V1]
rep1_unbi = json_data[!fart_song,
                      on = .(track_name,artist_name,album_name)]
fartist_unbi = rep1_unbi[,.(fart_time = round(sum(ms_played,na.rm = T)/3.6e6,1)),
                         by = .(artist_name,year)][rev(order(fart_time))]
fartist_unbi = fartist_unbi[fartist_unbi[,.I[1], by = year]$V1][order(year)]
print(fartist_unbi)

plot_heartbeat(json_data,"qtr",
               c(fartist_unbi$artist_name[1],
                 fartist_unbi$artist_name[5],
                 fartist_unbi$artist_name[9]))
               # c(fartist$artist_name[1],
               #   fartist$artist_name[5],
               #   fartist$artist_name[9]))

