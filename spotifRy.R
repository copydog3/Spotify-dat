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

###### Let's define some functions we'll use later

# dt = json_data
# plot_name = NULL
map_listens = function(dt,plot_name = NULL){
  
  dt = dt[order(endTime)
  ][,timeofday := as.character(paste0(substr(endTime,12,13),":00"))
  ][,timeofday := as.POSIXct(timeofday,format = "%H:%M",tz = my_tz)
  ][,month := factor(month.abb[month(endTime)],
                     levels = month.abb)]
  plot_dt = dt[,.(h_tot = sum(msPlayed)/3.6e6),
               by = .(timeofday,month)][rev(order(month))]
  
  q = ggplot(plot_dt,
             aes(x = timeofday, y = month, fill = h_tot)) +
    geom_tile(colour = "white") +
    scale_x_datetime(labels = scales::date_format("%H:%M",
                                                  tz = my_tz)) +
    scale_fill_gradient(low = AEMOColours[["lightGrey"]],
                        high = AEMOColours[["red"]]) +
    # xlab("") +
    # ylab("") + 
    labs(x = "",y = "",fill = "Total hours") +
    theme_classic()
  print(q)
  
  if(!is.null(plot_name)){
    jpeg(paste0(plot_name,".jpg"),width=1200,height=900,res=100,pointsize=15,quality = 100)
    print(q)
    dev.off()
  }
}

map_artists = function(dt,sel_artists,filter_type,plot_name = NULL){
  year_fav = dt[order(endTime) & artistName %in% sel_artists
  ][,month := factor(month.abb[month(endTime)],
                     levels = month.abb)]
  if(tolower(filter_type) == "total hours"){
    year_fav = year_fav[,.(V1 = sum(msPlayed)/3.6e6),
                        by = .(artistName,month)]
    fill_lab = "Total hours"
  } else if (tolower(filter_type) == "unique songs"){
    year_fav = year_fav[,.(V1 = .N),
                        by = .(trackName,artistName,month)
    ][,.(V1 = .N),
      by = .(artistName,month)]
    fill_lab = "Unique songs"
  } else {
    print("Please pick 'Total hours' or 'Unique songs'")
  }
  year_fav[,artistName := factor(artistName,levels = rev(sel_artists))]
  
  q = ggplot(year_fav,
             aes(x = month, y = artistName, fill = V1)) +
    geom_tile(colour = "white") +
    scale_fill_gradient(low = AEMOColours[["lightGrey"]],
                        high = AEMOColours[["red"]]) +
    labs(x = "",y = "",fill = fill_lab) +
    theme_classic()
  print(q)
  if(!is.null(plot_name)){
    jpeg(paste0(plot_name,".jpg"),width=1200,height=900,res=100,pointsize=15,quality = 100)
    print(q)
    dev.off()
  }
}

track_times = function(dt){
  q = ggplot(dt,
             aes(x = ms_tot/3.6e6,y = trackName)) +
    geom_col() +
    labs(y = "",x = "Listening hours") +
    theme_classic()
  print(q)
}
###### Set your working directory
script.dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script.dir)
ddpcr::quiet(source("C:/Users/dho/OneDrive - Australian Energy Market Operator/Desktop/R/AEMOtemplate.R"),all = T)

### Read your json data into R and convert it into a datatable (similar to python pandas dataframe)
# Note: it's good practice to name the library you're using
# just before you call a function from the library, eg. data.table::setDT()
# this is to prevent confusion if two libraries have identical function names
json_data = data.table::rbindlist(list(
  jsonlite::fromJSON(paste0(script.dir,"/Data/StreamingHistory0.json")),
  jsonlite::fromJSON(paste0(script.dir,"/Data/StreamingHistory1.json")),
  jsonlite::fromJSON(paste0(script.dir,"/Data/StreamingHistory2.json"))))

# this is an example of making changes 'in place'
# doing this is really handy for large tables
json_data[,endTime := as.POSIXct(endTime,
                                 format = "%Y-%m-%d %H:%M",
                                 tz = "UTC")]
# your Spotify data is in UTC, which is pretty different to Australian time!
# UTC is also R's default timezone
# if you're ever using timeseries data in R, be sure to check the timezone
# here's a handy way to check what timezone your data is in
attr(json_data$endTime,"tzone")

# let's change the timezone into something more understandable
# naming the city you're in works as well
my_tz = "Australia/Perth"
json_data[,endTime := lubridate::with_tz(endTime,my_tz)]

###### Analysis time!

### Here's the 5 songs you spent the most time listening to:
song_time = json_data[,.(ms_tot = sum(msPlayed)),
                      by = .(artistName,trackName)
][rev(order(ms_tot))]
print(head(song_time,5))

### Here's the 5 artists you spent the most time listening to:
fave_artist = json_data[,.(ms_tot = sum(msPlayed)),
                        by = .(artistName,trackName)
][,.(ms_artist = sum(ms_tot),
     unique_s = .N),
  by = .(artistName)
][rev(order(ms_artist))]
fave_artist[,long_listen := as.numeric(rownames(fave_artist))]
# fave_artist = fave_artist[, rank_ms := order(artistName),
#                           by = list(ms_artist)]
artist_time = head(fave_artist,5)
print(artist_time)
# Typically Spotify uses this to determine your favourite artist on Spotify Wrapped
# What about songs you listen to on repeat for a whole month, then never again?
# (I do this a lot)
# Artist range is also a cool stat to check out:
artist_unique = head(fave_artist[rev(order(unique_s))],5)
print(artist_unique)
# I get pretty different results! (Except for Linkin Park)

### Most artists are flavours of the month

map_listens(json_data,NULL)
map_listens(json_data[artistName %in% artist_unique$artistName],NULL)
map_listens(json_data[artistName %in% artist_time$artistName],NULL)
map_listens(json_data[artistName == "Linkin Park"],NULL)

map_artists(json_data,artist_time$artistName,"Unique songs")

LP_faves = song_time[artistName == "Linkin Park"]
LP_faves = LP_faves[,trackName:= factor(trackName,
                                        levels = rev(LP_faves$trackName))]
track_times(LP_faves[1:10])

fave_reps = song_time[1:10][,trackName:= factor(trackName,
                                                levels = rev(song_time$trackName[1:10]))]
track_times(fave_reps)

cut_9thile = fave_artist[
  unique_s > quantile(fave_artist$unique_s,
                       probs = c(0.99),na.rm = T)
  # ms_artist > quantile(fave_artist$ms_artist,
  #                      probs = c(0.99),na.rm = T)
][,h_artist := ms_artist/3.6e6]
m <- ggplot(cut_9thile, #[artistName != "Linkin Park"],
            aes(x = unique_s,
                y = h_artist)) +
  geom_jitter(size = 1.5) +
  ggrepel::geom_text_repel(aes(label = artistName),
                           size = 3)
print(m)

