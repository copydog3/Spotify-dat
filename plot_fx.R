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
    scale_fill_gradient(low = "#808080",
                        high = "#8B0000") +
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
    scale_fill_gradient(low = "#808080",
                        high = "#8B0000") +
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