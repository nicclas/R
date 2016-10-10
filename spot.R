# install.packages("RPostgreSQL")
# install.packages("ggplot2")
# install.packages("ggmap")
# install.packages("leaflet")
# install.packages("dplyr")

require("RPostgreSQL")
library("ggplot2")
library("ggmap")
library("leaflet")
library("dplyr")
library("geosphere")

options(digits=12)

pw <- {
  "0347";
  
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
con <- dbConnect(drv, dbname = "clryd", host = "localhost", port = 5433, user = "postgres", password = pw)
rm(pw) # removes the password

typecolors=c('#ff0000','#00ff00','#0000ff','a00000','#00a000','#0000a0','#000000','#808080','#ffff00','#00ffff','#a0a000','#00a0a0','#600000','#006000','#000060','#606000','#006060')

df_modes <- dbGetQuery(con, "SELECT * from transportation_modes")

# Get all user numbers 
df_users <- dbGetQuery(con, "SELECT id from user_table join trips_gt on (user_table.id=trips_gt.user_id) group by user_table.id having count(*) > 0 order by user_table.id asc")

# Ask which user
print(df_users$id)
user = readline(prompt="Enter user number: ")

# get data for specific user
sqls = paste0("SELECT username, gender, age FROM user_table, user_table_pp WHERE username=email AND id = ",user)
df_pdata <- dbGetQuery(con, sqls)
cat(paste0("Gender: ",df_pdata$gender, " age: ",df_pdata$age," (Name: ",df_pdata$username,")\n"))


# get trip for specific user
sqls = paste0("SELECT * from trips_gt where user_id = ",user)
df_trips <- dbGetQuery(con, sqls)

cat(paste0("This user has ",nrow(df_trips)," trip(s)\n"))

if(nrow(df_trips)==0){
  dbDisconnect(con)
  stop("No tips", call. = FALSE)
}

cat(paste0("Trips span: ",1+floor(((df_trips[nrow(df_trips),'to_time']-df_trips[1,'from_time'])/(86400*1000)))," day(s)\n"))

df_allpos = 0
rm(df_allpos)

for(t in 1:nrow(df_trips)){
  # Get the t:th trip 
  df_t <- df_trips[t,c('from_time','to_time')]

  sql_legs = paste0("SELECT * FROM triplegs_gt WHERE user_id = ",user," AND trip_id = '",df_trips$trip_id[t],"'")
  df_legs <- dbGetQuery(con, sql_legs)

  for(i in 1:nrow(df_legs)){
    if(df_legs[i,'transportation_type']>0){

      df_legstime <- df_legs[i,c('from_time','to_time')]

      sqls = paste("SELECT * FROM collected_locations WHERE user_id = ",user," AND time_ >=",df_legstime[1],"AND time_ <=",df_legstime[2],sep=" ")
      df_line <- dbGetQuery(con, sqls)

      if(nrow(df_line) > 0){
        df_pos <- df_line[c('lat_','lon_','time_')] 
      
        # Copy data frame and convert to SpatialPoints
        df_sppos <- df_pos
        coordinates(df_sppos)<- ~ lon_ + lat_
        proj4string(df_sppos) <- CRS("+proj=longlat +datum=WGS84")
        dists <- sapply(seq_along(df_sppos[-1, ]), function(i) spDistsN1(pts = df_sppos[i, ], pt = df_sppos[i+1, ], longlat = TRUE))
        
        if(class(dists) != "numeric") dists = 0

        
        cat(paste0("Trip: ",t,", leg ", i," of ",nrow(df_legs)," of length "))
        cat(paste0(round(sum(dists), digits=2)," km, mode: ",df_modes$name_[df_legs[i,'transportation_type']]," (",nrow(df_pos)," point(s)), at: ",as.POSIXct(df_legs[i,"from_time"]/1000, origin="1970-01-01")," to ",as.POSIXct(df_legs[i,"to_time"]/1000, origin="1970-01-01"),"\n" ))

        df_pos$typecolor = typecolors[df_legs[i,'transportation_type']]
        df_pos$mode = df_legs[i,'transportation_type']
        df_pos$leg = i
        df_pos$trip = t
        
        if(!exists("df_allpos")){
          df_allpos = df_pos[FALSE,]
        }
        df_allpos=rbind(df_allpos,df_pos)
        m = leaflet(df_allpos) %>% addTiles() %>% addCircles(~lon_, ~lat_, popup = sprintf("Trip: %s Leg: %s Mode: %s Time: %s",df_allpos$trip,df_allpos$leg, df_modes$name_[df_allpos$mode], as.character(as.POSIXct((df_allpos$time_/1000), origin="1970-01-01"))) , color=~typecolor, radius=20, stroke = TRUE, fillOpacity = 0.6)
        
        print(m)
      }else{
        cat(paste0("No GPS points found for the tripleg/trip\n"))  
      }
    }else{
    cat(paste0("No transportation type\n"))  
    }
  }
  enter = readline(prompt="Press enter.")
  
}


dbDisconnect(con)


