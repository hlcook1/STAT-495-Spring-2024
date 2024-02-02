firedf <- FireData
janjune <- WeatherFirstHalf
juldec <- WeatherSecondHalf


##########Combine and clean weather data and fix daylight savings###############
#Combines the weather dfs together 
df <- rbind.data.frame(janjune,juldec)
df

#Removes the fake date from the time column
df$Time <- as.POSIXlt(df$Time, format = "%Y-%m-%d %H:%M:%S")
df$Time <- format(df$Time, format = "%H:%M:%S")
print(df)

#march 12 - nov 5 is daylight savings, these days need +1hr on weather data

df$DateTime <- as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")

# Convert Date to POSIXct for comparison
date_start <- as.POSIXct("2023-03-12", tz = "UTC")
date_end <- as.POSIXct("2023-11-02", tz = "UTC")
date_range_condition <- df$DateTime >= date_start & df$DateTime <= date_end

# Add 3600 seconds to the DateTime for the daylight savings rows
df$DateTime[date_range_condition] <- df$DateTime[date_range_condition] + 3600

# Extract Date and Time columns again
df$Date <- as.Date(df$DateTime)
df$Time <- format(df$DateTime, format="%H:%M:%S")

#Removes new DateTime column
df <- df[, !names(df) %in% c("DateTime")]

weather <- df

####### Removes fake date from time column
firedf$Time <- as.POSIXlt(firedf$Time, format = "%Y-%m-%d %H:%M:%S")
firedf$Time <- format(firedf$Time, format = "%H:%M:%S")
firedf
##### Removed UTC stamp from date cells
firedf$Date <- sub('UTC','',firedf$Date)
firedf

################################################################################
######## Attach needed rows to the proper FireData rows ########################


## converts the date as a string to actual time object in a new vector
fire_t <- as.POSIXct(firedf$Time, format = "%H:%M:%S")
weather_t <- as.POSIXct(weather$Time, format = "%H:%M:%S")



for (i in firedf$Time){
  #selects rows where dates line up
  potiental_row<- which(weather$Date == firedf$Date[i])
  work <- weather_t[potiental_row]
  
  for (l in work){
    elapsed <- difftime(fire_t[i], work[l], unit = 'min')
  
    }
  selected_row <- which(potiential_row == min(elapsed))
}



working <- which(weather$Date == firedf$Date[1])
working
abba<- weather_t[working]
abba

a <- difftime(fire_t[1],weather_t[1], units = 'min')
b <- difftime(fire_t[1],weather_t[2], units = 'min')

q <- c(a,b)
a
b
min(q)


