firedf <- FireData
janjune <- WeatherFirstHalf
juldec <- WeatherSecondHalf


##########Combine and clean weather data and fix daylight savings###############

#Combines the weather dfs together 
df <- rbind.data.frame(janjune,juldec)
df

#Removes the fake date from the time column
df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M:%S")
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
firedf$Time <- as.POSIXct(firedf$Time, format = "%Y-%m-%d %H:%M:%S")
firedf$Time <- format(firedf$Time, format = "%H:%M:%S")
firedf
##### Removed UTC stamp from date cells
firedf$Date <- sub('UTC','',firedf$Date)
firedf

################################################################################
######## Attach needed rows to the proper FireData rows ########################
################################################################################
## converts the date as a string to actual time object in a new vector
firedf$Time <- as.POSIXct(firedf$Time, format = "%H:%M:%S")
weather$Time <- as.POSIXct(weather$Time, format = "%H:%M:%S")

firedf$Date

fulldata<-firedf
for(i in 1:ncol(weather)){
  fulldata[,i+ncol(firedf)]<-NA
  names(fulldata)[i+ncol(firedf)]<-names(weather)[i]
}

fulldata<-as.data.frame(fulldata)
head(fulldata)
weather<-as.data.frame(weather)

for (i in (1:nrow(firedf))){
  #selects rows where dates line up
  potiental_row_index <- which(weather$Date == firedf$Date[i])
  potiential_time <- weather$Time[potiental_row_index]
  abs_diff_times <- abs(difftime(firedf$Time[i],potiential_time))
  closest_time_row_index_abs <- which.min(abs_diff_times)
  
  a <- potiental_row_index[closest_time_row_index_abs]
  ifelse(length(potiental_row_index)==0,NA,fulldata[i,8:ncol(fulldata)]<-weather[a,])
}


##########################################################################


