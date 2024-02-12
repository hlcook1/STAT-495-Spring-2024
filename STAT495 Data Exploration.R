

## STAT495 Capstone Project
## 12 February 2024
## Joseph Russler

## Data Exploration

## Activate libraries
library(grid); library(gridExtra)

## Create a pie chart of types of events
slices = table(firedf$TypeOfEvent)
pct = round(slices/sum(slices)*100)
labels = sort(unique(firedf$TypeOfEvent))
labels = paste(labels, pct)
labels = paste(labels, "%", sep="")
pie(slices, labels=labels, main="Pie Chart of Types of Events")

## Create a table for types of event
TypeOfEvent_tabledata = data.frame(sort(unique(firedf$TypeOfEvent)), slices)
TypeOfEvent_tabledata = TypeOfEvent_tabledata[,2:3]
colnames(TypeOfEvent_tabledata) = c("Type of Event", "Frequency")
grid.newpage()
grid.draw(tableGrob(TypeOfEvent_tabledata, rows=NULL))

## Create a pie chart of responses by station
slices = table(firedf$Station)
pct = paste(round(slices/sum(slices)*100))
labels = sort(unique(firedf$Station))
pct = paste("(", pct, "%", ")", sep="")
labels = paste(labels, pct, sep=" ")
pie(slices, labels=labels, main="Pie Chart of Number of Calls by Station")

## Create a table for responses by station
Station_tabledata = data.frame(sort(unique(firedf$Station)), slices)
Station_tabledata = Station_tabledata[,2:3]
colnames(Station_tabledata) = c("Station", "Number of Calls")
grid.newpage()
grid.draw(tableGrob(Station_tabledata, rows=NULL))

## Create a five number summary for temperature
Temperature_summary = as.numeric(substr(firedf$Temperature, 1, 2))
Temperature_summarynum = as.numeric(summary(Temperature_summary))
Temperature_summarycolnames = names(summary(Temperature_summary))
Min. = Temperature_summarynum[1]
"1st Qu" = Temperature_summarynum[2]
Median = Temperature_summarynum[3]
Mean = round(Temperature_summarynum[4], 2)
"3rd Qu." = Temperature_summarynum[5]
Max. = Temperature_summarynum[6]
"Na's" = Temperature_summarynum[7]
Temperature_summarydf = cbind(`1st Qu`, Median, Mean, `3rd Qu.`, Max., `Na's`)
grid.newpage()
grid.draw(tableGrob(Temperature_summarydf, rows=NULL))

## Create a five number summary for humidity
Humidity_summary = as.numeric(substr(firedf$Humidity, 1, 2))
Humidity_summarynum = as.numeric(summary(Humidity_summary))
Humidity_summarycolnames = names(summary(Humidity_summary))
Min. = Humidity_summarynum[1]
"1st Qu" = Humidity_summarynum[2]
Median = Humidity_summarynum[3]
Mean = round(Humidity_summarynum[4], 2)
"3rd Qu." = Humidity_summarynum[5]
Max. = Humidity_summarynum[6]
"Na's" = Humidity_summarynum[7]
Humidity_summarydf = cbind(`1st Qu`, Median, Mean, `3rd Qu.`, Max., `Na's`)
grid.newpage()
grid.draw(tableGrob(Humidity_summarydf, rows=NULL))

## Create a five number summary for wind speed
WindSpeed_summary = as.numeric(substr(firedf$WindSpeed, 1, 2))
WindSpeed_summarynum = as.numeric(summary(WindSpeed_summary))
WindSpeed_summarycolnames = names(summary(WindSpeed_summary))
Min. = WindSpeed_summarynum[1]
"1st Qu" = WindSpeed_summarynum[2]
Median = WindSpeed_summarynum[3]
Mean = round(WindSpeed_summarynum[4], 2)
"3rd Qu." = WindSpeed_summarynum[5]
Max. = WindSpeed_summarynum[6]
"Na's" = WindSpeed_summarynum[7]
WindSpeed_summarydf = cbind(`1st Qu`, Median, Mean, `3rd Qu.`, Max., `Na's`)
grid.newpage()
grid.draw(tableGrob(WindSpeed_summarydf, rows=NULL))

## Create a five number summary for wind speed
WindSpeed_summary = as.numeric(substr(firedf$WindSpeed, 1, 2))
WindSpeed_summarynum = as.numeric(summary(WindSpeed_summary))
WindSpeed_summarycolnames = names(summary(WindSpeed_summary))
Min. = WindSpeed_summarynum[1]
"1st Qu" = WindSpeed_summarynum[2]
Median = WindSpeed_summarynum[3]
Mean = round(WindSpeed_summarynum[4], 2)
"3rd Qu." = WindSpeed_summarynum[5]
Max. = WindSpeed_summarynum[6]
"Na's" = WindSpeed_summarynum[7]
WindSpeed_summarydf = cbind(`1st Qu`, Median, Mean, `3rd Qu.`, Max., `Na's`)
grid.newpage()
grid.draw(tableGrob(WindSpeed_summarydf, rows=NULL))

## Create a five number summary for precipitation
Precipitation_summary = as.numeric(substr(firedf$Precip., 1, 3))
Precipitation_summarynum = as.numeric(summary(Precipitation_summary))
Precipitation_summarycolnames = names(summary(Precipitation_summary))
Min. = Precipitation_summarynum[1]
"1st Qu" = Precipitation_summarynum[2]
Median = Precipitation_summarynum[3]
Mean = round(Precipitation_summarynum[4], 2)
"3rd Qu." = Precipitation_summarynum[5]
Max. = Precipitation_summarynum[6]
"Na's" = Precipitation_summarynum[7]
Precipitation_summarydf = cbind(`1st Qu`, Median, Mean, `3rd Qu.`, Max., `Na's`)
grid.newpage()
grid.draw(tableGrob(Precipitation_summarydf, rows=NULL))

## Create correlation matrices for the numeric variables
Remove = sort(unique(c(which(is.na(firedf$Temperature)), which(is.na(firedf$Humidity)), which(is.na(firedf$WindSpeed)), which(is.na(firedf$Precip.)))))
cor(Temperature_summary, Humidity_summary, WindSpeed_summary, Precipitation_summary)
