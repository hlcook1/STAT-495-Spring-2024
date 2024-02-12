

## STAT495 Capstone Project
## 12 February 2024
## Joseph Russler

## Data Cleaning

## Data
firedf = fulldata
janjune = read.csv("C:/Users/jjrus/OneDrive - University of Southern Indiana/Documents/University of Southern Indiana/Spring 2024/STAT495/Ryan Weather Data.csv")
juldec = read.csv("C:/Users/jjrus/OneDrive - University of Southern Indiana/Documents/University of Southern Indiana/Spring 2024/STAT495/Joey Weather Data.csv")

## Assign natures to a type of event
Fires = which(firedf$Nature == "GARAGE" | firedf$Nature == "WORKFIRE" | firedf$Nature == "TRASH" | firedf$Nature == "2 ALARM" | firedf$Nature == "GRASS" | firedf$Nature == "STRU" | firedf$Nature == "CSTRU" | firedf$Nature == "CAR")
Rescues = which(firedf$Nature == "WATER" | firedf$Nature == "ROPE" | firedf$Nature == "EXTRIC" | firedf$Nature == "LOCK")
Alarms = which(firedf$Nature == "RALM" | firedf$Nature == "CALM" | firedf$Nature == "CARB")
Medical = which(firedf$Nature == "MEDIC" | firedf$Nature == "CARBI" | firedf$Nature == "SUCIE" | firedf$Nature == "SUICV")
Vehicle_Accident = which(firedf$Nature == "MVA" | firedf$Nature == "MVAU" | firedf$Nature == "MVAI" | firedf$Nature == "HRIN" | firedf$Nature == "HRUN")
Material_Cleanup = which(firedf$Nature == "HAZMAT" | firedf$Nature == "CLEANF")
Welfare_Check = which(firedf$Nature == "CHKWL" | firedf$Nature == "DRUNK" | firedf$Nature == "PDOWN")
Investigations = which(firedf$Nature == "INVEST" | firedf$Nature == "INVES")
Agency_Assist = which(firedf$Nature == "POLICE" | firedf$Nature == "HOST" | firedf$Nature == "DVINP" | firedf$Nature == "DA" | firedf$Nature == "DBEPD" | firedf$Nature == "NOTIF" | firedf$Nature == "MA" | firedf$Nature == "AIRSB" | firedf$Nature == "WIRES")
Remove = which(firedf$Nature == "MESG")

## Create a new column with the type of event for each row
Nature = firedf$Nature
Nature[Fires] = "Fires"
Nature[Rescues] = "Rescues"
Nature[Alarms] = "Alarms"
Nature[Medical] = "Medical"
Nature[Vehicle_Accident] = "Vehicle Accident"
Nature[Material_Cleanup] = "Material Cleanup"
Nature[Welfare_Check] = "Welfare Check"
Nature[Investigations] = "Investigations"
Nature[Agency_Assist] = "Agency Assist"
Nature[Remove] = "Remove"
firedf$Nature = Nature

## Replace the nature column with the new type of event column
Remove = which(firedf$Nature == "Remove")
firedf = firedf[-which(firedf$Nature == "Remove"),]

## Replace Rept # with index column
Index = c(1:11410)
firedf$`Rept #` = Index

## Remove the date column
firedf = firedf[c(1,3:7, 9:(ncol(firedf)-1))]

## Create new column names for the data frame
colnames(firedf) = c("Index", "DateTime", "TypeOfEvent", "Location", "PrimeUnit", "Employee", "Temperature", "DewPoint", "Humidity", "Wind", "WindSpeed", "WindGust", "Pressure", "Precip.", "Condition")

## Fix for the Prime Unit column
PrimeUnit = read.csv("C:/Users/jjrus/OneDrive - University of Southern Indiana/Documents/University of Southern Indiana/Spring 2024/STAT495/Event Listing Data.csv")
PrimeUnit = PrimeUnit$PrimeUnit
PrimeUnit = PrimeUnit[-Remove]
firedf$PrimeUnit = PrimeUnit

## Create a column for type of truck and responding station
Truck = vector(mode="character", length=0)
i = 0

while (i < nrow(firedf)) {
  i = i + 1
  working = firedf$PrimeUnit[i]
  working = substr(working, 2, 2)
  if (working == "E") {
    Truck[i] = "Engine"
  } else if (working == "Q") {
    Truck[i] = "Quint"
  } else if (working == "L") {
    Truck[i] = "Ladder"
  } else {
    Truck[i] = working
  }
}

Station = vector(mode="character", length=0)
i = 0

while (i < nrow(firedf)) {
  i = i + 1
  working = firedf$PrimeUnit[i]
  working = substr(working, 3, nchar(working))
  working = gsub(" ", "", working)
  if (substr(working, 1, 2) == "90") {
    working = substr(working, 3, nchar(working))
    working = paste("Station", working)
    Station[i] = working
  } else {
    working = paste("Station", working)
    Station[i] = working
  }
}

firedf = cbind(firedf[,1:4], Truck, Station, firedf[,6:ncol(firedf)])

## Write a csv for the new data frame
write.csv(firedf, "firedf.csv", row.names=FALSE)