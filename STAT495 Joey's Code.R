

## STAT495 Capstone Project
## 31 January 2024
## Joseph Russler

## Data
event_listings = read.csv("C:/Users/jjrus/OneDrive - University of Southern Indiana/Documents/University of Southern Indiana/Spring 2024/STAT495/Event Listing Data.csv")

## Assign natures to a type of event
Fires = which(event_listings$Nature == "GARAGE" | event_listings$Nature == "WORKFIRE" | event_listings$Nature == "TRASH" | event_listings$Nature == "2 ALARM | event_listings$Nature == GRASS" | event_listings$Nature == "STRU" | event_listings$Nature == "CSTRU" | event_listings$Nature == "CAR")
Rescues = which(event_listings$Nature == "WATER" | event_listings$Nature == "ROPE" | event_listings$Nature == "EXTRIC" | event_listings$Nature == "LOCK")
Alarms = which(event_listings$Nature == "RALM" | event_listings$Nature == "CALM" | event_listings$Nature == "CARB")
Medical = which(event_listings$Nature == "MEDIC" | event_listings$Nature == "CARBI" | event_listings$Nature == "SUCIE" | event_listings$Nature == "SUCIV")
Vehicle_Accident = which(event_listings$Nature == "MVA" | event_listings$Nature == "MVAU" | event_listings$Nature == "MVIA" | event_listings$Nature == "HRIN" | event_listings$Nature == "HRUN")
Material_Cleanup = which(event_listings$Nature == "HAZMAT" | event_listings$Nature == "CLEANF")
Welfare_Check = which(event_listings$Nature == "CHKWL" | event_listings$Nature == "DRUNK" | event_listings$Nature == "PDOWN")
Investigations = which(event_listings$Nature == "INVEST" | event_listings$Nature == "INVES")
Agency_Assist = which(event_listings$Nature == "POLICE" | event_listings$Nature == "HOST" | event_listings$Nature == "DVINP" | event_listings$Nature == "DA" | event_listings$Nature == "DBEPD" | event_listings$Nature == "NOTIF" | event_listings$Nature == "MA" | event_listings$Nature == "AIRSB" | event_listings$Nature == "WIRES")
Remove = which(event_listings$Nature == "MESG")

## Create a new column with the type of event for each row
Nature = event_listings$Nature
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
event_listings$Nature = Nature

## Replace the nature column with the new type of event column
event_listings = event_listings[-which(event_listings$Nature == "Remove"),]
colnames(event_listings) = c("Index", "Date", "Time", "TypeOfEvent", "Location", "PrimeUnit", "Employee")

