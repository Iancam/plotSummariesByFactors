library(lubridate)
library(dplyr)
library(geosphere)
df <- read.csv("/Users/iancampbell/Developer/DataAnalytics/plotSummariesByFactors/inst/extdata/divvySample.csv")
df$started_at = ymd_hms(df$started_at)
df$ended_at = ymd_hms(df$ended_at)

# get ride duration
df$ride_duration = df$ended_at-df$started_at
# add day of week, month and hour
df$day_of_week = df$started_at %>% wday(label=TRUE)
df$month = df$started_at %>% month(label=TRUE)
df$hour = hour(df$started_at)
df$year = year(df$started_at)
#get distance
df$distance_meters = distHaversine(
  data.matrix(df[c("start_lng", "start_lat")]),
  data.matrix(df[c("end_lng", "end_lat")]), 
  r = 6378137) # distance in meters

df = df[!(is.na(df$end_lat) | is.na(df$end_lng)),]

write.csv(df, "/Users/iancampbell/Developer/DataAnalytics/plotSummariesByFactors/inst/extdata/divvySample.csv", row.names = FALSE)
