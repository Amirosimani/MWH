library(stats)
library(dplyr)
library(DBI)
library(RSQLite)
library(lubridate)
library(ggplot2)

#Set up connection to the SQLite database
connection <- dbConnect(RSQLite::SQLite(), dbname = "clinton.sqlite")

#Print all tables
print("Tables")
all_tables <-  dbListTables(connection)
print(all_tables)

#Print information about 'docs' table
docs <- dbGetQuery(connection, sprintf("SELECT * FROM %s", "docs"))
print("Column Name")
print(colnames(docs))
print(sprintf("Number of Rows: %d", nrow(docs)))


#parsing date
  
  arrive <- ymd_hms(docs$date, tz = "Pacific/Auckland")
  time <- hour(arrive)
  time <-time[complete.cases(time)]
  
  date <- as.Date(as.POSIXct(strptime(docs$date, "%Y-%m-%d %H:%M:%S")))
  date <-date[complete.cases(date)]
  
  stripped_date = data.frame(date, time)
  sorted_data <- stripped_date[order(date),]

year2009 <- subset(sorted_data, format(date,'%Y') %in% c('2009','2010'))
year2010 <- subset(sorted_data, format(date,'%Y') %in% c('2010','2011'))
year2011 <- subset(sorted_data, format(date,'%Y') %in% c('2011','2012'))
year2012 <- subset(sorted_data, format(date,'%Y') %in% c('2012','2013'))

par(mfrow=c(2,2))
hist(year2009$time, breaks = 24, freq = T, main = paste("2009"), xlim = c(0,24),ylim = c(0,2000))
hist(year2010$time, breaks = 24, freq = T, main = paste("2010"), xlim = c(0,24),ylim = c(0,2000))
hist(year2011$time, breaks = 24, freq = T, main = paste("2011"), xlim = c(0,24),ylim = c(0,2000))
hist(year2012$time, breaks = 24, freq = T, main = paste("2012"), xlim = c(0,24), ylim = c(0,2000))

#qplot(stripped_date$time, geom="histogram") 


#Clean up connection to the database
dbDisconnect(connection)

