library(stats)
library(dplyr)
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
for (i in 1:nrow(docs)) {
  
  
  arrive <- ymd_hms(docs$date, tz = "Pacific/Auckland")
  time <- hour(arrive)
  time <-time[complete.cases(time)]
  
  date <- as.Date(as.POSIXct(strptime(docs$date, "%Y-%m-%d %H:%M:%S")))
  date[complete.cases(date)]
  date <-date[complete.cases(date)]
  
  stripped_date = data.frame(date, time)
}



#Clean up connection to the database
dbDisconnect(connection)

