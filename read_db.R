library(stats)
library(dplyr)
library(DBI)
library(RSQLite)
library(lubridate)
library(NLP)
library(tm)

#Set up connection to the SQLite database
connection <- dbConnect(RSQLite::SQLite(), dbname = "clinton.sqlite")
all_tables <-  dbListTables(connection)
print(all_tables)

#Print information about 'docs' table
docs <- dbGetQuery(connection, sprintf("SELECT * FROM %s", "docs"))

#parsing dates
arrive <- ymd_hms(docs$date, tz = "Pacific/Auckland")
time <- hour(arrive)
#time <-time[complete.cases(time)]
date <- as.Date(as.POSIXct(strptime(docs$date, "%Y-%m-%d %H:%M:%S")))
#date <-date[complete.cases(date)]
  
stripped_date = data.frame(date, time, docs$body)
sorted_data <- stripped_date[order(date),]

print(docs$body[1])
"
year2009 <- subset(sorted_data, format(date,'%Y') %in% c('2009','2010'))
year2010 <- subset(sorted_data, format(date,'%Y') %in% c('2010','2011'))
year2011 <- subset(sorted_data, format(date,'%Y') %in% c('2011','2012'))
year2012 <- subset(sorted_data, format(date,'%Y') %in% c('2012','2013'))

par(mfrow=c(2,2))
hist(year2009$time, breaks = 24, freq = T, main = paste("2009"), xlim = c(0,24),ylim = c(0,2000))
hist(year2010$time, breaks = 24, freq = T, main = paste("2010"), xlim = c(0,24),ylim = c(0,2000))
hist(year2011$time, breaks = 24, freq = T, main = paste("2011"), xlim = c(0,24),ylim = c(0,2000))
hist(year2012$time, breaks = 24, freq = T, main = paste("2012"), xlim = c(0,24), ylim = c(0,2000))
"

selected_keyword <- filter(sorted_data, grepl("settlements",sorted_data$docs.body))
txt = NULL
for (i in 1:nrow(selected_keyword)) {
  txt <- rbind(txt, regmatches(selected_keyword$docs.body[i], regexpr("(nSubject:).*(\\n)?", selected_keyword$docs.body[i]), invert = F))
}

corpus <- Corpus(DataframeSource(txt))
coprus <- tm_map(txt, removePunctuation)
coprus <-data.frame(text=unlist(sapply(txt, `[`, "content")), stringsAsFactors=F)

#TM Corpus




#network graph
#there are many emails whitout @
#regmatches(docs$body[1], regexpr("(nTo:).*(\\n)?", docs$body[1]), invert = F)

for (i in 1:nrow(docs)){ 
emails <- unlist(regmatches( docs$body[i], gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))",  docs$body[i])))
}

dbDisconnect(connection)

