library(stats)
library(dplyr)
library(DBI)
library(RSQLite)
library(lubridate)
library(NLP)
library(tm)
library(SnowballC)

#Set up connection to the SQLite database
connection <- dbConnect(RSQLite::SQLite(), dbname = "clinton.sqlite")
all_tables <-  dbListTables(connection)
docs <- dbGetQuery(connection, sprintf("SELECT * FROM %s", "docs"))
keeps <- c("date","body")
DF <- docs[keeps]
DF <- mutate_each(DF, funs(tolower))

#doesnt work! fix it.
parsing.date <- function(x) {
  arrive <- ymd_hms(x$date, tz = "Pacific/Auckland")
  time <- hour(arrive)
  #time <-time[complete.cases(time)]
  date <- as.Date(as.POSIXct(strptime(x$date, "%Y-%m-%d %H:%M:%S")))
  #date <-date[complete.cases(date)]
  stripped_date = data.frame(date, time, x$body)
  DF <- stripped_date[order(date),]
}))

parsing.date(DF)

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

keyword <- readline(prompt = "Enter a keyword:   ")
keyword <- tolower(keyword)
user.query <- filter(DF, grepl(keyword, DF$body))


txt = NULL
for (i in 1:nrow(user.query)) {
  txt <- rbind(txt, regmatches(user.query$body[i], regexpr("(nSubject:).*(\\n)?", user.query$body[i]), invert = F))
  txt <- gsub("(\\\\n)", " ", txt)
  txt <- gsub("(\\Re:)", " ", txt)
  txt <- gsub("(nSubject:)", " ", txt)
}

corpus <- Corpus(DataframeSource(txt))

## >>>>>> turn it to a funciton
  corpus.tm <- tm_map(corpus, tolower)
  corpus.tm <- tm_map(corpus.tm, stemDocument)
  corpus.tm <- tm_map(corpus.tm, removeWords, c(stopwords("english"), stopwords("SMART"))) 
  corpus.tm <- tm_map(corpus.tm, removePunctuation)
  corpus.tm <- tm_map(corpus.tm, removeNumbers)
  corpus.tm <- tm_map(corpus.tm, stripWhitespace)

##need more cleaning
inspect(corpus.tm)

#the problems is why coprus works but not corpus TM
coprus.df <-data.frame(text=unlist(sapply(corpus.tm, `[`, "content")), stringsAsFactors=F)

TDM <- TermDocumentMatrix(corpus,
                          control = list(removePunctuation = TRUE,
                          stopwords = TRUE))

inspect(TDM[300:350,1:10])
findFreqTerms(TDM, 100)
findAssocs(TDM, "peace", 0.5)

#network graph
#there are many emails whitout @
#regmatches(docs$body[1], regexpr("(nTo:).*(\\n)?", docs$body[1]), invert = F)

for (i in 1:nrow(docs)){ 
emails <- unlist(regmatches( docs$body[i], gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))",  docs$body[i])))
}

dbDisconnect(connection)

