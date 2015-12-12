library(stats)
library(dplyr)
library(DBI)
library(RSQLite)
library(lubridate)
library(NLP)
library(tm)
library(SnowballC)

query.body = NULL

#functions
clean.up <- function(x, DF){
  for (i in 1:nrow(user.query)) {
    query.body <- rbind(query.body, regmatches(user.query$body[i], regexpr("(nsubject:).*(\\n)?", user.query$body[i]), invert = F))
    query.body <- gsub("(\\\\n)", " ", query.body)
    query.body <- gsub("(\\Re:)", " ", query.body)
    query.body <- gsub("(nsubject:)", " ", query.body)
    assign('query.body',query.body,envir=.GlobalEnv)
  }}

clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"), stopwords("SMART"))) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  assign('corpus',corpus,envir=.GlobalEnv)
}

pars.date <- function(DF) {
  arrive <- ymd_hms(DF$date, tz = "Pacific/Auckland")
  time <- hour(arrive)
  #time <-time[complete.cases(time)]
  date <- as.Date(as.POSIXct(strptime(DF$date, "%Y-%m-%d %H:%M:%S")))
  #date <-date[complete.cases(date)]
  stripped_date = data.frame(date, time, DF$body)
  DF <- stripped_date[order(date),]
  assign('DF',DF,envir=.GlobalEnv)
}

#Set up connection to the SQLite database
connection <- dbConnect(RSQLite::SQLite(), dbname = "clinton.sqlite")
all_tables <-  dbListTables(connection)
docs <- dbGetQuery(connection, sprintf("SELECT * FROM %s", "docs"))
keeps <- c("date","body")

#tidy up data frame
DF <- docs[keeps]
DF <- mutate_each(DF, funs(tolower))
pars.date(DF)
colnames(DF)[3] <- "body"

#interacting with the user to get input
keyword <- readline(prompt = "Enter a keyword:   ")
keyword <- tolower(keyword)
user.query <- filter(DF, grepl(keyword, DF$body))

#clean up the corpus ofr analysis
clean.up(user.query)
corpus <- Corpus(DataframeSource(query.body))
clean.corpus(corpus)

inspect(corpus)

coprus.df <-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)


TDM <- TermDocumentMatrix(corpus,
                          control = list(removePunctuation = TRUE,
                          stopwords = TRUE))

inspect(TDM[1:350,1:10])
findFreqTerms(TDM, 100)
findAssocs(TDM, "peace", 0.5)

#network graph
#there are many emails whitout @
#regmatches(docs$body[1], regexpr("(nTo:).*(\\n)?", docs$body[1]), invert = F)

for (i in 1:nrow(docs)){ 
emails <- unlist(regmatches( docs$body[i], gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))",  docs$body[i])))
}

dbDisconnect(connection)


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
