### 0. Libraries----
#library(stats)
#library(dplyr)
#library(regexr)
#library(NLP)
#library(tm)
#library(SnowballC)
#library(RColorBrewer)
#library(wordcloud)
#library(igraph)

### 1. Functions----
query.body = NULL

# 1.1
clean.up <- function(x, DF){
  for (i in 1:nrow(user.query)) {
    query.body <- rbind(query.body, regmatches(user.query$body[i], 
      regexpr("(nsubject:).*(\\n)?", user.query$body[i]), invert = F))
    query.body <- gsub("(\\\\n)", " ", query.body)
    query.body <- gsub("(\\re:)", " ", query.body)
    query.body <- gsub("(nsubject:)", " ", query.body)
    assign('query.body',query.body,envir=.GlobalEnv)
  }}

# 1.2
clean.corpus <- function(user.query){
  corpus <- Corpus(DataframeSource(query.body))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"), stopwords("SMART"))) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  assign('corpus',corpus,envir=.GlobalEnv)
}

# 1.4
word.frequency <- function (TDM){
  temp <- inspect(TDM)
  word.frequency <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
  row.names(word.frequency) <- NULL
  word.frequency <- word.frequency[order(-word.frequency$Freq),]
  assign('word.frequency',word.frequency,envir=.GlobalEnv)
}

#1.5
chronology <- function(user.query){
  year2009 <- subset(user.query, format(date,'%Y') %in% c('2009'))
  year2010 <- subset(user.query, format(date,'%Y') %in% c('2010'))
  year2011 <- subset(user.query, format(date,'%Y') %in% c('2011'))
  year2012 <- subset(user.query, format(date,'%Y') %in% c('2012'))
  y <- as.numeric(c(nrow(year2009),nrow(year2010),nrow(year2011),nrow(year2012)))
  yy <- c(0,max(y))
  par(mfrow=c(3,2))
  barplot(y, main=" Emails per year", names.arg = c("2009","2010","2011","2012"))
  plot(1, type="n", axes=F, xlab="", ylab="")
  hist(year2009$time, breaks = 24, freq = T, main = paste("Daily activity in 2009"), xlab = "2009", xlim = c(0,25), ylim = c(0,10))
  hist(year2010$time, breaks = 24, freq = T, main = paste("Daily activity in 2010"), xlab = "2009", xlim = c(0,25), ylim = c(0,10))
  hist(year2011$time, breaks = 24, freq = T, main = paste("Daily activity in 2011"), xlab = "2009", xlim = c(0,25), ylim = c(0,10))
  hist(year2012$time, breaks = 24, freq = T, main = paste("Daily activity in 2012"), xlab = "2009", xlim = c(0,25), ylim = c(0,10))
}

### 2. Improt data----
# 2.1 Set up connection to the SQLite database
library(DBI)
library(RSQLite)
connection <- dbConnect(RSQLite::SQLite(), dbname = "clinton.sqlite")
all_tables <-  dbListTables(connection)
docs <- dbGetQuery(connection, sprintf("SELECT * FROM %s", "docs"))

#2.2 drop unneccasry columns
library(data.table)
docs = data.table(docs)
DF = docs[,c("posted_date","pdf_link","author","doc_class","message_num","case_num",
              "declass_date", "id","full_path","is_document","doc_date","pdf_url" ):=NULL]

#2.3 data wrangling
#classification
"secret=2, confidential =1, NA =0"
DF[is.na(classification),classification:=0]
DF$classification[is.na(DF$classification)] = 0
DF$classification[DF$classification == NA] = 0
DF$classification[DF$classification == 'CONFIDENTIAL'] = 1
DF$classification[DF$classification == 'SECRET'] = 2

#reason
DF$reason <- gsub(" ", "", DF$reason)

#subject
"check if the email was a reply"

  
  
  
//
#time & date
DF[, c("Date", "Time") := tstrsplit(date, " ", fixed=TRUE)]
DF[,c("date"):=NULL]
DF[, Date:=as.IDate(Date)]
DF[, Time:=as.ITime(Time)]



//
library(lubridate)
arrive <- ymd_hms(DF$date, tz = "Pacific/Auckland")
time <- hour(arrive)
date <- as.Date(as.POSIXct(strptime(DF$date, "%Y-%m-%d %H:%M:%S")))
stripped_date = data.frame(date, time, DF$body)
  user.query <- stripped_date[order(date),]
  assign('user.query',user.query,envir=.GlobalEnv)
}


//
DF <- mutate_each(DF, funs(tolower))



"need to seperate senders and recievers in Trifecta to creat the network"
write.csv(DF,file = "data.csv")







### other ----

#interacting with the user to get input
keyword <- readline(prompt = "Enter a keyword:   ")
keyword <- tolower(keyword)
user.query <- filter(DF, grepl(keyword, DF$body))
pars.date(user.query)
colnames(user.query)[3] <- "body"
chronology(user.query)

#clean up the corpus ofr analysis - make it one function with clean.corpus
clean.up(user.query)
corpus <- Corpus(DataframeSource(query.body))
clean.corpus(query.body)

corpus.df <-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)

TDM <- TermDocumentMatrix(corpus,control = list(removePunctuation = TRUE,stopwords = TRUE))
inspect(TDM)
word.frequency(TDM)
##unstem
wordcloud(word.frequency$ST, word.frequency$Freq,max.words=100)

"
associate <- readline(prompt = "Enter an asscoiated word: ")
assocplot <- tolower(keyword)
similarity <- as.numeric(readline(prompt ="Enter a expected sparsity (between 0 and 1):  "))
ass.word <- findAssocs(TDM, associate, similarity)
"

head(user.query, n=2)
#network graph
#there are many emails whitout @
#regmatches(docs$body[1], regexpr("(nTo:).*(\\n)?", docs$body[1]), invert = F)


sender = NULL
recipient = NULL

for (i in 1:nrow(user.query)) {
  if (length(regmatches(user.query$body[i], regexpr("from:.+nto:", user.query$body[i]))) > 0){
    user.query$sender[i] <- rbind(sender, regmatches(user.query$body[i], regexpr("from:.+nsent:", user.query$body[i]), invert = F))
    user.query$sender[i] <- gsub("(from: )", "", user.query$sender[i])
    user.query$sender[i] <- gsub("(nsent:)", "", user.query$sender[i])
  } else {
    user.query$sender[i] <- "NA"
  }
  if (length(regmatches(user.query$body[i], regexpr("to:.+subject:", user.query$body[i])))>0){
    user.query$recipient[i] <- rbind(recipient, regmatches(user.query$body[i], regexpr("to:.+subject:", user.query$body[i]), invert = F))
    user.query$recipient[i] <- gsub("(to:)", "", user.query$recipient[i])
    user.query$recipient[i] <- gsub("(\\\ncc:)", "", user.query$recipient[i])
    user.query$recipient[i] <- gsub("(\\\nsubject:)", "", user.query$recipient[i])
  } else {
    user.query$recipient[i] <- "NA"
  }
}

user.query$sender <- gsub( ".+nto:", "", user.query$sender, invert = T)
user.query$sender <- gsub( ".+ncc:", "", user.query$sender, invert = T)
user.query$recipient <- gsub( ".+nsent:", "", user.query$recipient, invert = T)


dbDisconnect(connection)



