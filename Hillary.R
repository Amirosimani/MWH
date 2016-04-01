library(stats)
library(dplyr)
library(DBI)
library(RSQLite)
library(lubridate)
library(regexr)
library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(igraph)

query.body = NULL


#functions
##merge clean.up and clean.corpus
clean.up <- function(x, DF){
  for (i in 1:nrow(user.query)) {
    query.body <- rbind(query.body, regmatches(user.query$body[i], 
      regexpr("(nsubject:).*(\\n)?", user.query$body[i]), invert = F))
    query.body <- gsub("(\\\\n)", " ", query.body)
    query.body <- gsub("(\\re:)", " ", query.body)
    query.body <- gsub("(nsubject:)", " ", query.body)
    assign('query.body',query.body,envir=.GlobalEnv)
  }}

clean.corpus <- function(user.query){
  corpus <- Corpus(DataframeSource(query.body))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"), stopwords("SMART"))) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  assign('corpus',corpus,envir=.GlobalEnv)
}

pars.date <- function(user.query) {
  arrive <- ymd_hms(user.query$date, tz = "Pacific/Auckland")
  time <- hour(arrive)
  date <- as.Date(as.POSIXct(strptime(user.query$date, "%Y-%m-%d %H:%M:%S")))
  stripped_date = data.frame(date, time, user.query$body)
  user.query <- stripped_date[order(date),]
  assign('user.query',user.query,envir=.GlobalEnv)
}

word.frequency <- function (TDM){
  temp <- inspect(TDM)
  word.frequency <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
  row.names(word.frequency) <- NULL
  word.frequency <- word.frequency[order(-word.frequency$Freq),]
  assign('word.frequency',word.frequency,envir=.GlobalEnv)
}

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


#Set up connection to the SQLite database
connection <- dbConnect(RSQLite::SQLite(), dbname = "clinton.sqlite")
all_tables <-  dbListTables(connection)
docs <- dbGetQuery(connection, sprintf("SELECT * FROM %s", "docs"))

#tidy up data frame
keeps <- c("date","body")
DF <- docs[keeps]
DF <- mutate_each(DF, funs(tolower))

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



