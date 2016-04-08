### 1. Improt data----
# 2.1 Set up connection to the SQLite database
library(DBI)
library(RSQLite)
connection <- dbConnect(RSQLite::SQLite(), dbname = "clinton.sqlite")
all_tables <-  dbListTables(connection)
docs <- dbGetQuery(connection, sprintf("SELECT * FROM %s", "docs"))
dbDisconnect(connection)

#2.2 drop unneccasry columns
library(data.table)
docs = data.table(docs)
DF = docs[,c("posted_date","pdf_link","author","doc_class","message_num","case_num",
              "declass_date", "id","full_path","is_document","doc_date","pdf_url" ):=NULL]

#2.3 data wrangling
#classification
"secret=2, confidential =1, NA =0"
DF[is.na(classification),classification:=0]
DF$classification[DF$classification == 'CONFIDENTIAL'] = 1
DF$classification[DF$classification == 'SECRET'] = 2

#reason
DF[is.na(classification),classification:="0"]
DF$reason <- gsub(" ", "", DF$reason)

#subject
"check if the email was a reply"
setDT(DF)[subject %like% "RE:", message_type:=1] #reply: 1
setDT(DF)[subject %like% "FW:", message_type:=2] #forward: 2
DF[is.na(message_type),message_type:=0] #normal

#time & date
DF[, c("Date", "Time") := tstrsplit(date, " ", fixed=TRUE)]
DF[,c("date"):=NULL]
DF[, Date:=as.IDate(Date)]
DF[, Time:=as.ITime(Time)]

### 2. contact list ----
#senders and recievers
b<- setDT(tstrsplit(as.character(DF$body), "Subject:", fixed=TRUE))[]
b <- b[ ,`:=`(V2 = NULL, V3 = NULL, V4 = NULL)] #remove the body of messages

#nCC
c <- setDT(tstrsplit(as.character(b$V1), "Cc:", fixed=TRUE))[]
c2 <- c[ ,`:=`(V3 = NULL, V4 = NULL)]
c <- setDT(tstrsplit(as.character(c2$V2), "nSent:", fixed=TRUE))[]
c <- c[ ,`:=`(V2 = NULL)]
names(c)[names(c) == "V1"] = "cc" #extract CCs to a column

#nTo
d <- setDT(tstrsplit(as.character(c2$V1), "To:", fixed=TRUE))[]
d2 <- d[ ,`:=`(V3 = NULL, V4 = NULL, V5 = NULL, V6 = NULL)]
d <- setDT(tstrsplit(as.character(d2$V2), "Sent:", fixed=TRUE))[]
d <- d[ ,`:=`(V2 = NULL, V3=NULL)]
names(d)[names(d) == "V1"] = "to" #extract CCs to a column

#nFrom
e <- setDT(tstrsplit(as.character(d2$V1), "Sent:", fixed=TRUE))[]
e <- e[ ,`:=`(V2 = NULL)]
names(e)[names(e) == "V1"] = "from" #extract Senders to a column

#binding appropriate columns together 
people <- cbind(e, d, c)

### 3. cleaning up----
#senders
people$from  <- gsub("From:", "", people$from)

people <- as.data.frame(sapply(people, function(x) gsub("(<)(.*)(>)", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("([[])(.*)([]])", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("([(])(.*)([)])", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("(Sent)(.*)($)", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("(Classified)(.*)($)", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("(UNCLASSIFIED)(.*)($)", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("(Reason)(.*)($)", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("(Date)(.*)($)", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("\\\\n", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("(<)(.*)($)", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("([[])(.*)($)", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("([(])(.*)($)", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub(")", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("[(]", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("<", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub(">", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("•", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("»", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("«", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub('"', '', x)))
people <- as.data.frame(sapply(people, function(x) gsub(':', '', x)))
people <- as.data.frame(sapply(people, function(x) gsub('-', '', x)))
people <- as.data.frame(sapply(people, function(x) gsub(",", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("'", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("[.]", "", x)))

#cleaning up reciepeints 
people$to  <- gsub("(CONFIDENTIAL)(.*)($)", "", people$to)
people$to  <- gsub("(just)(.*)($)", "", people$to)
people$to  <- gsub("(Famous)(.*)($)", "", people$to)

#cleaning up CCs
people$cc  <- gsub("(Subject)(.*)($)", "", people$cc)
people$cc  <- gsub("(\n\n)(.*)(\n\n)", "", people$cc)
people$cc  <- gsub("\n", "", people$cc)
people$cc  <- gsub("\\\\", "", people$cc)

#trim leading/tailing whitespae
people <- data.frame(lapply(people, trimws))

#writing csv file
write.csv(people, file = "people.csv", row.names=NULL)

### 4. entity resolution ----
#read csv file
people <- read.csv('/Users/Amiros/GitHub/MWH/people.csv', sep = ",")
people$X <- NULL
people$from <- as.character(people$from)
people$to <- as.character(people$to)
people$cc <- as.character(people$cc)

# It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
dist.name<-adist(people$from,people$to, partial = TRUE, ignore.case = TRUE)

# We now take the pairs with the minimum distance
min.name<-apply(dist.name, 1, min)

match.s1.s2<-NULL  
for(i in 1:nrow(dist.name))
{
  s2.i<-match(min.name[i],dist.name[i,])
  s1.i<-i
  match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=source2.devices[s2.i,]$name, s1name=source1.devices[s1.i,]$name, adist=min.name[i]),match.s1.s2)
}
# and we then can have a look at the results
View(match.s1.s2)