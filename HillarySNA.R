# functions ----
clean.up <- function(x, DF){
  for (i in 1:nrow(user.query)) {
    query.body <- rbind(query.body, regmatches(user.query$body[i], 
      regexpr("(nsubject:).*(\\n)?", user.query$body[i]), invert = F))
    query.body <- gsub("(\\\\n)", " ", query.body)
    query.body <- gsub("(\\re:)", " ", query.body)
    query.body <- gsub("(nsubject:)", " ", query.body)
    assign('query.body',query.body,envir=.GlobalEnv)
  }}

### 2. Improt data----
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

### 3. graph matrix ----
#senders and recievers
b<- setDT(tstrsplit(as.character(DF$body), "Subject:", fixed=TRUE))[]
b[ ,`:=`(V2 = NULL, V3 = NULL, V4 = NULL)]

c<- setDT(tstrsplit(as.character(b$V1), "nCc:", fixed=TRUE))[]
#drop last 2 coumns and do the same process for others

 
  m <- gsub(".*From:\\.*|\\n.*", "", b$V1[1])
  gsub(".*\\\n","",b$V1[1])
  gsub("^(.*?)\\nSent.*", "\\1", b$V1[1])



library(stringr)
str_extract(string = b$V1[1], pattern = regexp("(?<=From:).*(?=\\n)"))


  a<- setDT(tstrsplit(as.character(DF$body), "\\\n", fixed=TRUE))[]

  a<-gsub("nTo", "\\\", b$V1)


#create attribute data
#Att <- DF[,c("subject","body"):=NULL]

regmatches(b$V1[1], regexpr("(nTo:).*(\\n)?", b$body[1]), invert = F)



