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
b2 <- b[ ,`:=`(V2 = NULL, V3 = NULL, V4 = NULL)] #remove the body of messages

#nCC
c <- setDT(tstrsplit(as.character(b2$V1), "nCc:", fixed=TRUE))[]
c2 <- c[ ,`:=`(V3 = NULL, V4 = NULL)]
c3 <- setDT(tstrsplit(as.character(c2$V2), "nSent:", fixed=TRUE))[]
cc <- c3[ ,`:=`(V2 = NULL)]
names(cc)[names(cc) == "V1"] = "cc" #extract CCs to a column

#nTo
d <- setDT(tstrsplit(as.character(c2$V1), "nTo:", fixed=TRUE))[]
d2 <- d[ ,`:=`(V3 = NULL, V4 = NULL, V5 = NULL, V6 = NULL)]
d3 <- setDT(tstrsplit(as.character(d2$V2), "nSent:", fixed=TRUE))[]
reciepients <- d3[ ,`:=`(V2 = NULL)]
names(reciepients)[names(reciepients) == "V1"] = "to" #extract CCs to a column

#nFrom
e <- setDT(tstrsplit(as.character(d2$V1), "nSent:", fixed=TRUE))[]
from <- e[ ,`:=`(V2 = NULL)]
names(from)[names(from) == "V1"] = "from" #extract Senders to a column

#binding appropriate columns together 
people <- cbind(from, reciepients, cc)

#cleaning up
people$from <- as.data.frame(sapply(people$from,gsub,pattern="From:",replacement=""))
#people$from <- as.data.frame(sapply(people$from,gsub,pattern="^.*< *(.*?) +>.*$",replacement=""))

gsub("^.*< *(.*?) +>.*$", "\\1", people$from[1])
people <- as.data.frame(sapply(people,gsub,pattern="\\\\n",replacement=""))

#trim leading/tailing whitespae
people <- data.frame(lapply(people, trimws))



#create attribute data
#Att <- DF[,c("subject","body"):=NULL]

regmatches(b$V1[1], regexpr("(nTo:).*(\\n)?", b$body[1]), invert = F)



