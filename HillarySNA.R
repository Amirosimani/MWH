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

### 3. cleaning up and entitiy resoluition----
#senders
people$from  <- gsub("From:", "", people$from)

people <- as.data.frame(sapply(people, function(x) gsub("(<)(.*)(>)|([[])(.*)([]])|([(])(.*)([)])", "", x)))

people <- as.data.frame(sapply(people, function(x) gsub("(Sent)(.*)($)|(Classified)(.*)($)|(Action)(.*)($)|(mailto)(.*)($)|
                                                        (UNCLASSIFIED)(.*)($)|(Reason)(.*)($)|(Date)(.*)($)|
                                                        (Infullappreciation)(.*)($)|(RELEASEINPART)(.*)($)", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub('[[:space:]]', "", x)))

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
people <- as.data.frame(sapply(people, function(x) gsub("[*]", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("[+]", "", x)))

#cleaning up reciepeints 
people$to  <- gsub("(CONFIDENTIAL)(.*)($)", "", people$to)
people$to  <- gsub("(just)(.*)($)", "", people$to)
people$to  <- gsub("(Famous)(.*)($)", "", people$to)

#cleaning up CCs
people$cc  <- gsub("(Subject)(.*)($)", "", people$cc)
people$cc  <- gsub("(\n\n)(.*)(\n\n)", "", people$cc)
people$cc  <- gsub("\n", "", people$cc)
people$cc  <- gsub("\\\\", "", people$cc)

#post-write gsubs
people <- as.data.frame(sapply(people, function(x) gsub("â–ª|â€ž|â€”", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("BESTCOPY", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("B6|B1|B5|B61|B114|14", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub('[0-9]+', "", x)))

#trim leading/tailing whitespae
people <- data.frame(lapply(people, trimws))

#name correction
#Huma Abedin
people <- as.data.frame(sapply(people, function(x) gsub('Hu ma|Hume|Humi|Hunia|Hunna|Htma|HuiTia', "Huma", x)))
people <- as.data.frame(sapply(people, function(x) gsub('Abed in|Abeclin', "Abedin", x)))
people <- as.data.frame(sapply(people, function(x) gsub('AbedinHuma|abedinh@stategov ', "HumaAbedin", x)))
people <- as.data.frame(sapply(people, function(x) gsub('abedinh@stategov|abedinh@stategov', "HumaAbedin", x)))
#Jake Sulivan
people <- as.data.frame(sapply(people, function(x) gsub('JacobJ|Jacobi|Jake', "Jacob", x)))
people <- as.data.frame(sapply(people, function(x) gsub('jakesullivan', "SullivanJacob", x)))
people <- as.data.frame(sapply(people, function(x) gsub('JacobSullivan', "SullivanJacob", x)))
people <- as.data.frame(sapply(people, function(x) gsub('SullivanJacobI|SullivanJacobJ', "SullivanJacob", x)))
people <- as.data.frame(sapply(people, function(x) gsub('sullivanjj@stategov|sullivanij@stategov', "SullivanJacob", x)))
#AnneMarie Slaughter
people <- as.data.frame(sapply(people, function(x) gsub('SlaughterAnneMarie', "AnneMarieSlaughter", x)))
#William Burns
people <- as.data.frame(sapply(people, function(x) gsub('WilliamJ', "William", x)))
#Hillary
people <- as.data.frame(sapply(people, function(x) gsub('HillaryClinton', "H", x)))
people <- as.data.frame(sapply(people, function(x) gsub('hr@mycingularblackberrynet|DR@clintonemailcoms|HDR@clintonemailcom|HDR@clintonemailcorn|HDR@clintonemailcom|HDR@clintonemallcom|HDR@clintonemallcomi|HDR@clintomailcom|HDR@clintomailcomi|HDR@clim', "H", x)))
#cherylmills
people <- as.data.frame(sapply(people, function(x) gsub('cherylmills', "CherylMills", x)))
people <- as.data.frame(sapply(people, function(x) gsub('MillsCherylD|millscd@stategov', "CherylMills", x)))
#OPSNEWs
people <- as.data.frame(sapply(people, function(x) gsub('OpsNewsTicker@stategov', "OpsNewsTicker", x)))
#Ross Alec
people <- as.data.frame(sapply(people, function(x) gsub('RossAlecJ', "RossAlec", x)))
#Valmoro Lona J
people <- as.data.frame(sapply(people, function(x) gsub('ValmoroLonaJ', "ValmoroLona", x)))
#Pverveer
people <- as.data.frame(sapply(people, function(x) gsub('pverveer', "pVerveer", x)))
#Jiloty Lauren C
people <- as.data.frame(sapply(people, function(x) gsub('JilotyLC@stategov', "JilotyLaurenC", x)))
#VerveerMelanneS
people <- as.data.frame(sapply(people, function(x) gsub('verveerms@stategov', "VerveerMelanneS", x)))

people <- as.data.frame(sapply(people, function(x) gsub('PowerSamanthaJ', "PowerSamantha", x)))

#writing csv file
write.csv(people, file = "people.csv")

### 4. adjacency ----
#read csv file
people <- read.csv('/Users/Amiros/GitHub/MWH/people.csv', sep = ",")
people$X <- NULL

#sort senders based on frequecny
people$from <- as.character(people$from)
people <- as.data.frame(sapply(people, function(x) gsub("UNCLASSIFIEDUSDepartmentofStateCaseNoFDocNoC", "", x)))

library(data.table)
from <- as.data.frame(sort(table(people$from), decreasing = TRUE))
from <- setDT(from, keep.rownames = T)[]
write.csv(from, file = "from.csv")

#selecting top 100 senders
from <- read.csv('/Users/Amiros/GitHub/MWH/from.csv', sep = ",")
from$X <- NULL

top100 <- from[1:100,]
colnames(top100) <- c("person", "freq")
#top100 <- top100[person != '']
selected <- people[people$from %in% top100$rn,]

#split to & cc columns and remove NAs and non-address rows
recipients_sep <- setDT(tstrsplit(as.character(selected$to), ";", fixed=TRUE))[]
recipients_sep <- recipients_sep[, list(V1)] ##keep the first recipeint
#b <- setDT(tstrsplit(as.character(selected$cc), ";", fixed=TRUE))[] ##dont care about CCs at the moment
mat_sel <- cbind(selected$from, recipients_sep)
colnames(mat_sel)[1] <- "from"
mat_sel <- mat_sel[ V1 != 'NA' & nchar(V1) < 29 & V1 != ''] 

library(plyr)
cdata1 <- ddply(mat_sel, c("from", "V1"), summarise,   N = length(from))

#Adjacency matrix
library(igraph)
mat <- cdata1
mat$N <- NULL

a <- get.adjacency(graph.edgelist(as.matrix(mat), directed=T))

