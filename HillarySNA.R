### 1. Improt data----
#Set up connection to the SQLite database
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
simmilarity_index <- function(x, y){
  if (max(nchar(as.character(x)), nchar(as.character(y))) > 0){
  sim_index <- 1- stringdist(x, y, method = "lv")/max(nchar(as.character(x)), nchar(as.character(y)))
  return(sim_index)

  }
}
#senders
people$from  <- gsub("From:", "", people$from)

people <- as.data.frame(sapply(people, function(x) gsub("(<)(.*)(>)|([[])(.*)([]])|([(])(.*)([)])", "", x)))

people <- as.data.frame(sapply(people, function(x) gsub("(Sent)(.*)($)|(Classified)(.*)($)|(Action)(.*)($)|(mailto)(.*)($)|
                                                        (UNCLASSIFIED)(.*)($)|(Reason)(.*)($)|(Date)(.*)($)|
                                                        (Infullappreciation)(.*)($)|(RELEASEINPART)(.*)($)", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub('[[:space:]]', "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("UNCLASSIFIEDUSDepartmentofStateCaseNoFDocNoC", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("unclassifiedusdepartmentofstatecasenofdocnoc", "", x)))

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
people <- as.data.frame(sapply(people, function(x) gsub('-|_', '', x)))
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
people <- as.data.frame(sapply(people, function(x) gsub("latin1", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("ASCII", "", x)))

people <- as.data.frame(sapply(people, function(x) gsub("BESTCOPY", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub("B6|B1|B5|B61|B114|14", "", x)))
people <- as.data.frame(sapply(people, function(x) gsub('[0-9]+', "", x)))

library(dplyr)
people <- mutate_each(people, funs(tolower))

people <- as.data.frame(sapply(people, function(x) gsub('stategov|stategoy', "", x)))


#trim leading/tailing whitespae
people <- data.frame(lapply(people, trimws))

#name correction
#Huma Abedin
people <- as.data.frame(sapply(people, function(x) gsub('hu ma|hume|humi|hunia|hunna|htma|huiTia', "huma", x)))
people <- as.data.frame(sapply(people, function(x) gsub('abed in|abeclin', "abedin", x)))
people <- as.data.frame(sapply(people, function(x) gsub('abedinhuma|abedinh@', "humaabedin", x)))
people <- as.data.frame(sapply(people, function(x) gsub('abedinh@s|abedinh@s', "humaabedin", x)))

#Jake Sulivan
people <- as.data.frame(sapply(people, function(x) gsub('jacobj|jacobi|jake', "jacob", x)))
people <- as.data.frame(sapply(people, function(x) gsub('jakesullivan', "jacobsullivan", x)))
people <- as.data.frame(sapply(people, function(x) gsub('sullivanjacob', "jacobsullivan", x)))
people <- as.data.frame(sapply(people, function(x) gsub('sullivanjacobI|sullivanjacobj|jakesullivar|lakesullivar', "jacobsullivan", x)))
people <- as.data.frame(sapply(people, function(x) gsub('sullivanjj@stategov|sullivanij@stategov', "jacobsullivan", x)))
people <- as.data.frame(sapply(people, function(x) gsub('sullivanjj@|sullivanij@', "jacobsullivan", x)))

#AnneMarie Slaughter
people <- as.data.frame(sapply(people, function(x) gsub('slaughterannemarie', "annemarieslaughter", x)))
#William Burns
people <- as.data.frame(sapply(people, function(x) gsub('williamJ', "william", x)))
#Hillary
people <- as.data.frame(sapply(people, function(x) gsub('hillaryclinton|clintonhillary', "H", x)))
people <- as.data.frame(sapply(people, function(x) gsub('hr@mycingularblackberrynet|dr@clintonemailcoms|hdr@clintonemailcom|hdr@clintonemailcorn|hdr@clintonemailcom|hdr@clintonemallcom|hdr@clintonemallcomi|hdr@clintomailcom|hdr@clintomailcomi|hdr@clim', "H", x)))
#cherylmills
people <- as.data.frame(sapply(people, function(x) gsub('millscheryld|millscd@', "cherylmills", x)))
#OPSNEWs
people <- as.data.frame(sapply(people, function(x) gsub('OpsNewsTicker@stategov', "OpsNewsTicker", x)))
people <- as.data.frame(sapply(people, function(x) gsub('opsnewsticker@', "opsnewsticker", x)))

#Ross Alec
people <- as.data.frame(sapply(people, function(x) gsub('rossalecj', "rossalec", x)))
#Valmoro Lona J
people <- as.data.frame(sapply(people, function(x) gsub('valmorolonaj', "valmorolona", x)))
#Jiloty Lauren C
people <- as.data.frame(sapply(people, function(x) gsub('jilotylc@', "jilotylaurenc", x)))
#VerveerMelanneS
people <- as.data.frame(sapply(people, function(x) gsub('verveerms@', "verveermelannes", x)))

people <- as.data.frame(sapply(people, function(x) gsub('powersamanthaj', "powersamantha", x)))

people <- as.data.frame(sapply(people, function(x) gsub('campbelikm@', "campbellkurtm", x)))

#writing csv file
people <- cbind(people, docs$reason)
write.csv(people, file = "people.csv")
rm(list = ls())

### 4. pre-adjacency ----
#read csv file
people <- read.csv('/Users/Amiros/GitHub/MWH/people.csv', sep = ",")
people$X <- NULL

#sort senders based on frequecny
people$from <- as.character(people$from)

library(data.table)
from <- as.data.frame(sort(table(people$from), decreasing = TRUE))
from <- setDT(from, keep.rownames = T)[]
from <- from[rn != '']


#selecting top 100 senders
top100 <- from[1:100,]
colnames(top100) <- c("person", "freq")
top100 <- top100[person != '']
selected <- people[people$from %in% top100$person,]


#split to & cc columns and remove NAs and non-address rows
recipients_sep <- setDT(tstrsplit(as.character(selected$to), ";", fixed=TRUE))[]
recipients_sep <- recipients_sep[, list(V1)] ##keep the first recipeint
#b <- setDT(tstrsplit(as.character(selected$cc), ";", fixed=TRUE))[] ##dont care about CCs at the moment
mat_sel <- cbind(selected$from, recipients_sep, selected$docs.reason)
colnames(mat_sel)[1] <- "from"
mat_sel <- mat_sel[ V1 != 'NA' & nchar(V1) < 29 & V1 != ''] 

library(plyr)
cdata1 <- ddply(mat_sel, c("from", "V1", "V3"), summarise,   N = length(from))#with freq
write.csv(cdata1, file = "edge_list_aggregated.csv")
write.csv(mat_sel, file = "edge_list.csv")

### 4.5 Adjacency matrix ----
rm(list = ls())
edge_list <- read.csv('/Users/Amiros/GitHub/MWH/edge_list.csv', sep = ",")
edge_list$X <- NULL
classif <- edge_list$V3
edge_list$V3 <- NULL

library(igraph)
adj_mat <- get.adjacency(graph.edgelist(as.matrix(edge_list), directed=T))
adj_mm <- as.matrix(adj_mat)
write.csv(adj_mm, file = "adj_mat1.csv")

### 5. Attributes ----