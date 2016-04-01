library(shiny)
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


connection <- dbConnect(RSQLite::SQLite(), dbname = "./clinton.sqlite")
all_tables <-  dbListTables(connection)
docs <- dbGetQuery(connection, sprintf("SELECT * FROM %s", "docs"))

#tidy up data frame
keeps <- c("date","body")
DF <- docs[keeps]
DF <- mutate_each(DF, funs(tolower))
