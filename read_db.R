library(dplyr)
library(RSQLite)

#Set up connection to the SQLite database
connection <- dbConnect(RSQLite::SQLite(), dbname = "clinton.sqlite")

#Print all tables
print("Tables")
all_tables <-  dbListTables(connection)
print(all_tables)

#Print information about 'docs' table
docs <- dbGetQuery(connection, sprintf("SELECT * FROM %s", "docs"))
print("Column Name")
print(colnames(docs))
print(sprintf("Number of Rows: %d", nrow(docs)))

#Clean up connection to the database
dbDisconnect(connection)