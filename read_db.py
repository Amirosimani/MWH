import pandas as pd
import sqlite3

#Set up connection to the SQLite database
connection = sqlite3.connect('clinton.sqlite')

#Print all tables
print("Tables")
all_tables = pd.read_sql("SELECT name FROM sqlite_master WHERE type = 'table'", connection)
print(all_tables)

#Print information about 'docs' table
docs = pd.read_sql("SELECT * FROM %s" % ("docs"), connection)
print("Column Names")
print(list(docs.columns.values))
print("Number of Rows: %d" % len(docs))

#Clean up connection to the database
connection.close()