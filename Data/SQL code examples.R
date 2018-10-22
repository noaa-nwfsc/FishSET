library(RSQLite)
library(DBI)
setwd('C:/Users/Melanie/Documents/FistSET_RPackage/Data')
# connect to a database
db <- dbConnect(SQLite(), dbname='testdb.db')
summary(db)
#List tables in the database
dbListTables(db)
#list fielsd of selected table in database
dbListFields(db, "MainDataTable")

#View everything in main data table
a_data <- dbGetQuery(db, "select * from MainDataTable")


#View Haul  in main data table
dbGetQuery(db, "select HAUL from MainDataTable")

#View NMSF_AREA  in main data table filtered by HAUL
dbGetQuery(db, "select NMFS_AREA, HAUL from MainDataTable where HAUL > 100")

#Pull all variables names of a specific type
dbGetQuery(db, "select variable_name from MainDataTableInfo where isEFFORT = 1")

# create a new sqlite database
testdb <- dbConnect(SQLite(), "testdb.db")

#disconnet
 dbDisconnect(db)
 
 #Load data
    #1) You can easily copy an R data frame into a SQLite database with dbWriteTable():
 mydb <- dbConnect(RSQLite::SQLite(), "")
 dbWriteTable(mydb, "mtcars", mtcars)
 dbWriteTable(mydb, "iris", iris)
 dbListTables(mydb)
 
 #Queries
 #Issue a query with dbGetQuery():
 dbGetQuery(mydb, 'SELECT * FROM mtcars LIMIT 5')
 # Not all R variable names are valid SQL variable names, so you may need to escape them with ":
 dbGetQuery(mydb, 'SELECT * FROM iris WHERE "Sepal.Length" < 4.6')
 #to insert the value from a user into a query, use a parameterised query:
 dbGetQuery(mydb, 'SELECT * FROM iris WHERE "Sepal.Length" < :x', 
            params = list(x = 4.6))
 
 
 #Batched queries
 
 db <- dbConnect(SQLite(), dbname='C:/Users/Melanie/Documents/FishSET/fishset-matlab codes/port_AKFIN.mat')
 
