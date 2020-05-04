#install.packages("RMariaDB")

library(RMariaDB) 

bigdatadb <- dbConnect(RMariaDB::MariaDB(), user='bdauser', password='BDA4ever!!!', dbname='bigdatadb', host='35.193.193.138')
dbListTables(bigdatadb)

query = "SELECT * FROM openaq_state WHERE openaq_state.country= 'AU';"
result = dbGetQuery(bigdatadb,query)

