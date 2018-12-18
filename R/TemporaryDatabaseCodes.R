mydb <- dbConnect(RSQLite::SQLite(), "fishet-db.sqlite")
dbGetQuery(conn = mydb, "CREATE TABLE IF NOT EXISTS data (AlternativeMatrix ALT)");
dbGetQuery(mydb, 'INSERT INTO data VALUES (:AlternativeMatrix)', params = list(AlternativeMatrix = list(serialize(Alt, NULL))));
some_object <- unserialize(RSQLite::dbGetQuery(mydb, 'SELECT AlternativeMatrix FROM data LIMIT 1')$AlternativeMatrix[[1]]);


dbRemoveTable(mydb, "data")
