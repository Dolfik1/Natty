# Natty
Fast and Functional ORM for F#


## Initialize connection (Sqlite for example)
```F#
let private dbPath = Path.Combine(AppContext.BaseDirectory, "db.sqlite")

let private getConn = lazy(
    if File.Exists(dbPath) then File.Delete(dbPath)
    new SqliteConnection(sprintf "Data Source=%s" dbPath))

let execute sqlQuery = execute (getConn.Force()) defaultConfig sqlQuery
```

## Executing queries

