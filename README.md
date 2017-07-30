# Natty
Fast and Functional ORM for F#


## Initialize connection (Sqlite for example)
```F#
let private dbPath = Path.Combine(AppContext.BaseDirectory, "db.sqlite")

let private getConn = lazy(
    if File.Exists(dbPath) then File.Delete(dbPath)
    new SqliteConnection(sprintf "Data Source=%s" dbPath))

let execute sqlQuery = execute (getConn.Force()) defaultConfig sqlQuery
let executeSingle (sqlQuery: SqlQuery<'a>) = executeSingle (getConn.Force()) defaultConfig sqlQuery
```

## Executing queries

### Select single record
Define the type:
```F#
[<CLIMutable>]
type Person =
    { Id: int64
      FirstName: string
      MiddleName: string option
      LastName: string
      Quotes: PersonQuote seq }

```
... and just execute query
```F#
let person: Person = sqlQueryf "select * from Persons where Id = %i" 1 |> executeSingle
```

### Insert single record
For inserting records, your can declare the function for inserting records:
```F#
let executeInsert sqlQuery = 
    let q = { sqlQuery with QueryText = (sprintf "%s;\nselect last_insert_rowid()" sqlQuery.QueryText) }
    executeSingle q
```
... and insert the record:
```F#
let id = 
    sqlQueryf
        "insert into Persons values (NULL, %s, %O, %s)" 
        person.FirstName person.MiddleName person.LastName
        |> executeInsert
```

You also can use executeSingle function in case, when not need return inserted id:
```F#
sqlQueryf
    "insert into Persons values (NULL, %s, %O, %s)" 
    person.FirstName person.MiddleName person.LastName
    |> executeSingle<unit>
``` 

### Using named parameters

If you need to use named parameters for any reason, you can do this like this:

```F#
sqlQuery
    "insert into Persons values (NULL, @firstName, @middleName, @lastName)" 
    (Some [ "firstName", box person.FirstName; "middleName", box person.MiddleName; "lastName", box person.LastName ])
    |> executeSingle<unit>
```
But this do not necessary, because formatted queries (`sqlQueryf`) automatically do it.