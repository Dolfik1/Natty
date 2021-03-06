module Natty.Tests.Data

open System
open System.IO
open Natty.Data

open Microsoft.Data.Sqlite

let private dbPath = Path.Combine(AppContext.BaseDirectory, "db.sqlite")

let private getConn = lazy(
  if File.Exists(dbPath) then File.Delete(dbPath)
  new SqliteConnection(sprintf "Data Source=%s" dbPath))

let inline map reader =
  Natty.Mappy.createMapper (Mappy.MappyOptions()) reader

let execute sqlQuery = execute (getConn.Force()) map sqlQuery
let executeSingle (sqlQuery: SqlQuery<'a>) = executeSingle (getConn.Force()) map sqlQuery
let executeSingleOrDefault sqlQuery = executeSingleOrDefault getConn.Value map sqlQuery
let executeInsert sqlQuery = 
  let q = { sqlQuery with QueryText = (sprintf "%s;\nselect last_insert_rowid()" sqlQuery.QueryText) }
  executeSingle q