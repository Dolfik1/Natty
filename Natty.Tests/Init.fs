module Natty.Tests.Init

open Natty.Data

let execute = Natty.Tests.Data.execute

let init() =
  execute (sqlQuery<unit, unit> """
    create table `Persons` (
      `Id` INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      `FirstName` TEXT NOT NULL,
      `MiddleName` TEXT,
      `LastName` TEXT NOT NULL);""" None) |> ignore

  execute (sqlQuery<unit, unit> """
    create table `PersonQuotes` (
      `Id` INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      `Text` TEXT NOT NULL,
      `PersonId` INTEGER NOT NULL);""" None) |> ignore

    
  execute (sqlQuery<unit, unit> """
    create table `PersonQuoteTags` (
      `Id` INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      `PersonQuoteId` INTEGER NOT NULL,
      `Tag` TEXT NOT NULL);""" None) |> ignore