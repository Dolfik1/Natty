open System
open Expecto

open Natty.Tests.Init
open Natty.Tests.Types

open Natty.Data
open Natty.Tests.Data

let insertTests = 
  testList "Insert tests" [
    test "Insert Leslie Nielsen" {
      let person = leslieNielsenPerson
      let id = 
        sqlQuery<int64> 
          "insert into Persons values (NULL, @FirstName, @MiddleName, @LastName)" 
          (Some [ "FirstName", box person.FirstName; "LastName", box person.LastName; "MiddleName", box person.MiddleName ]) 
          |> executeInsert

      Expect.equal id 1L "Inserted id must equal 1"
    }
    test "Insert Leslie Nielsen quotes" {
      let insertQuote text personId = sqlQueryf "insert into PersonQuotes values (NULL, %s, %i)" text personId |> executeInsert
      
      let quote = leslieNielsenQuote

      let id = insertQuote quote.Text quote.PersonId
      Expect.equal id quote.QuoteId "Inserted id must equal 1"

      let quote2 = leslieNielsenQuote2
      let id2 = insertQuote quote2.Text quote2.PersonId
      Expect.equal id2 quote2.QuoteId "Inserted id must equal 2"
        
    }]

let tests =
  testList "" [
    test "Get Leslie Nielsen person by Id" {
      let person = { leslieNielsenPerson with Quotes = null }
      let dbPerson: Person = sqlQueryf "select * from Persons where Id = %i" person.Id |> executeSingle

      Expect.equal person dbPerson "Local person and db person must equals"
    }
    test "Get Leslie Nielsen person by Id with Quotes" {

      let person = { leslieNielsenPerson with Quotes = [ leslieNielsenQuote; leslieNielsenQuote2 ] }
      let dbPerson = 
        sqlQueryf
          """select 
              Persons.*, 
              PersonQuotes.Id Quotes_QuoteId,
              PersonQuotes.Text Quotes_Text,
              PersonQuotes.PersonId Quotes_PersonId
             from Persons
             join PersonQuotes on PersonQuotes.PersonId = Persons.Id
             where Persons.Id = %i""" person.Id 
             |> executeSingle

      let dbPerson = { dbPerson with Quotes = dbPerson.Quotes |> Seq.toList }

      
      Expect.equal person dbPerson "Local person and db person must equals"
    }
  ]

[<EntryPoint>]
let main argv =
    init()

    let insertConfig = { Expecto.Tests.defaultConfig with ``parallel`` = false }

    Tests.runTestsWithArgs insertConfig argv insertTests |> ignore
    Tests.runTestsWithArgs Expecto.Tests.defaultConfig argv tests |> ignore
    0 // return an integer exit code
