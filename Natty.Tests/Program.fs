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
        sqlQueryf
          "insert into Persons values (NULL, %s, %O, %s)" 
          person.FirstName person.MiddleName person.LastName
          |> executeInsert

      Expect.equal id 1L "Inserted id must equal 1"
    }

    test "Insert Nikola Tesla" {
      let person = nikolaTeslaPerson
      let id = 
        sqlQuery
          "insert into Persons values (NULL, @firstName, @middleName, @lastName)" 
          (Some {| firstName = person.FirstName; middleName = person.MiddleName; lastName = person.LastName |})
           |> executeInsert
 
      Expect.equal id 2L "Inserted id must equal 2"
    }
    
    test "Insert Leslie Nielsen quotes" {
      let insertQuote text personId = 
        sqlQuery 
          "insert into PersonQuotes values (NULL, @text, @personId)"
          (Some {| text = text; personId = personId |}) |> executeInsert
      
      let quote = leslieNielsenQuote

      let id = insertQuote quote.Text quote.PersonId
      Expect.equal id quote.QuoteId "Inserted id must equal 1"
      let quote2 = leslieNielsenQuote2
      let id2 = insertQuote quote2.Text quote2.PersonId
      Expect.equal id2 quote2.QuoteId "Inserted id must equal 2"
    }
  ]

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

      Expect.equal dbPerson person "Local person and db person must equals"
    }
    
    test "Get no records" {
      let person: Person option = sqlQueryf "select * from Persons where Id = 999" |> executeSingleOrDefault

      Expect.equal person None "Default person must equal None"
      match person with
      | Some x -> failtest "Default person must be None"
      | None -> ()
    }
    
    test "Get record with None/null" {
      let person = nikolaTeslaPerson
      let dbPerson: Person = sqlQueryf "select * from Persons where FirstName = %s and LastName = %s" person.FirstName person.LastName |> executeSingle
      Expect.equal { person with Id = dbPerson.Id } { dbPerson with Quotes = [] } "Local Nikola Tesla person and db person must equals"
    }
    
    test "Composable Query" {
      let person = nikolaTeslaPerson
      let condition = sqlQueryf "FirstName = %s and LastName = %s" person.FirstName person.LastName
      let dbPerson: Person = sqlQueryf "select * from Persons where %O" condition |> executeSingle

      Expect.equal { person with Id = dbPerson.Id } { dbPerson with Quotes = [] } "Local Nikola Tesla person and db person must equals"
    }
    
    test "Check null argument in query" {
      let person = { nikolaTeslaPerson with Quotes = null }
      let dbPerson: Person = sqlQueryf "select * from Persons where Id = %i and MiddleName IS %O" person.Id null |> executeSingle

      Expect.equal person dbPerson "Local person and db person must equals"
    }
  ]

[<EntryPoint>]
let main argv =
  init()

  let insertConfig = { Expecto.Tests.defaultConfig with runInParallel = false }

  Tests.runTestsWithArgs insertConfig argv insertTests |> ignore
  Tests.runTestsWithArgs Expecto.Tests.defaultConfig argv tests |> ignore
  0
