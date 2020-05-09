module Natty.Tests.Types

open Mappy

[<CLIMutable>]
type PersonQuoteTag =
  { Id: int64
    PersonQuoteId: int64
    Tag: string
    PersonQuote: PersonQuote }
and [<CLIMutable>] PersonQuote =
  { [<Id>] QuoteId: int64
    Text: string
    PersonId: int64
    Person: Person option
    Tags: PersonQuoteTag seq }
and [<CLIMutable>] Person =
  { Id: int64
    FirstName: string
    MiddleName: string option
    LastName: string 
      // BithdayDate: DateTime
    Quotes: PersonQuote seq }
let leslieNielsenPerson =  
  { Id = 1L;
    FirstName = "Leslie"
    MiddleName = Some "William"
    LastName = "Nielsen"
    Quotes = [] }

let nikolaTeslaPerson =
  { Id = 2L;
    FirstName = "Nikola"
    MiddleName = None
    LastName = "Tesla"
    Quotes = [] }

let leslieNielsenQuote = 
  { QuoteId = 1L;
    Text = "Like a blind man at an orgy, I was going to have to feel my way through."; 
    PersonId = 1L;
    Person = None; 
    Tags = null }

let leslieNielsenQuote2 = 
  { QuoteId = 2L;
    Text = "Jane, since I've met you, I've noticed things that I never knew were there before... birds singing, dew glistening on a newly formed leaf, stoplights."; 
    PersonId = 1L;
    Person = None; 
    Tags = null }

let getLeslieNielsenPersonFull() =
  { leslieNielsenPerson with 
      Quotes = [ leslieNielsenQuote; leslieNielsenQuote2 ] }