# Cacheable

About
-----
An F# library to help compose caching functions that have triggers for resets.


F# Example
-----

```fsharp
[<Struct>]
type UserId = UserId of int

[<Struct>]
type Name = Name of string

[<Struct>]
type Address = Address of string

[<Struct>]
type CompanyId = CompanyId of int

type User =
    {
        Id : UserId
        Name : Name
        Company : CompanyId
    }

type Company =
    {
        Id : CompanyId
        Name : Name
        Address : Address
    }

let getUser (id : UserId) : User =
    // This will call out to a database.
    { Id = id; Name = Name "Jack"; Company = CompanyId 2 }


let getCompany (c : CompanyId) : Company =
    // This will call out to a different database.
    { Id = c; Name = Name "TestCompany"; Address = Address "TestLane" }

// Events that are used to signify databases have been changed.
let userDbResetEvent = Event<unit> ()
let companyDbResetEvent = Event<unit> ()

let constructCompanyList
    (getUser : UserId -> User)
    (getCompany : CompanyId -> Company)
    (userIds : UserId list)
    : Set<Company>
    =
    // This function does a lot of work on top of hitting the databases.
    // We also have to set up the cache invalidations with respect to
    // the two databases
    Set.empty

let getUserCacheable =
    Cacheable.func getUser userDbResetEvent.Publish CachingStrategy.All
let getCompanyCacheable =
    Cacheable.func getCompany companyDbResetEvent.Publish CachingStrategy.All

// This constructs a (UserId list -> Set<Company>) function that will cache the results of
// this function and reset its cache based on the reset events of either of its parameters.
let (constructCompanyListCacheable, _) =
    Cacheable.lift constructCompanyList CachingStrategy.All
    |> fun f -> Cacheable.applyArg f getUserCacheable
    |> fun f -> Cacheable.applyArg f getCompanyCacheable
    |> Cacheable.convert
```