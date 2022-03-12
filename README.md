# Cacheable

About
-----
An F# library to help compose caching functions that have triggers for resets.

Why would you ever use this?
-----
Suppose you have a system designed around multiple "Lookup" functions that all pull data from a database. As repeated calls to the database can be costly, you'll probably cache
the data in these objects. However, other services may also be writing to these databases at the same time so you'll have a system that notifies you when you need to flush
specific Lookup functions.

You may also have functions that make use of many of these Lookup functions and perform their own work that is also time consuming, and therefore needs to be cached.
This library will help you thread the invalidation notifications through the graph of functions to ensure all caches are invalidated correctly.

Key features of the library
-----
The first key feature is invalidation management. The aim of the library is to manage how you compose cacheable functions together so that they automatically
invalidate their caches when their dependencies change. This should hopefully remove a lot of bugs!

The second key feature is automatic memoisation. The library will generate a memoised function for you. The aim is to make these generated functions as efficient
as the memoised functions you'd normally write by hand.

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
let constructCompanyListCacheable =
    Cacheable.lift constructCompanyList CachingStrategy.All
    |> Cacheable.applyArg getUserCacheable
    |> Cacheable.applyArg getCompanyCacheable
    |> Cacheable.convert
```