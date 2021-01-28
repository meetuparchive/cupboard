# Cupboard

[![Build Status](https://travis-ci.org/meetup/cupboard.svg?branch=main)](https://travis-ci.org/meetup/cupboard)
[![Coverage Status](https://coveralls.io/repos/github/meetup/cupboard/badge.svg?branch=master&t=iBRIRu)](https://coveralls.io/github/meetup/cupboard?branch=master)

###
Note there is currently a known performance issue with cupboard, to be resolved in an upcoming release.
We believe that the cause is that we're not properly maintaining open connections to google datastore.
###

"library making it easier, safer, and less boilerplatey to store and retrieve models from GCD"

This library will persist Scala case classes to Google Cloud Datastore, and perform type-safe queries.

There are some restrictions to the case classes that it currently supports.

It supports classes with the following types of fields:
* `Int`, `Long`, `String`, `Boolean`, `BigDecimal`
* `java.time.Instant`, `java.time.ZonedDateTime`, `java.time.Period`
* other case classes
* `Seq` of the above

Cupboard does support using ancestor paths for strong consistency queries.

It currently does not support abstract types (e.g. Enum-style case classes that implement a sealed trait), but
that's a desired future feature.

It's possible to add support for other column types or custom handling for a given class.
(An example should go here.)

### Sample Usage

```scala
case class Foo(s: String, i: Int)
object Foo extends Persistable[Foo] {
  val properties = Persistable.createProperties[Foo]
}

// connect to datastore
val datastoreOptions = DatastoreOptions.defaultInstance()
val datastore = datastoreOptions.service()

// persist a case class
val foo = Foo("hi", 3)
val saveResult: Xor[Throwable, Persisted[Foo]] = Cupboard.save(datastore, foo)

// load a case class
val entityXor = Cupboard.load[Foo](datastore, id)

// query
val entities = EntityQuery[Foo]()
    .filter(Foo.properties.i eq 5)
    .resultAsSeq(datastore)
    
```



