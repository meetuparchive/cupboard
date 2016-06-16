## Cupboard

[![Coverage Status](https://coveralls.io/repos/github/meetup/cupboard/badge.svg?branch=master&t=iBRIRu)](https://coveralls.io/github/meetup/cupboard?branch=master)

"library making it easier, safer, and less boilerplatey to store and retrieve models from GCD"

This library will persist Scala case classes to Google Cloud Datastore.

There are some restrictions to the case classes that it currently supports.

It supports classes with the following types of fields:
* `Int`, `Long`, `String`, `Boolean`
* `java.time.Instant`, `java.time.ZonedDateTime`, `java.time.Period`
* other case classes
* `List` of the above

It currently does not support abstract types (e.g. Enum-style case classes that implement a sealed trait), but
that's a desired future feature.

It's possible to add support for other column types or custom handling for a given class.
(An example should go here.)

### Sample Usage

```scala
package com.meetup.foo.models

case class Foo(s: String, i: Int)
case class Bar(i: Int, f: Foo)
```

```scala
val bar = Bar(2, Foo("hi", 3)
val persisted = Cupboard.save(ds, e)
```

```scala
val result = Cupboard.load(ds, id)
```



