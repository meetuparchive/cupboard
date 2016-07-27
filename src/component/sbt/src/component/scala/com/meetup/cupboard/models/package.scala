package com.meetup.cupboard

import java.time.ZonedDateTime

import spray.json.{ JsNumber, JsString, JsValue, JsonFormat }

package object models {
  case class Foo(s: String, i: Int)
  object Foo extends Persistable[Foo] {
    val properties = Persistable.createProperties[Foo]
  }
}

