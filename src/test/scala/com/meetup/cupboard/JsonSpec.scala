package com.meetup.cupboard

import org.scalatest._
import spray.json._

import com.meetup.cupboard.models.Foo

class FamilyFormatsSpec extends FlatSpec with Matchers {
  import JsonFormats._

  "JsonFormats" should "serialize & deserialize case classes" in {
    // serialize
    val foo1 = Foo("foo", 1)
    val fooJson = foo1.toJson
    println(fooJson)
    fooJson shouldBe """{"i":1,"s":"foo"}""".parseJson

    // deserialize
    val foo1restored = fooJson.convertTo[Foo]
    foo1restored shouldBe foo1

  }
}

