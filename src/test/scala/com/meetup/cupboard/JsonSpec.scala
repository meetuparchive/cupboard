package com.meetup.cupboard

import org.scalatest._
import spray.json._
import com.meetup.cupboard.models.{ Bar, Foo }

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

    // nested case class example
    val bar1 = Bar(3, Foo("foo", 2))
    val barJson = bar1.toJson
    barJson shouldBe """{"f":{"i":2,"s":"foo"},"i":3}""".parseJson
    val bar1restored = barJson.convertTo[Bar]
    bar1restored shouldBe bar1

  }
}

