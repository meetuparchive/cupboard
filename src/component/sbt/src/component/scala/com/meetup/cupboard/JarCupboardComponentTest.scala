package com.meetup.cupboard

import java.io.File
import com.meetup.cupboard.JsonFormats._
import spray.json._

import org.scalatest.{FunSpec, Matchers}

class JarCupboardComponentTest extends FunSpec with Matchers {
  import com.meetup.cupboard.models.Foo

  describe("jar-cupboard-test") {
    it("should work as dependency") {
      // serialize
      val x: JsonFormat[Foo] = implicitly[JsonFormat[Foo]]

      val foo1 = Foo("foo", 1)
      val fooJson = foo1.toJson
      println(fooJson)
      fooJson shouldBe """{"i":1,"s":"foo"}""".parseJson

      // deserialize
      val foo1restored = fooJson.convertTo[Foo]
      foo1restored shouldBe foo1
    }
  }
}
