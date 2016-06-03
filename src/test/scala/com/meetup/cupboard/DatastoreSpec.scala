package com.meetup.cupboard

import java.time.ZonedDateTime

import com.google.cloud.datastore.{Datastore, Entity, DateTime => GDateTime}
import org.scalatest._
// spray.json._
import com.meetup.cupboard.models.{Bar, Subscription, _}
import cats.data.Xor

class DatastoreSpec extends FlatSpec with Matchers with AdHocDatastore {

  import DatastoreFormats._

  "DatastoreFormats" should "serialize & deserialize case classes" in {
    withDatastore() { ds =>
      val z = Foo("hi", 3)

      val z1Result = Cupboard.save(ds, z, "customKind")
      val z2Result = Cupboard.save(ds, z)

      val z1P = z1Result.getOrElse(fail())
      val z2P = z2Result.getOrElse(fail())

      val z1R = Cupboard.load[Foo](ds, z1P.id, "customKind")
      z1Result shouldBe z1R
      z1R.map(_.entity) shouldBe Xor.Right(z)

      val z2R = Cupboard.load[Foo](ds, z2P.id, "Foo")
      z2Result shouldBe z2R
      z2R.map(_.entity) shouldBe Xor.Right(z)

      val bar = Bar(1, Foo("hi", 4))
      val barResult = Cupboard.save(ds, bar)
      val barP = barResult.getOrElse(fail())

      val barR = Cupboard.load[Bar](ds, barP.id, "Bar")
      barResult shouldBe barR
      barR.map(_.entity) shouldBe Xor.Right(bar)
    }
  }
}

