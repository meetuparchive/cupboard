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

      val z1X = Cupboard.save(ds, z, "customKind")
      val z2X = Cupboard.save(ds, z)

      val z1id = z1X.getOrElse(fail()).id
      val z2id = z2X.getOrElse(fail()).id

      val z1restored = Cupboard.load[Foo](ds, z1id, "customKind")
      z1X shouldBe z1restored

      val z2 = z2X.getOrElse(fail())
      val z2restored = Cupboard.load[Foo](ds, z2id, "Foo")
      z2X shouldBe z2restored
    }
  }
}

