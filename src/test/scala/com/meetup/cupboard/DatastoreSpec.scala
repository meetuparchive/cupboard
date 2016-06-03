package com.meetup.cupboard

import java.time.ZonedDateTime

import com.google.cloud.datastore.{Datastore, Entity, DateTime => GDateTime}
import org.scalatest._
// spray.json._
import com.meetup.cupboard.models.{Bar, Subscription, _}

class DatastoreSpec extends FlatSpec with Matchers with AdHocDatastore {

  import DatastoreFormats._

  "DatastoreFormats" should "serialize & deserialize case classes" in {
    withDatastore() { ds =>
      val z = Foo("hi", 3)

      val z1: Persisted[Foo] = Cupboard.save(ds, z, "customKind")
      val z2: Persisted[Foo] = Cupboard.save(ds, z)

      val z1restored: Persisted[Foo] = Cupboard.load[Foo](ds, z1.id, "customKind")
      z1 shouldBe z1restored
      val z2restored: Persisted[Foo] = Cupboard.load[Foo](ds, z2.id, "Foo")
      z2 shouldBe z2restored

      val bar = Bar(1, Foo("hi", 4))
      val barP: Persisted[Bar] = Cupboard.save(ds, bar)
      val barR: Persisted[Bar] = Cupboard.load[Bar](ds, barP.id, "Bar")
      barP shouldBe barR

    }
  }
}

