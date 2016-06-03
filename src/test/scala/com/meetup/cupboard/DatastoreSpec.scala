package com.meetup.cupboard

import java.time.{Instant, ZoneOffset, ZonedDateTime}

import com.google.cloud.datastore.{Datastore, Entity, DateTime => GDateTime}
import org.scalatest._
// spray.json._
import com.meetup.cupboard.models.{Bar, Subscription, _}
import cats.data.Xor

class DatastoreSpec extends FunSpec with Matchers with AdHocDatastore {

  import DatastoreFormats._

  describe("DatastoreFormats") {
    val z = Foo("hi", 3)

    it("should serialize and deserialize simple case classes") {
      withDatastore() { ds =>

        val z1Result = Cupboard.save(ds, z, "customKind")

        if (z1Result.isLeft) {
          println("z1Result is left")
        }
        val z1P = z1Result.getOrElse(fail())
        val z1R = Cupboard.load[Foo](ds, z1P.id, "customKind")
        z1Result shouldBe z1R
        z1R.map(_.entity) shouldBe Xor.Right(z)
      }
    }
    it("should support custom kinds") {
      withDatastore() { ds =>

        val z2Result = Cupboard.save(ds, z)

        val z2P = z2Result.getOrElse(fail())

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

    it("should support sealed families of case classes") {
      withDatastore() { ds =>
        val status = SubscriptionStatus.Active

        val statusResult = Cupboard.save[SubscriptionStatus](ds, status, "SubscriptionStatus")
        val statusP = statusResult.getOrElse(fail())
        val statusR = Cupboard.load[SubscriptionStatus](ds, statusP.id, "SubscriptionStatus")
        statusP.entity.id shouldBe SubscriptionStatus.Active.id

        statusR shouldBe statusResult
      }
    }

    it("should support sealed families as individual properties") {
      withDatastore() { ds =>
        val zdt = ZonedDateTime.now()
        val now = ZonedDateTime.from(zdt.toInstant().atOffset(ZoneOffset.UTC))

        val subscription = Subscription.empty.copy(
          startDate = Some(now),
          renewDate = Some(now),
          status = SubscriptionStatus.Active)
        val subscriptionResult = Cupboard.save[Subscription](ds, subscription)
        val subscriptionP = subscriptionResult.getOrElse(fail())
        val subscriptionR = Cupboard.load[Subscription](ds, subscriptionP.id, "Subscription")
        subscriptionR shouldBe subscriptionResult

      }

    }
  }
}
