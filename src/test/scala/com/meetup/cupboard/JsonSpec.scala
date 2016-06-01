package com.meetup.cupboard

import java.time.ZonedDateTime

import org.scalatest._
import spray.json._
import com.meetup.cupboard.models.{ Bar, Foo, Subscription, SubscriptionStatus }

class JsonSpec extends FlatSpec with Matchers {
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

    val zonedDateTime = ZonedDateTime.parse("2016-05-26T15:55:39.163-04:00[America/New_York]")

    val subscription = Subscription.empty.copy(
      startDate = Some(zonedDateTime),
      renewDate = Some(zonedDateTime),
      // status = SubscriptionStatus.Active,
      flag = 1,
      notes = "test notes"
    )
    val subscriptionJson = subscription.toJson
    print(subscriptionJson)
    val expected = """{"endDate":null,"renewDate":"2016-05-26T15:55:39.163-04:00[America/New_York]","trialEnd":null,"trialStart":null, "status": 0, "flag":1,"notes":"test notes","startDate":"2016-05-26T15:55:39.163-04:00[America/New_York]"}"""
    subscriptionJson shouldBe expected.parseJson

  }
}

