package com.meetup.cupboard

import java.time.{Period, ZonedDateTime}

import cats.data.Xor
import com.google.cloud.datastore.Entity.Builder
import com.google.cloud.datastore.FullEntity
import com.meetup.cupboard.models.PlanStatus.PlanStatus
import spray.json.{JsNumber, JsString, JsValue, JsonFormat}

import com.meetup.cupboard.datastore.DatastoreProperties._

package object models {
  case class User(memberId: Int, username: String, createdAt: java.time.Instant)
  object User extends Persistable[User]

  case class Simple(s: String)
  object Simple extends Persistable[User]

  case class Foo(s: String, i: Int, b: Boolean)
  object Foo extends Persistable[Foo]

  case class Bar(i: Int, f: Foo)
  object Bar extends Persistable[Bar]

  case class Qux[T](i: Int)
  // object Qux extends Persistable[Qux[T]]

  case class Phantom[T, U](i: Int)
  //object Phantom extends Persistable[Phantom]

  case class Many(seq: List[Simple])
  object Many extends Persistable[Many]

  case class BigDecimalTest(bd: BigDecimal)
  object BigDecimalTest extends Persistable[BigDecimalTest]

  case class SeqStringTest(foo: Seq[String])
  object SeqStringTest extends Persistable[SeqStringTest]

  case class SeqIntTest(foo: Seq[Int])
  object SeqIntTest extends Persistable[SeqIntTest]

  case class ZonedDateTimeTest(d: ZonedDateTime)
  object ZonedDateTimeTest extends Persistable[ZonedDateTimeTest]

  case class Subscription(
    startDate: Option[ZonedDateTime],
    endDate: Option[ZonedDateTime],
    renewDate: Option[ZonedDateTime],
    trialStart: Option[ZonedDateTime],
    trialEnd: Option[ZonedDateTime],
    status: SubscriptionStatus,
    notes: String,
    flag: Int
  )
  object Subscription extends Persistable[Subscription] {
    val empty = Subscription(None, None, None, None, None, SubscriptionStatus.New, "", 0)
    //val empty = Subscription(None, None, None, None, None, "", 0)
  }

  case class TrialPeriod(p: Period)
  object TrialPeriod extends Persistable[TrialPeriod]

  /* MeetupStatus.OrgSub */
  sealed abstract class SubscriptionStatus(val id: Int)

  case class Plan(
    name: String,
    entitlements: Entitlements,
    status: PlanStatus)

  case class Entitlements(maxGroups: Option[Long], maxUsers: Option[Int])
  object Entitlements extends Persistable[Entitlements]
  //case object BasicEntitlement extends Entitlements(Some(3), Some(50))
  //case object UnlimitedEntitlement extends Entitlements(Some(3), None)
  //case object ChapterizerUnlimitedEntitlement extends Entitlements(None, None)

  object PlanStatus {
    sealed abstract class PlanStatus
    case object Available extends PlanStatus
    case object Unavailable extends PlanStatus
  }

  object SubscriptionStatus {
    case object Expired extends SubscriptionStatus(-2)
    case object Ending extends SubscriptionStatus(-1)
    case object New extends SubscriptionStatus(0)
    case object Pending extends SubscriptionStatus(1)
    case object Active extends SubscriptionStatus(2)

    // This is necessary because we haven't added support for handling sealed families of case classes yet.
    // See SubscriptionStatusFormat.
    def statusById(i: Int): SubscriptionStatus = i match {
      case -2 => Expired
      case -1 => Ending
      case 0 => New
      case 1 => Pending
      case 2 => Active
    }
  }

  /* If we were to define a "coproduct" format, this would not be necessary --
     the coproduct format would allow us to serialize/deserialize sealed families
     of case classes, like SubscriptionStatus above.
   */
  //TODO: implement coproduct format for sealed families of case classes
  implicit object SubscriptionStatusFormat extends JsonFormat[SubscriptionStatus] {
    override def write(status: SubscriptionStatus) = JsNumber(status.id)

    override def read(json: JsValue): SubscriptionStatus = json match {
      case JsNumber(i) => SubscriptionStatus.statusById(i.toInt)
      case _ => throw new RuntimeException("Date must be encoded as string")
    }
  }

  sealed abstract class RenewalDuration(val period: Period)

  object RenewalDuration {
    case object MonthlyRenewal extends RenewalDuration(Period.ofMonths(1))
    case object DailyRenewal extends RenewalDuration(Period.ofDays(1))
    case object SixMonthsRenewal extends RenewalDuration(Period.ofMonths(6))
  }

}

