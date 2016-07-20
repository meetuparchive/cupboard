package com.meetup.cupboard.tests

import org.scalatest._
import com.meetup.cupboard.models._
import com.meetup.cupboard.datastore.DatastoreProperties._
import com.meetup.cupboard.datastore.DatastoreProperties.InstantDatastoreProperty
import com.google.cloud.datastore.{DateTime => GDateTime}
import com.meetup.cupboard.{Persistable, Property}
import java.time.Instant

import com.google.cloud.datastore.DateTime
import com.meetup.cupboard.datastore.DatastoreProperty

class MacroSpec extends FlatSpec with Matchers {
  "Persistable trait" should "extract class field info & conversion instances" in {
    /**
     * Given:
     *
     * case class User(memberId: Int, username: String, createdAt: java.time.Instant)
     * object User extends Persistable[User]
     */

    // The macro has extracted and created a Properties object which includes:
    //val expectedProperties = List(
    //  new Property[Int, User]("memberId"),
    //  new Property[String, User]("username"),
    //  new Property[Instant, User]("createdAt")
    //)

    //  User.properties.all shouldBe expectedProperties

    assert(User.properties.createdAt.name == "createdAt")

    User.properties.createdAt.getPropertyConverter shouldBe a[DatastoreProperty[_, _]]
  }
}
