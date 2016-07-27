package com.meetup.cupboard

import java.time.ZonedDateTime
import com.meetup.cupboard.datastore.DatastoreProperties._

package object models {
  case class User(username: String, id: Int)
  object User extends Persistable[User] {
    val properties = Persistable.createProperties[User]
  }
}

