package com.meetup.cupboard

import java.io.File
import com.meetup.cupboard.models._
import com.meetup.cupboard.datastore.DatastoreProperties._

import org.scalatest.{FunSpec, Matchers}
import com.meetup.cupboard.models.User

class JarCupboardComponentTest extends FunSpec with Matchers {

  describe("jar-cupboard-test") {
    it("macro should work as dependency") {
        assert(User.properties.username.name == "username")
    }
  }
}
