package com.meetup.cupboard

import java.io.File
import com.meetup.cupboard.models._
import com.meetup.cupboard.datastore.DatastoreProperties._

import org.scalatest.{FunSpec, Matchers}

class JarCupboardComponentTest extends FunSpec with Matchers {
  import com.meetup.cupboard.models.Foo

  describe("jar-cupboard-test") {
    it("macro should work as dependency") {
        assert(User.properties.createdAt.name == "createdAt")
    }
  }
}
