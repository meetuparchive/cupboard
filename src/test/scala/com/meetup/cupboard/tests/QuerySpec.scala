package com.meetup.cupboard.tests

import java.time.{Period, ZoneOffset, ZonedDateTime}

import cats.data.Xor
import com.google.cloud.datastore.Datastore
import com.meetup.cupboard._
import com.meetup.cupboard.models.{Bar, Subscription, _}
import org.scalatest._

import scala.reflect.runtime.universe.WeakTypeTag
import scala.reflect.ClassTag
import com.meetup.cupboard.datastore.DatastoreProperties._

class QuerySpec extends FunSpec with Matchers with AdHocDatastore {
  describe("Queries") {
    it("should load entities with a filter") {
      withDatastore() { ds =>

        // for consistency in this query, we need a parent entity
        // (otherwise this test would fail 50% of the time)
        val parentEntity = Foo("parent", -1, false)
        val parent = Cupboard.save(ds, parentEntity)
        val persistedParent: Persisted[Foo] = parent.getOrElse(fail())

        val ancestorKey = Cupboard.getKeyWithId(ds, "Foo", persistedParent.id)

        // save 10 instances of Foo
        for (i <- 1 until 10) {
          val z1 = Foo(s"test${i}_1", i, i % 2 == 0)
          val z2 = Foo(s"test${i}_2", i, i % 2 == 0)
          val z3 = Foo(s"test${i}_3", i, i % 2 == 0)

          Cupboard.saveWithAncestor(ds, z1, "Foo", "Foo", persistedParent.id)
          Cupboard.saveWithAncestor(ds, z2, "Foo", "Foo", persistedParent.id)
          Cupboard.save(ds, z3)
        }

        val filter = Foo.properties.i.eq(3)

        val resultXor = EntityQuery[Foo]()
          .filter(filter)
          .ancestorKey(ancestorKey)
          .resultAsSeq(ds)

        val results = resultXor.getOrElse(fail())
        assert(results.exists(foo => foo.entity.s == "test3_1"))
        assert(results.exists(foo => foo.entity.s == "test3_2"))
        assert(!results.exists(foo => foo.entity.s == "test3_3"))

        val noFilterResult = EntityQuery[Foo]()
          .ancestorKey(ancestorKey)
          .resultAsSeq(ds)
          .toOption
        noFilterResult match {
          case None => fail()
          case Some(seq) => assert(seq.length == 21)
        }
      }
    }
  }
}
