package com.meetup.cupboard.tests

import java.time.{Period, ZoneOffset, ZonedDateTime}

import cats.data.Xor
import com.google.cloud.datastore.Datastore
import com.meetup.cupboard.{Filter, _}
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
        for (i <- 1 to 10) {
          val z1 = Foo(s"test${i}_1", i, i % 2 == 0)
          val z2 = Foo(s"test${i}_2", i, i % 2 == 0)
          val z3 = Foo(s"test${i}_3", i, i % 2 == 0)

          Cupboard.saveWithAncestor(ds, z1, "Foo", "Foo", persistedParent.id)
          Cupboard.saveWithAncestor(ds, z2, "Foo", "Foo", persistedParent.id)
          Cupboard.save(ds, z3)
        }

        // "i eq 3" is scala infix notation for "i.eq(3)"
        val filter = Foo.properties.i eq 3


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

        val mkQuery: (Filter) => Option[Seq[Persisted[Foo]]] = {
          EntityQuery[Foo]()
            .filter(_)
            .ancestorKey(ancestorKey)
            .resultAsSeq(ds)
            .toOption
        }
        val countFilterResults = (foos: Option[Seq[Persisted[Foo]]], n: Int) =>
          foos match {
            case Some(seq) => assert(seq.length == n)
            case None => fail
          }

        countFilterResults(mkQuery(Foo.properties.i lt 3), 5)
        countFilterResults(mkQuery(Foo.properties.i le 3), 7)
        countFilterResults(mkQuery(Foo.properties.i gt 3), 14)
        countFilterResults(mkQuery(Foo.properties.i ge 3), 16)

        countFilterResults(mkQuery(Foo.properties.s eq "test2_1"), 1)
        countFilterResults(mkQuery(Foo.properties.s lt "test2_1"), 5)
        countFilterResults(mkQuery(Foo.properties.s le "test2_1"), 6)
        countFilterResults(mkQuery(Foo.properties.s gt "test2_1"), 15)
        countFilterResults(mkQuery(Foo.properties.s ge "test2_1"), 16)

      }
    }
  }
}

