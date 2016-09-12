package com.meetup.cupboard

import java.util.concurrent.atomic.AtomicInteger

import com.google.cloud.datastore.Datastore
import com.google.cloud.datastore.testing.LocalDatastoreHelper
import org.scalatest.Suite
import org.scalatest.exceptions.TestFailedException

import scala.util.control.NonFatal

object AdHocDatastore {
  var cachedDatastore: Option[Datastore] = None
  var count: Int = 0
}
trait AdHocDatastore extends Suite {
  def withDatastore[A](consistency: Double = .5)(f: Datastore => A): A = {
    println("Preparing to start up local datastore.")
    var lds: LocalDatastoreHelper = null
    try {
      val ds: Datastore = AdHocDatastore.cachedDatastore match {
        case None => {
          lds = LocalDatastoreHelper.create(consistency)
          println("Local datastore created.")
          lds.start()
          println("Local datastore started.")
          val cached = lds.options().service()
          AdHocDatastore.cachedDatastore = Some(cached)
          AdHocDatastore.count += 1
          cached
        }
        case Some(cached) => {
          AdHocDatastore.count += 1
          cached
        }
      }
      val a = f(ds)
      println("Test function completed.")
      //lds.stop()
      println("Datastore stopped.")
      a
    } catch {
      case e: TestFailedException => {
        println("Test failed exception.")
        fail(e)
      }
      case NonFatal(e) => {
        e.printStackTrace()
        cancel(e)
      }
    } finally {
      AdHocDatastore.count -= 1
      println(s"In finally block of AdHocDatastore: $count")
      //if (AdHocDatastore.count == 0) lds.stop()
    }
  }
}
