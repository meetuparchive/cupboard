package com.meetup.cupboard

import com.google.cloud.datastore.Datastore
import com.google.cloud.datastore.testing.LocalDatastoreHelper
import org.scalatest.Suite
import org.scalatest.exceptions.TestFailedException
import scala.util.control.NonFatal

trait AdHocDatastore extends Suite {
  def withDatastore[A](consistency: Double = .5)(f: Datastore => A): A = {
    println("Preparing to start up local datastore.")
    var lds: LocalDatastoreHelper = null
    try {
      lds = LocalDatastoreHelper.create(consistency)
      println("Local datastore created.")
      lds.start()
      println("Local datastore started.")
      val a = f(lds.options().service())
      println("Test function completed.")
      lds.stop()
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
      println("In finally block of AdHocDatastore.")
      if (lds != null)
        lds.stop()
    }
  }
}
