package com.meetup.cupboard

import com.google.cloud.datastore.Datastore
import com.google.cloud.datastore.testing.LocalDatastoreHelper
import org.scalatest.Suite
import org.scalatest.exceptions.TestFailedException
import scala.util.control.NonFatal

trait AdHocDatastore extends Suite {
  def withDatastore[A](consistency: Double = .5)(f: Datastore => A): A = {
    var lds: LocalDatastoreHelper = null
    try {
      lds = LocalDatastoreHelper.create(consistency)
      lds.start()
      val a = f(lds.options().service())
      lds.stop()
      a
    } catch {
      case e: TestFailedException => fail(e)
      case NonFatal(e) => {
        e.printStackTrace()
        cancel(e)
      }
    } finally {
      if (lds != null)
        lds.stop()
    }
  }
}