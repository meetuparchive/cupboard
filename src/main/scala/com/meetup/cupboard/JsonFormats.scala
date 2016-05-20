package com.meetup.cupboard

import shapeless.labelled._
import shapeless.{ ::, HList, HNil, LabelledGeneric, Lazy, Witness }
import spray.json.{ DefaultJsonProtocol, JsObject, JsValue, JsonFormat }

/**
 * This exists primarily to test the conversion between case classes and a representation,
 * and for educational purposes because JSON is easier to understand and it is well documented.
 *
 * But we could choose to keep it if we want.
 */
object JsonFormats extends DefaultJsonProtocol {

  // If you are reading this, keep in mind that there are lots of instances of JsonFormat[T] provided
  // by spray.json, e.g. JsonFormat[String], JsonFormat[Int], etc. etc.  This is a typeclass for anything
  // that can be turned into Json.  If we were implementing this for, say, Google Datastore, we'd need to
  // be providing these instances ourselves.

  // The big picture idea of hNilFormat and hListFormat is that we're providing typeclass instances for
  // an HList (a typed list from Shapeless) of values that have their field names stored as part of their
  // types.  To understand that last part better, you can look up Singleton Types and other shapeless documentation.
  implicit object hNilFormat extends JsonFormat[HNil] {
    def read(j: JsValue) = HNil
    def write(n: HNil) = JsObject()
  }

  implicit def hListFormat[Key <: Symbol, Value, Remaining <: HList](
    implicit
    key: Witness.Aux[Key],
    headFormat: JsonFormat[Value],
    tailFormat: JsonFormat[Remaining]
  ): JsonFormat[FieldType[Key, Value] :: Remaining] = new JsonFormat[FieldType[Key, Value] :: Remaining] {

    //val headFormat = lazyHeadFormat.value
    //val tailFormat = lazyTailFormat.value

    def write(hlist: FieldType[Key, Value] :: Remaining) = {
      val tailJs = tailFormat.write(hlist.tail).asJsObject()
      val headJs = headFormat.write(hlist.head)

      val fieldName = key.value.name // the name was part of the tagged type

      JsObject(tailJs.fields + (fieldName -> headJs))
    }

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      val head = headFormat.read(fields(key.value.name))
      val tail = tailFormat.read(json)
      field[Key](head) :: tail
    }
  }

  /**
   * The following code is what allows us to make the leap from case classes
   * to HLists of FieldType[Key, Value].
   *
   * If you want to know more, look at LabelledGeneric in shapeless (as well
   * as the idea of Singleton types).
   */
  implicit def jsonFormat[T, Repr](
    implicit
    gen: LabelledGeneric.Aux[T, Repr],
    lazySg: Lazy[JsonFormat[Repr]]
  ): JsonFormat[T] = new JsonFormat[T] {
    val sg = lazySg.value

    def read(j: JsValue): T = gen.from(sg.read(j))
    def write(t: T): JsValue = sg.write(gen.to(t))
  }
}
