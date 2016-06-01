package com.meetup.cupboard

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME

import shapeless.labelled._
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import com.google.cloud.datastore.{Datastore, Entity, DateTime => GDateTime}
import com.google.datastore.v1beta3.EntityOrBuilder

trait DatastoreFormat[A] {
  def fromEntity(e: Entity): A
  def buildEntity(a: A, e: Entity.Builder): Entity.Builder
}

trait DatastoreProperty[V, D] {
  def getValueFromEntity(fieldName: String, e: Entity): V
  def setEntityProperty(v: V, fieldName: String, e: Entity.Builder): Entity.Builder
}

/**
 * This exists primarily to test the conversion between case classes and a representation,
 * and for educational purposes because JSON is easier to understand and it is well documented.
 *
 * But we could choose to keep it if we want.
 */
object DatastoreFormats {

  implicit object hNilFormat extends DatastoreFormat[HNil] {
    def fromEntity(j: Entity): HNil = HNil
    def buildEntity(h: HNil, e: Entity.Builder): Entity.Builder = e
  }

  implicit def hListFormat[Key <: Symbol, Value, Remaining <: HList, DatastoreValue](
    implicit
    key: Witness.Aux[Key],
    propertyConverter: DatastoreProperty[Value, DatastoreValue],
    headFormat: DatastoreFormat[Value],
    tailFormat: DatastoreFormat[Remaining]
  ): DatastoreFormat[FieldType[Key, Value] :: Remaining] = new DatastoreFormat[FieldType[Key, Value] :: Remaining] {

    //val headFormat = lazyHeadFormat.value
    //val tailFormat = lazyTailFormat.value

    def buildEntity(hlist: FieldType[Key, Value] :: Remaining, e: Entity.Builder): Entity.Builder = {
      val tailEntity = tailFormat.buildEntity(hlist.tail, e)
      val fieldName = key.value.name // the name was part of the tagged type
      propertyConverter.setEntityProperty(hlist.head, fieldName, tailEntity)
    }

    def fromEntity(e: Entity): A = {
      val tail = tailFormat.fromEntity(e)
      val head = headFormat.fromEntity(e)
    }
    /*
    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      val head = headFormat.read(fields(key.value.name))
      val tail = tailFormat.read(json)
      field[Key](head) :: tail
    }*/
  }

  /**
   * The following code is what allows us to make the leap from case classes
   * to HLists of FieldType[Key, Value].
   *
   * If you want to know more, look at LabelledGeneric in shapeless (as well
   * as the idea of Singleton types).
   */
  /*
  implicit def jsonFormat[T, Repr](
    implicit
    gen: LabelledGeneric.Aux[T, Repr],
    lazySg: Lazy[JsonFormat[Repr]]
  ): JsonFormat[T] = new JsonFormat[T] {
    val sg = lazySg.value

    def read(j: JsValue): T = gen.from(sg.read(j))
    def write(t: T): JsValue = sg.write(gen.to(t))
  }*/
}
