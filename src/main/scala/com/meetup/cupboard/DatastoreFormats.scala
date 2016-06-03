package com.meetup.cupboard

import java.time.{Instant, ZoneOffset, ZonedDateTime}

import shapeless.labelled._
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import com.google.cloud.datastore.{Entity, FullEntity, DateTime => GDateTime, Key}
import com.google.datastore.v1beta3.client.DatastoreHelper

object DatastoreFormats {

  /////////
  // DatastoreProperties define how to translate individual values from the case class type to the datastore type.
  // TODO: Move these to a trait which DatastoreFormats extends.
  /////////

  trait DatastoreProperty[V, D] {
    def getValueFromEntity(fieldName: String, e: FullEntity[Key]): V

    def setEntityProperty(v: V, fieldName: String, e: Entity.Builder): Entity.Builder
  }

  implicit def EntityDatastoreProperty[E](implicit entityDatastoreFormat: DatastoreFormat[E]): DatastoreProperty[E, FullEntity[Key]] = new DatastoreProperty[E, FullEntity[Key]] {
    def getValueFromEntity(name: String, e: FullEntity[Key]): E = {

      val internalEntity: FullEntity[Key] = e.getEntity(name)
      val subentity: E = entityDatastoreFormat.fromEntity(internalEntity)
      subentity

    }
    def setEntityProperty(v: E, name: String, e: Entity.Builder): Entity.Builder = {
      val emptyEntity = FullEntity.builder()
      val newEntity = entityDatastoreFormat.buildEntity(v, e)
      e.set(name, newEntity.build())
    }

  }

  implicit object StringDatastoreProperty extends DatastoreProperty[String, String] {
    def getValueFromEntity(name: String, e: FullEntity[Key]): String = {
      e.getString(name)
    }

    def setEntityProperty(v: String, name: String, e: Entity.Builder): Entity.Builder = {
      e.set(name, v)
    }
  }

  implicit object IntDatastoreProperty extends DatastoreProperty[Int, Int] {
    def getValueFromEntity(name: String, e: FullEntity[Key]): Int = {
      e.getLong(name).toInt
    }

    def setEntityProperty(v: Int, name: String, e: Entity.Builder) = {
      e.set(name, v)
    }
  }

  implicit object ZonedDateTimeDatastoreProperty extends DatastoreProperty[ZonedDateTime, GDateTime] {
    def getValueFromEntity(name: String, e: FullEntity[Key]): ZonedDateTime = {
      val millis: Long = e.getDateTime(name).timestampMillis()
      ZonedDateTime.from(Instant.ofEpochMilli(millis).atOffset(ZoneOffset.UTC))
    }
    def setEntityProperty(v: ZonedDateTime, name: String, e: Entity.Builder) = {
      e.set(name, GDateTime.copyFrom(java.util.Date.from(v.toInstant())))
    }
  }

  implicit object InstantDatastoreProperty extends DatastoreProperty[Instant, GDateTime] {
    def getValueFromEntity(name: String, e: FullEntity[Key]): Instant = {
      val millis: Long = e.getDateTime(name).timestampMillis()
      Instant.ofEpochMilli(millis)
    }
    def setEntityProperty(v: Instant, name: String, e: Entity.Builder) = {
      e.set(name, GDateTime.copyFrom(java.util.Date.from(v)))
    }
  }

  /// The following section uses the Shapeless library to allow us to work with case classes in a generic way.
  /// You may need to refer to the Shapeless documentation to get a good sense of what this is doing.

  trait DatastoreFormat[A] {
    def fromEntity(e: FullEntity[Key]): A

    def buildEntity(a: A, e: Entity.Builder): Entity.Builder
  }

  implicit object hNilFormat extends DatastoreFormat[HNil] {
    def fromEntity(j: FullEntity[Key]): HNil = HNil

    def buildEntity(h: HNil, e: Entity.Builder): Entity.Builder = e
  }

  implicit def hListFormat[FieldKey <: Symbol, Value, Remaining <: HList, DatastoreValue](
    implicit
    key: Witness.Aux[FieldKey],
    propertyConverter: DatastoreProperty[Value, DatastoreValue],
    tailFormat: DatastoreFormat[Remaining]
  ): DatastoreFormat[FieldType[FieldKey, Value] :: Remaining] = new DatastoreFormat[FieldType[FieldKey, Value] :: Remaining] {

    def buildEntity(hlist: FieldType[FieldKey, Value] :: Remaining, e: Entity.Builder): Entity.Builder = {
      val tailEntity = tailFormat.buildEntity(hlist.tail, e)
      val fieldName = key.value.name // the name was part of the tagged type
      propertyConverter.setEntityProperty(hlist.head, fieldName, tailEntity)
      e
    }

    def fromEntity(e: FullEntity[Key]): ::[FieldType[FieldKey, Value], Remaining] = {
      val fieldName = key.value.name
      val v = propertyConverter.getValueFromEntity(fieldName, e)
      val tail = tailFormat.fromEntity(e)
      field[FieldKey](v) :: tail
    }

  }

  /**
   * The following code is what allows us to make the leap from case classes
   * to HLists of FieldType[Key, Value].
   *
   * If you want to know more, look at LabelledGeneric in shapeless (as well
   * as the idea of Singleton types).
   */

  implicit def datastoreFormat[T, Repr](
    implicit
    gen: LabelledGeneric.Aux[T, Repr],
    lazySg: Lazy[DatastoreFormat[Repr]]
  ): DatastoreFormat[T] = new DatastoreFormat[T] {
    val sg = lazySg.value

    def fromEntity(j: FullEntity[Key]): T = gen.from(sg.fromEntity(j))

    def buildEntity(t: T, e: Entity.Builder): Entity.Builder = sg.buildEntity(gen.to(t), e)
  }
}
