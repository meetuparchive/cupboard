package com.meetup.cupboard.datastore

import java.time.{Instant, ZoneOffset, ZonedDateTime}

import cats.data.Xor
import com.google.cloud.datastore.{DateTime, Entity, FullEntity, Key}
import com.meetup.cupboard.DatastoreFormats.DatastoreFormat
import com.google.cloud.datastore.{Entity, FullEntity, Key, DateTime => GDateTime}

trait DatastoreProperties {
  /////////
  // DatastoreProperties define how to translate individual values from the case class type to the datastore type.
  // TODO: Move these to a trait which DatastoreFormats extends.
  /////////

  trait DatastoreProperty[V, D] {
    def getValueFromEntity(fieldName: String, e: FullEntity[Key]): Xor[Throwable, V]

    def setEntityProperty(v: V, fieldName: String, e: Entity.Builder): Entity.Builder
  }

  implicit def EntityDatastoreProperty[E](implicit entityDatastoreFormat: DatastoreFormat[E]): DatastoreProperty[E, FullEntity[Key]] =
    new DatastoreProperty[E, FullEntity[Key]] {
      def getValueFromEntity(name: String, e: FullEntity[Key]) = {
        val internalEntity: FullEntity[Key] = e.getEntity(name)
        entityDatastoreFormat.fromEntity(internalEntity)
      }
      def setEntityProperty(v: E, name: String, e: Entity.Builder): Entity.Builder = {
        val emptyEntity = FullEntity.builder()
        val newEntity = entityDatastoreFormat.buildEntity(v, e)
        e.set(name, newEntity.build())
      }
    }

  implicit object StringDatastoreProperty extends DatastoreProperty[String, String] {
    def getValueFromEntity(name: String, e: FullEntity[Key]) = {
      Xor.catchNonFatal(e.getString(name))
    }

    def setEntityProperty(v: String, name: String, e: Entity.Builder): Entity.Builder = {
      e.set(name, v)
    }
  }

  implicit object IntDatastoreProperty extends DatastoreProperty[Int, Int] {
    def getValueFromEntity(name: String, e: FullEntity[Key]) = {
      Xor.catchNonFatal(e.getLong(name).toInt)
    }

    def setEntityProperty(v: Int, name: String, e: Entity.Builder) = {
      e.set(name, v)
    }
  }

  implicit object ZonedDateTimeDatastoreProperty extends DatastoreProperty[ZonedDateTime, GDateTime] {
    def getValueFromEntity(name: String, e: FullEntity[Key]) = {
      Xor.catchNonFatal {
        val millis: Long = e.getDateTime(name).timestampMillis()
        ZonedDateTime.from(Instant.ofEpochMilli(millis).atOffset(ZoneOffset.UTC))
      }
    }
    def setEntityProperty(v: ZonedDateTime, name: String, e: Entity.Builder) = {
      e.set(name, GDateTime.copyFrom(java.util.Date.from(v.toInstant())))
    }
  }

  implicit object InstantDatastoreProperty extends DatastoreProperty[Instant, GDateTime] {
    def getValueFromEntity(name: String, e: FullEntity[Key]) = {
      Xor.catchNonFatal {
        val millis: Long = e.getDateTime(name).timestampMillis()
        Instant.ofEpochMilli(millis)
      }
    }
    def setEntityProperty(v: Instant, name: String, e: Entity.Builder) = {
      e.set(name, GDateTime.copyFrom(java.util.Date.from(v)))
    }
  }

}
