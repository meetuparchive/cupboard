package com.meetup.cupboard.datastore

import java.time.{Instant, ZoneOffset, ZonedDateTime}

import cats.data.Xor
import com.google.cloud.datastore.{DateTime => GDateTime, _}
import com.meetup.cupboard.DatastoreFormats.DatastoreFormat
import com.meetup.cupboard.Result

import scala.collection.JavaConversions._

/**
  *   DatastoreProperties define how to translate individual values from the case class type to the datastore type.
  *
  *   We extend LowPriorityProperties as a way of setting the priority order of typeclass instances where more than
  *   one can apply.
  */
trait DatastoreProperties extends LowPriorityProperties {
  implicit def SeqEntityProperty[E](implicit entityDatastoreFormat: DatastoreFormat[E]): DatastoreProperty[List[E], java.util.List[FullEntity[_]]] = {
    new DatastoreProperty[List[E], java.util.List[FullEntity[_]]] {
      def getValueFromEntity(name: String, e: FullEntity[_]) = {
        val internalEntities: java.util.List[EntityValue] = e.getList[EntityValue](name)
        val entities = internalEntities.map(e => entityDatastoreFormat.fromEntity(e.get())).toList
        sequence(entities)
      }

      def setEntityProperty(v: List[E], name: String, e: Entity.Builder): Entity.Builder = {
        val entities = v.map { x =>
          val emptyEntity = FullEntity.builder()
          val newEntity = entityDatastoreFormat.buildEntity(x, e).build()
          new EntityValue(newEntity)
        }
        //com.google.cloud.datastore.Value.
        import scala.collection.JavaConversions._

        val entities2 = entities.toBuffer
        e.set(name, entities2)
      }
      import PartialFunction._

      // turn a list of Xors into an Xor of a list
      def sequence(input: List[Xor[Throwable, E]]): Xor[Throwable, List[E]] = {
        input.foldRight[Xor[Throwable, List[E]]](Xor.Right(Nil)) { (o, ol) =>
          (o, ol) match {
            case (Xor.Right(x), Xor.Right(xs)) => Xor.Right(x :: xs)
            case (Xor.Left(y), _) => Xor.Left(y)
            case (_, Xor.Left(z)) => Xor.Left(z)
          }
        }
      }
    }

  }
}

trait LowPriorityProperties {

  trait DatastoreProperty[V, D] {
    def getValueFromEntity(fieldName: String, e: FullEntity[_]): Xor[Throwable, V]

    def setEntityProperty(v: V, fieldName: String, e: Entity.Builder): Entity.Builder
  }

  implicit object StringDatastoreProperty extends DatastoreProperty[String, String] {
    def getValueFromEntity(name: String, e: FullEntity[_]) = {
      Xor.catchNonFatal(e.getString(name))
    }

    def setEntityProperty(v: String, name: String, e: Entity.Builder): Entity.Builder = {
      e.set(name, v)
    }
  }

  implicit object IntDatastoreProperty extends DatastoreProperty[Int, Int] {
    def getValueFromEntity(name: String, e: FullEntity[_]) = {
      Xor.catchNonFatal(e.getLong(name).toInt)
    }

    def setEntityProperty(v: Int, name: String, e: Entity.Builder) = {
      e.set(name, v)
    }
  }

  implicit object ZonedDateTimeDatastoreProperty extends DatastoreProperty[ZonedDateTime, GDateTime] {
    def getValueFromEntity(name: String, e: FullEntity[_]) = {
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
    def getValueFromEntity(name: String, e: FullEntity[_]) = {
      Xor.catchNonFatal {
        val millis: Long = e.getDateTime(name).timestampMillis()
        Instant.ofEpochMilli(millis)
      }
    }
    def setEntityProperty(v: Instant, name: String, e: Entity.Builder) = {
      e.set(name, GDateTime.copyFrom(java.util.Date.from(v)))
    }
  }

  implicit def EntityDatastoreProperty[E](implicit entityDatastoreFormat: DatastoreFormat[E]): DatastoreProperty[E, FullEntity[_]] =
    new DatastoreProperty[E, FullEntity[_]] {
      def getValueFromEntity(name: String, e: FullEntity[_]) = {
        val internalEntity: FullEntity[_] = e.getEntity(name)
        entityDatastoreFormat.fromEntity(internalEntity)
      }
      def setEntityProperty(v: E, name: String, e: Entity.Builder): Entity.Builder = {
        val emptyEntity = FullEntity.builder()
        val newEntity = entityDatastoreFormat.buildEntity(v, e)
        e.set(name, newEntity.build())
      }
    }

  /*  implicit def SeqEntityProperty[E](implicit entityDatastoreFormat: DatastoreFormat[E]): DatastoreProperty[List[E], java.util.List[FullEntity[_]]] = {
    new DatastoreProperty[List[E], java.util.List[FullEntity[_]]] {
      def getValueFromEntity(name: String, e: FullEntity[_]) = {
        val internalEntities: java.util.List[EntityValue] = e.getList[EntityValue](name)
        val entities = internalEntities.map(e => entityDatastoreFormat.fromEntity(e.get())).toList
        sequence(entities)
      }

      def setEntityProperty(v: List[E], name: String, e: Entity.Builder): Entity.Builder = {
        val entities = v.map { x =>
          val emptyEntity = FullEntity.builder()
          val newEntity = entityDatastoreFormat.buildEntity(x, e).build()
          new EntityValue(newEntity)
        }
        //com.google.cloud.datastore.Value.
        import scala.collection.JavaConversions._

        val entities2 = entities.toBuffer
        e.set(name, entities2)
      }
      import PartialFunction._

      // turn a list of Xors into an Xor of a list
      def sequence(input: List[Xor[Throwable, E]]): Xor[Throwable, List[E]] = {
        input.foldRight[Xor[Throwable, List[E]]](Xor.Right(Nil)) { (o, ol) =>
          (o, ol) match {
            case (Xor.Right(x), Xor.Right(xs)) => Xor.Right(x :: xs)
            case (Xor.Left(y), _) => Xor.Left(y)
            case (_, Xor.Left(z)) => Xor.Left(z)
          }
        }
      }
    }

  } */

}
