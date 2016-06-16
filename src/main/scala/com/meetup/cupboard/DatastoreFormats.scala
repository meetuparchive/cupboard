package com.meetup.cupboard

import java.time.{Instant, ZoneOffset, ZonedDateTime}

import cats.data.Xor
import com.google.cloud.datastore.Entity.Builder
import shapeless.labelled._
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}
import com.google.cloud.datastore.{Entity, FullEntity, Key, DateTime => GDateTime}
import com.meetup.cupboard.datastore.DatastoreProperties

object DatastoreFormats extends DatastoreProperties {

  /// The following section uses the Shapeless library to allow us to work with case classes in a generic way.
  /// You may need to refer to the Shapeless documentation to get a good sense of what this is doing.

  trait DatastoreFormat[A] {
    def fromEntity(e: FullEntity[_]): Xor[Throwable, A]
    def buildEntity(a: A, e: Entity.Builder): Entity.Builder
  }

  implicit object hNilFormat extends DatastoreFormat[HNil] {
    def fromEntity(j: FullEntity[_]) = Xor.Right(HNil)
    def buildEntity(h: HNil, e: Entity.Builder): Entity.Builder = e
  }

  implicit def hListFormat[FieldKey <: Symbol, Value, Remaining <: HList, DatastoreValue](
    implicit
    key: Witness.Aux[FieldKey],
    propertyConverter: DatastoreProperty[Value, DatastoreValue],
    tailFormat: Lazy[DatastoreFormat[Remaining]]
  ): DatastoreFormat[FieldType[FieldKey, Value] :: Remaining] =
    new DatastoreFormat[FieldType[FieldKey, Value] :: Remaining] {

      def buildEntity(hlist: FieldType[FieldKey, Value] :: Remaining, e: Entity.Builder): Entity.Builder = {

        val tailEntity = tailFormat.value.buildEntity(hlist.tail, e)
        val fieldName = key.value.name // the name was part of the tagged type
        propertyConverter.setEntityProperty(hlist.head, fieldName, tailEntity)
        e
      }

      def fromEntity(e: FullEntity[_]): Xor[Throwable, FieldType[FieldKey, Value] :: Remaining] = {
        val fieldName = key.value.name
        val v = propertyConverter.getValueFromEntity(fieldName, e)
        val tail = tailFormat.value.fromEntity(e)
        tail.flatMap { tail2 =>
          v.map(v2 =>
            field[FieldKey](v2) :: tail2
          )
        }
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

    def fromEntity(j: FullEntity[_]): Xor[Throwable, T] = {
      sg.fromEntity(j)
        .map(gen.from(_))
    }

    def buildEntity(t: T, e: Entity.Builder): Entity.Builder = sg.buildEntity(gen.to(t), e)
  }
}
