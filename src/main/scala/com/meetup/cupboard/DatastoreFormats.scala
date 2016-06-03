package com.meetup.cupboard

import java.time.{Instant, ZoneOffset, ZonedDateTime}

import cats.data.Xor
import com.google.cloud.datastore.Entity.Builder
import shapeless.labelled._
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}
import com.google.cloud.datastore.{Entity, FullEntity, Key, DateTime => GDateTime}

object DatastoreFormats {

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

  /// The following section uses the Shapeless library to allow us to work with case classes in a generic way.
  /// You may need to refer to the Shapeless documentation to get a good sense of what this is doing.

  trait DatastoreFormat[A] {
    def fromEntity(e: FullEntity[Key]): Xor[Throwable, A]
    def buildEntity(a: A, e: Entity.Builder): Entity.Builder
  }

  implicit object hNilFormat extends DatastoreFormat[HNil] {
    def fromEntity(j: FullEntity[Key]) = Xor.Right(HNil)
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

    def fromEntity(e: FullEntity[Key]): Xor[Throwable, ::[FieldType[FieldKey, Value], Remaining]] = {
      val fieldName = key.value.name
      val v = propertyConverter.getValueFromEntity(fieldName, e)
      val tail = tailFormat.fromEntity(e)
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

    def fromEntity(j: FullEntity[Key]): Xor[Throwable, T] = {
      sg.fromEntity(j)
        .map(gen.from(_))
    }

    def buildEntity(t: T, e: Entity.Builder): Entity.Builder = sg.buildEntity(gen.to(t), e)
  }

  implicit object CNilDatastoreFormat extends DatastoreFormat[CNil] {
    override def fromEntity(e: FullEntity[Key]): Xor[Throwable, CNil] = ???
    override def buildEntity(a: CNil, e: Builder): Builder = ???
  }

  implicit def coproductDatastoreFormat[Name <: Symbol, Head, Tail <: Coproduct](implicit
    key: Witness.Aux[Name],
    lazyHeadFormat: Lazy[DatastoreFormat[Head]],
    lazyTailFormat: Lazy[DatastoreFormat[Tail]]): DatastoreFormat[FieldType[Name, Head] :+: Tail] = new DatastoreFormat[FieldType[Name, Head] :+: Tail] {
    override def fromEntity(e: FullEntity[Key]): Xor[Throwable, :+:[FieldType[Name, Head], Tail]] = {
      if (e.getString("type") == key.value.name) {
        lazyHeadFormat.value.fromEntity(e).map(headResult =>
          Inl(field[Name](headResult))
        )
      } else {
        lazyTailFormat.value.fromEntity(e).map(tailResult =>
          Inr(tailResult)
        )
      }
    }

    override def buildEntity(a: :+:[FieldType[Name, Head], Tail], e: Builder): Builder = {
      a match {
        case Inl(head) =>
          val newE: Builder = lazyHeadFormat.value.buildEntity(head, e)
          newE.set("type", key.value.name)
          e
        case Inr(tail) =>
          lazyTailFormat.value.buildEntity(tail, e)
      }

    }
  }

  /*

  +  implicit def coproductBigDataFormat[Name <: Symbol, Head, Tail <: Coproduct](
                                                                                   +    implicit
  +    key: Witness.Aux[Name],
  +    lazyHeadFormat: Lazy[BigDataFormat[Head]],
  +    lazyTailFormat: Lazy[BigDataFormat[Tail]]
  +  ): BigDataFormat[FieldType[Name, Head] :+: Tail] = new BigDataFormat[FieldType[Name, Head] :+: Tail] {
    +    def label: String = key.value.name
    +    def toProperties(t: FieldType[Name, Head] :+: Tail): StringyMap = t match {
    +      case Inl(head) =>
    +        val map = lazyHeadFormat.value.toProperties(head)
    +        map.put("_typeHint", key.value.name)
    +        map
    +      case Inr(tail) =>
        +        lazyTailFormat.value.toProperties(tail)
    +    }
    +    def fromProperties(m: StringyMap): BigResult[FieldType[Name, Head] :+: Tail] = {
      +      if (m.get("_typeHint").asInstanceOf[String] == label) {
        +        lazyHeadFormat.value.fromProperties(m).right map { headResult =>
          +          Inl(field[Name](headResult))
          +        }
        +      } else {
        +        lazyTailFormat.value.fromProperties(m).right map { tailResult =>
          +          Inr(tailResult)
          +        }
        +      }
      +    }
    +  }
*/
}
