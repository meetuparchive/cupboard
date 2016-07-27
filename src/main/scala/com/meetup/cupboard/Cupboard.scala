package com.meetup.cupboard

import java.time.Instant

import cats.data.Xor
import com.google.cloud.datastore.Entity.Builder
import com.google.cloud.datastore.{Datastore, Entity, Key, ReadOption}
import scala.reflect.runtime.universe.WeakTypeTag

import com.meetup.cupboard.datastore.DatastoreProperties.InstantDatastoreProperty
import scala.reflect.ClassTag

object Cupboard {

  /**
   * Save an entity to Datastore using the default kind.
   *
   * The default kind is based on the name of the case class type, as determined by compile time reflection.
   *
   * @param ds        A datastore instance
   * @param caseClass The entity to store
   * @tparam C The type of your case class
   */
  def save[C](ds: Datastore, caseClass: C)(implicit cf: DatastoreFormat[C], typeTag: WeakTypeTag[C]): Result[C] = {
    save(ds, caseClass, getName(typeTag))
  }

  /**
   * Save an entity with a custom kind.
   */
  def save[C](ds: Datastore, caseClass: C, kind: String)(implicit cf: DatastoreFormat[C]): Result[C] = {
    Xor.catchNonFatal {
      val key = getKey(ds, kind)
      createEntity(ds, caseClass, key, cf)
    }
  }

  /**
   * Save an entity with a custom key.
   */
  def saveWithKey[C](ds: Datastore, caseClass: C, key: Key)(implicit cf: DatastoreFormat[C]): Result[C] = {
    Xor.catchNonFatal {
      createEntity(ds, caseClass, key, cf)
    }
  }

  /**
   * Update an entity with a new value, using a custom kind.
   *
   * This will replace the old entity with what you're providing.
   */
  def update[C](ds: Datastore, caseClass: C, id: Long, kind: String)(implicit cf: DatastoreFormat[C], classtag: ClassTag[C]): Result[C] = {
    val key = getKeyWithId(ds, kind, id)
    updateEntity[C](ds, caseClass, key, kind)
  }

  /**
   * Update an entity with a new value.
   */
  def update[C](ds: Datastore, caseClass: C, id: Long)(implicit cf: DatastoreFormat[C], typeTag: WeakTypeTag[C], classtag: ClassTag[C]): Result[C] = {
    update(ds, caseClass, id, getName(typeTag))
  }

  def getKey(ds: Datastore, kind: String): Key = {
    val keyFactory = ds.newKeyFactory().kind(kind)
    ds.allocateId(keyFactory.newKey())
  }

  def getKeyWithId(ds: Datastore, kind: String, id: Long): Key = {
    val keyFactory = ds.newKeyFactory().kind(kind)
    keyFactory.newKey(id)
  }

  def load[C](ds: Datastore, id: Long)(implicit cf: DatastoreFormat[C], typeTag: WeakTypeTag[C]) = {
    loadKind(ds, id, getName(typeTag))
  }

  def loadKind[C](ds: Datastore, id: Long, kind: String)(implicit cf: DatastoreFormat[C]): Result[C] = {
    val key = ds.newKeyFactory()
      .kind(kind)
      .newKey(id)

    val entityXor: Xor[Throwable, Entity] = Option(ds.get(key, Array.empty[ReadOption]: _*))
      .map(Xor.Right(_)) // converting option to Xor
      .getOrElse(Xor.Left(new RuntimeException(s"No entity found with id $id")))

    entityXor.flatMap { entity =>
      cf.fromEntity(entity).flatMap { caseClass =>
        val modified = InstantDatastoreProperty.getValueFromEntity("modified", entity)
        val created = InstantDatastoreProperty.getValueFromEntity("created", entity)
        modified.flatMap { mtime =>
          created.map(ctime =>
            Persisted(id, caseClass, mtime, ctime)
          )
        }
      }
    }
  }

  private def createEntity[C](ds: Datastore, caseClass: C, key: Key, cf: DatastoreFormat[C]): Persisted[C] = {
    val eBuilder = Entity.builder(key)
    val e = cf.buildEntity(caseClass, eBuilder)
    val now = Instant.now
    InstantDatastoreProperty.setEntityProperty(now, "modified", e)
    InstantDatastoreProperty.setEntityProperty(now, "created", e)

    ds.put(e.build())

    Persisted(Long.unbox(key.id()), caseClass, now, now)
  }

  private def updateEntity[C](ds: Datastore, caseClass: C, key: Key, kind: String)(implicit cf: DatastoreFormat[C], classtag: ClassTag[C]): Xor[Throwable, Persisted[C]] = {
    val loadResponse = this.loadKind(ds, key.id(), kind)
    loadResponse.map { persisted =>
      val eBuilder = Entity.builder(key)
      val e = cf.buildEntity(caseClass, eBuilder)

      val now = Instant.now
      InstantDatastoreProperty.setEntityProperty(now, "modified", e)
      InstantDatastoreProperty.setEntityProperty(persisted.created, "created", e)

      ds.put(e.build())

      Persisted(Long.unbox(key.id()), caseClass, now, persisted.created)
    }
  }

  /**
   * Use compile-time reflection to get the name of a type as a string.
   *
   * @param typeTag WeakTypeTag (allows components with generic or abstract types)
   * @return name of type as a string
   */
  private def getName(typeTag: WeakTypeTag[_]): String = {
    typeTag.tpe.typeSymbol.name.toString
  }

}
