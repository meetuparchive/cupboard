package com.meetup.cupboard

import java.time.Instant

import cats.data.Xor
import com.google.cloud.datastore.{Datastore, Entity, ReadOption}
import com.meetup.cupboard.DatastoreFormats.{DatastoreFormat, InstantDatastoreProperty}
import shapeless.Typeable

object Cupboard {
  def save[C](ds: Datastore, caseClass: C, kind: String)(implicit cf: DatastoreFormat[C]): Result[C] = {
    Xor.catchNonFatal {
      val keyFactory = ds.newKeyFactory().kind(kind)
      val key = ds.allocateId(keyFactory.newKey())
      val eBuilder = Entity.builder(key)
      val e = cf.buildEntity(caseClass, eBuilder)

      // set mtime, ctime
      val now = Instant.now
      InstantDatastoreProperty.setEntityProperty(now, "modified", e)
      InstantDatastoreProperty.setEntityProperty(now, "created", e)

      ds.put(e.build())

      Persisted(Long.unbox(key.id()), caseClass, now, now)
    }
  }

  def save[C](ds: Datastore, caseClass: C)(implicit cf: DatastoreFormat[C]): Result[C] = {
    val kind = caseClass.getClass().getSimpleName
    save(ds, caseClass, kind)
  }

  def load[C](ds: Datastore, id: Long)(implicit cf: DatastoreFormat[C], typeable: Typeable[C]) = {
    loadKind(ds, id, typeable.describe)
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
}
