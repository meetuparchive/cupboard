package com.meetup.cupboard

import java.time.Instant

import cats.data.Xor
import com.google.cloud.datastore.{Datastore, Entity, ReadOption}
import com.meetup.cupboard.DatastoreFormats.{DatastoreFormat, InstantDatastoreProperty}

object Cupboard {
  def save[C](ds: Datastore, caseClass: C, kind: String)(implicit cf: DatastoreFormat[C]): Xor[Exception, Persisted[C]] = {
    val keyFactory = ds.newKeyFactory().kind(kind)
    val key = ds.allocateId(keyFactory.newKey())
    val eBuilder = Entity.builder(key)
    val e = cf.buildEntity(caseClass, eBuilder)

    // set mtime, ctime
    val now = Instant.now
    InstantDatastoreProperty.setEntityProperty(now, "modified", e)
    InstantDatastoreProperty.setEntityProperty(now, "created", e)

    ds.put(e.build())

    //TODO: catch failure above
    Xor.Right(Persisted(Long.unbox(key.id()), caseClass, now, now))
  }

  def save[C](ds: Datastore, caseClass: C)(implicit cf: DatastoreFormat[C]): Xor[Exception, Persisted[C]] = {
    val kind = caseClass.getClass().getSimpleName
    save(ds, caseClass, kind)
  }

  //TODO: do we want a custom error type?
  def load[C](ds: Datastore, id: Long, kind: String)(implicit cf: DatastoreFormat[C]): Xor[Exception, Persisted[C]] = {
    val key = ds.newKeyFactory()
      .kind(kind)
      .newKey(id)

    val entityOpt: Option[Entity] = Option(ds.get(key, Array.empty[ReadOption]: _*))

    entityOpt.map { entity =>
      val caseClass: C = cf.fromEntity(entity)

      val modified = InstantDatastoreProperty.getValueFromEntity("modified", entity)
      val created = InstantDatastoreProperty.getValueFromEntity("created", entity)
      Xor.Right(Persisted(id, caseClass, modified, created))
    }.getOrElse(Xor.Left(new RuntimeException(s"Did not find entity with id $id")))
  }
}
