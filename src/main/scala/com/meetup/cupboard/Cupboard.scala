package com.meetup.cupboard

import java.time.Instant

import com.google.cloud.datastore.{Datastore, Entity, ReadOption}
import com.meetup.cupboard.DatastoreFormats.{DatastoreFormat, InstantDatastoreProperty}

object Cupboard {
  def save[C](ds: Datastore, caseClass: C, kind: String)(implicit cf: DatastoreFormat[C]): Persisted[C] = {
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

  def save[C](ds: Datastore, caseClass: C)(implicit cf: DatastoreFormat[C]): Persisted[C] = {
    val kind = caseClass.getClass().getSimpleName
    save(ds, caseClass, kind)
  }

  def load[C](ds: Datastore, id: Long, kind: String)(implicit cf: DatastoreFormat[C]): Persisted[C] = {
    val key = ds.newKeyFactory()
      .kind(kind)
      .newKey(id)

    val entity: Entity = ds.get(key, Array.empty[ReadOption]: _*)
    val caseClass: C = cf.fromEntity(entity)

    val modified = InstantDatastoreProperty.getValueFromEntity("modified", entity)
    val created = InstantDatastoreProperty.getValueFromEntity("created", entity)
    Persisted(id, caseClass, modified, created)
  }
}
