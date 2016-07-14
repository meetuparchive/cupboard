package com.meetup.cupboard.datastore

import cats.data.Xor
import com.google.cloud.datastore.{Entity, FullEntity}

trait DatastoreProperty[V, D] {
  def getValueFromEntity(fieldName: String, e: FullEntity[_]): Xor[Throwable, V]

  def setEntityProperty(v: V, fieldName: String, e: Entity.Builder): Entity.Builder
}
