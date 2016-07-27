package com.meetup.cupboard.datastore

import cats.data.Xor
import com.google.cloud.datastore.StructuredQuery.PropertyFilter
import com.google.cloud.datastore.{Entity, FullEntity}

trait DatastoreProperty[V, D] {
  def getValueFromEntity(fieldName: String, e: FullEntity[_]): Xor[Throwable, V]

  def setEntityProperty(v: V, fieldName: String, e: Entity.Builder): Entity.Builder

}

@annotation.implicitNotFound(msg = "Properties of type ${V} can't be used to filter (or you haven't imported com.meetup.cupboard.datastore.DatastoreProperties._)")
trait FilterProperty[V, D] extends DatastoreProperty[V, D] {
  def getPropertyFilterEq(v: V, fieldName: String): PropertyFilter
}