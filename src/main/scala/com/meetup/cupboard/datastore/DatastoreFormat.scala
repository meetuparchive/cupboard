package com.meetup.cupboard

import cats.data.Xor
import com.google.cloud.datastore.{Entity, FullEntity}

trait DatastoreFormat[A] {
  def fromEntity(e: FullEntity[_]): Xor[Throwable, A]

  def buildEntity(a: A, e: Entity.Builder): Entity.Builder
}
