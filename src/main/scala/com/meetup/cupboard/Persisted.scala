package com.meetup.cupboard

import java.time.Instant

/**
 * This class captures the metadata common to any entity that has been stored
 * in the datastore, as well as the entity itself. All such data are either
 * created, modified, or both, when an entity is saved or retrieved.
 *
 * The motivation for such a class starts with the assertion that it is useful
 * to treat an entity that has been stored versus one that has not as having
 * different types. In fact this concept already exists in Chapstick's entity
 * system: for every entity there exists a _Proto_, which is a version of that
 * same entity that has not yet been stored.
 *
 * The advantage of a wrapper type over the method used by Chapstick is we
 * need only a single type, instead of a subtype per each entity type.
 *
 * This class is copied directly from subscription service.
 *
 * @author chrislewis
 *
 * I made all IDs Long for now.
 */
final case class Persisted[E](
  id: Long,
  entity: E,
  modified: Instant,
  created: Instant
)