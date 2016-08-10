package com.meetup.cupboard

import cats.data.Xor

import scala.reflect.runtime.universe.WeakTypeTag
import com.google.cloud.datastore.StructuredQuery.{CompositeFilter, PropertyFilter, Filter => GFilter}
import com.google.cloud.datastore.{QueryResults, EntityQuery => GEntityQuery, Query => GQuery, _}
import com.meetup.cupboard.datastore.DatastoreProperties

import scala.util.control.NonFatal
import scala.util.{Either, Left, Right}
import scala.language.existentials

case class Filter(gfilter: GFilter)
case class SortOrder(property: Property[_, _], ascending: Boolean)

import scala.collection.JavaConverters._

case class EntityQuery[C](
    kindOpt: Option[String] = None,
    ancestorKey: Option[Key] = None,
    filters: Seq[Filter] = Seq(),
    sortOrders: Seq[SortOrder] = Seq(),
    eventualConsistency: Boolean = false)(implicit cf: DatastoreFormat[C], typeTag: WeakTypeTag[C]) {

  def from(kind: String): EntityQuery[C] = this.kind(kind)
  def kind(kind: String): EntityQuery[C] = this.copy(kindOpt = Some(kind))

  def where(criteria: Filter): EntityQuery[C] = filter(criteria)
  def filter(filter: Filter): EntityQuery[C] = this.copy(filters = filters :+ filter)

  def ancestorKey(key: Key): EntityQuery[C] = {
    val filter = PropertyFilter.hasAncestor(key)

    // add new filter to filters
    this.copy(filters = filters :+ Filter(filter))
  }

  //TODO: result as iterator, handle cursors

  def resultAsSeq(ds: Datastore): Xor[Throwable, Seq[Persisted[C]]] = {
    val result = runQuery(ds).asScala.map(entity => {
      val key: Long = entity.key().id()
      Cupboard.entityToCaseClass(key, entity, cf)
    })
    DatastoreProperties.sequence(result.toList)
  }

  private def runQuery(ds: Datastore): QueryResults[Entity] = {
    val readOptions: Seq[ReadOption] = if (eventualConsistency) {
      Seq(ReadOption.eventualConsistency())
    } else {
      Seq()
    }
    // scala is confused about the java vararg argument here
    // combined with method overloading, so we expand with _*
    ds.run(getQuery, readOptions: _*)
  }

  private def getQuery: GEntityQuery = {
    val kindForQuery = kindOpt match {
      case Some(kind) => kind
      case None => Cupboard.getName(typeTag)
    }
    val kindedQuery = GQuery
      .entityQueryBuilder()
      .kind(kindForQuery)

    val filteredQuery = combineFilters match {
      case None => kindedQuery
      case Some(filters) => kindedQuery.filter(filters)
    }

    filteredQuery.build()

    //TODO: ordering
  }

  private def combineFilters: Option[GFilter] = {
    if (filters.length == 0) {
      None
    } else {
      val gfilters = filters.map(_.gfilter)
      val head :: tail = gfilters
      Some(CompositeFilter.and(head, tail: _*))
    }
  }
}

