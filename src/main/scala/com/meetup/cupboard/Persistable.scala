package com.meetup.cupboard

import com.meetup.cupboard.datastore.DatastoreProperty
import scala.language.postfixOps
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import com.meetup.cupboard.datastore.DatastoreProperties._
import com.meetup.cupboard.datastore.DatastoreProperty
import com.meetup.cupboard.DatastoreFormat

/**
 * Persistable captures information about and conversion instances for each field in a case class for persistence.
 *
 * Each field of a case class will have a Property instance which includes its field name and a typeclass instance
 * for conversion.
 *
 * This uses a macro behind the scenes to pull the name of each field to runtime (without reflection) and to create
 * fields on the Properties object that matches
 *
 * For example, given:
 *
 * ```
 * case class User(memberId: Int, username: String, createdAt: java.time.Instant)
 * object User extends Persistable[User]
 * ```
 *
 * Persistable provides the following:
 *
 * ```
 * scala> User.properties.all
 * res0: List[com.meetup.cupboard.Property[_]] = List(Property(memberId), Property(username), Property(createdAt))
 *
 * scala> User.properties.createdAt
 * res1: com.meetup.cupboard.Property[java.time.Instant] = Property(createdAt)
 *
 * scala> User.properties.createdAt.name
 * res2: String = createdAt
 * ```
 *
 * @tparam T (or parent class) to be persisted
 */
trait Persistable[T] {
  val properties: Properties[T]
  implicit def datastoreFormat[T]: DatastoreFormat[T] = macro Persistable.materializeDatastoreFormatImpl[T]
}

object Persistable {
  def createProperties[T]: Properties[T] = macro Persistable.materializePropertiesImpl[T]
  implicit def datastoreFormat[T]: DatastoreFormat[T] = macro Persistable.materializeDatastoreFormatImpl[T]

  def materializePropertiesImpl[T: c.WeakTypeTag](c: Context): c.Expr[Properties[T]] = {
    import c.universe._
    val typ: Type = weakTypeOf[T]
    val typeName = typ.toString

    // get the fields from the case class, in order
    val fields: List[Symbol] = typ.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    // create code to instantiate Property instances
    // and also create properties named the same as the case class
    val (properties, defProperties) = fields.map { field ⇒
      val name: TermName = field.name.toTermName
      val decoded = name.decodedName.toString
      val returnType: Type = typ.decl(name).typeSignature

      val property =
        q"""
            (new  _root_.com.meetup.cupboard.Property[$returnType, $typ]($decoded) {
                def getPropertyValueFromClass(c: $typ): $returnType = c.$name
            })"""

      val defProperty = q"val $name = $property"

      (property, defProperty)
    }.unzip

    c.Expr[Properties[T]](
      q"""new _root_.com.meetup.cupboard.Properties[$typ] (List(..$properties)) {
           ..$defProperties
       }
       """
    )
  }

  def materializeDatastoreFormatImpl[T: c.WeakTypeTag](c: Context): c.Expr[DatastoreFormat[T]] = {
    import c.universe._
    val typ: Type = weakTypeOf[T]
    val typeName = typ.toString

    //TODO: refactor this code into a "macro bundle" so we can have macro helper functions
    //      so the following code is not replicated in both macros, or (alternately)
    //      move to a @Persistable annotation macro.
    // get the fields from the case class, in order
    val fields: List[Symbol] = typ.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    // get the companion class for this case class, which we need for the constructor
    val companion = typ.typeSymbol.companion

    // create code to instantiate Property instances
    // and also create properties named the same as the case class
    val (buildEntities, fromEntities) = fields.map { field ⇒
      val name: TermName = field.name.toTermName
      val decoded = name.decodedName.toString
      val returnType: Type = typ.decl(name).typeSignature

      val buildEntity =
        q"""
           val property = $companion.properties.$name
           val propertyConverter = property.getPropertyConverter
           val value: $returnType = property.getPropertyValueFromClass(c)
           propertyConverter.setEntityProperty(value, property.name, entity)
          """

      val fromEntity = q"""
        $companion.properties.$name.getPropertyConverter.getValueFromEntity($decoded, entity) match {
          case _root_.cats.data.Xor.Right(v) => v
          case _root_.cats.data.Xor.Left(e) => throw new Exception(e)
        }
      """

      (buildEntity, fromEntity)
    }.unzip

    // When writing macros, we use fully qualified class names (prefixed with _root_).

    val getDF = q"""new _root_.com.meetup.cupboard.DatastoreFormat[$typ] {
        def fromEntity(entity: _root_.com.google.cloud.datastore.FullEntity[_]): _root_.cats.data.Xor[Throwable, $typ] = {
            _root_.cats.data.Xor.catchNonFatal {
              $companion( ..$fromEntities)
            }
        }
        def buildEntity(c: $typ, entity: _root_.com.google.cloud.datastore.Entity.Builder): com.google.cloud.datastore.Entity.Builder = {
          ..$buildEntities
          entity
        }
      }

    """
    c.Expr[DatastoreFormat[T]](getDF)
  }
}

abstract class Property[PropertyValue, C](val name: String)(implicit propertyConverter: DatastoreProperty[PropertyValue, _]) {
  def getPropertyConverter() = propertyConverter
  def getPropertyValueFromClass(c: C): PropertyValue
}

class Properties[T](all: List[Property[_, T]])
