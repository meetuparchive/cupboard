package com.meetup.cupboard

import com.google.cloud.datastore.StructuredQuery
import com.google.cloud.datastore.StructuredQuery.PropertyFilter
import com.meetup.cupboard.datastore.DatastoreProperty

import scala.language.postfixOps
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import com.meetup.cupboard.datastore.DatastoreProperties._
import com.meetup.cupboard.datastore.{DatastoreProperty, FilterProperty}
import com.google.cloud.datastore.StructuredQuery.PropertyFilter

/**
 * Persistable is a trait that allows a case class to be persisted by Cupboard.
 *
 * To use Persistable, extend the companion class of the case class with Persistable[T]
 * (where [T] is the type of your case class) and define a properties method as follows:
 *
 * ```
 * case class User(memberId: Int, username: String, createdAt: java.time.Instant)
 * object User extends Persistable[User] {
 *   val properties = Persistable.createProperties[User]
 * }
 * ```
 *
 * There are two 'def macros' that are included in this PR.
 *
 * One is an implicit def which provides the implicit DatastoreFormat[T] which knows how to transform the case class
 * into the Entity objects we store and receive from Datastore.
 *
 * The second method macro implements Persistable.createProperties creates a Properties[T] object available at
 * properties which provides access to Property objects which contain the name of the property, its type, and the
 * DatastoreProperty object that knows how to do the transformations between the type and the appropriate Datastore
 * type.
 *
 * For example, given the definition of User above, User.properties includes:
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
 * This PR does not include the ability to persist one of a case class family (e.g. enum/AST style case classes that
 * extend a common sealed trait).
 *
 * In the future, a @Persistable annotation may be added to implement the boilerplate above, and a change to the
 * implementation ("vampire methods") will remove the "reflective calls" warning.
 *
 * @tparam T case class to be persisted
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
  def eq(p: PropertyValue)(implicit filterProperty: FilterProperty[PropertyValue, _]): Filter = Filter(
    filterProperty.getPropertyFilterEq(p, name)
  )

  def getPropertyConverter() = propertyConverter
  def getPropertyValueFromClass(c: C): PropertyValue
}

class Properties[T](all: List[Property[_, T]])
