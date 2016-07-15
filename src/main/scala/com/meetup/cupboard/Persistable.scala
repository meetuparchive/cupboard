package com.meetup.cupboard

import com.meetup.cupboard.datastore.DatastoreProperty
import scala.language.postfixOps
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Persistable captures information about and conversion instances for each field in a case class for persistance.
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
 * @tparam Class (or parent class) to be persisted
 */
trait Persistable[T] {
  def properties: Properties[T] = macro Persistable.materializePropertiesImpl[T]
}

object Persistable {
  def materializePropertiesImpl[T: c.WeakTypeTag](c: Context): c.Expr[Properties[T]] = {
    import c.universe._
    val typ: Type = weakTypeOf[T]
    val typeName = typ.toString

    // get the fields from the case class
    val fields: List[Symbol] = typ.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    // create code to instantiate Property instances
    // and also create properties named the same as the case class
    val (properties, defProperties) = fields.map { field ⇒
      val name: TermName = field.name.toTermName
      val decoded = name.decodedName.toString
      val returnType: Type = typ.decl(name).typeSignature

      val property = q"_root_.com.meetup.cupboard.Property[$returnType]($decoded)"
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
}

case class Property[A](name: String)(implicit propertyConverter: DatastoreProperty[A, _]) {
  def getPropertyConverter() = propertyConverter
}

case class Properties[T](all: List[Property[_]])
