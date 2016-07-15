package com.meetup.cupboard

//import io.circe.{ Decoder, Encoder }
//import io.circe.generic.decoding.DerivedDecoder
//import io.circe.generic.encoding.DerivedObjectEncoder
//import macrocompat.bundle
import com.meetup.cupboard.datastore.DatastoreProperty
import macrocompat.bundle

import scala.reflect.macros.whitebox
import shapeless.{CNil, Coproduct, HList, HNil, LabelledGeneric, Lazy}
import shapeless.labelled.KeyTag

import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.postfixOps
import scala.reflect.macros.whitebox
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

//TODO: generalize over DatastoreProperty
case class Property[A](name: String)(implicit propertyConverter: DatastoreProperty[A, _]) {
  def getPropertyConverter() = propertyConverter
}

// we could make our own hlist if we require type level computation
case class Properties[T](properties: List[Property[_]]) // internal representation should maybe be HList

object Properties {
  def apply[T](properties: Property[_]*) = new Properties[T](properties.toList)

  // set the stage for HList'ing later?
  def apply[T](t: (Property[_], Property[_])) = new Properties[T](List(t._1, t._2))

  def materializePropertiesImpl[T: c.WeakTypeTag](c: Context): c.Expr[Properties[T]] = {
    import c.universe._
    val typ = weakTypeOf[T]

    val fields = typ.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    val properties = fields.map { field ⇒
      val name: TermName = field.name.toTermName
      val decoded = name.decodedName.toString
      val returnType: Type = typ.decl(name).typeSignature

      q"_root_.com.meetup.cupboard.Property[$returnType]($decoded)"
    }

    c.Expr[Properties[T]](
      q"""_root_.com.meetup.cupboard.Properties[$typ] (..$properties)"""
    )
  }
}

trait Persistable[T] {
  //def properties: Properties[T] // could be a custom HList
  def mapper: Mappable[T] = macro Mappable.materializeMappableImpl[T]
  def properties: Properties[T] = macro Properties.materializePropertiesImpl[T]

  //  val mapper: Mappable[T] = implicitly[Mappable[T]]
  //  val properties = macro materializePropertiesImpl[T]
}

/*object Persistable
object Persistable {
  implicit def materializeProperties[T]: DerivedFormatter[T] = macro materializeMappableImpl[T]

  def materializeMappableImpl[T: c.WeakTypeTag](c: Context): c.Expr[DerivedFormatter[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    c.info(c.enclosingPosition, s"type is $tpe", true)
    c.Expr[DerivedFormatter[T]](
      q"""new DerivedFormatter[$tpe] { def debug(x: $tpe)  = "hi" } """)
  }

}*/
trait Mappable[T] {
  def toMap(t: T): Map[String, Any]
  def fromMap(map: Map[String, Any]): T
}

object Mappable {
  implicit def materializeMappable[T]: Mappable[T] = macro materializeMappableImpl[T]

  def materializeMappableImpl[T: c.WeakTypeTag](c: Context): c.Expr[Mappable[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val companion = tpe.typeSymbol.companion

    val fields = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramLists.head

    val (toMapParams, fromMapParams) = fields.map { field ⇒
      val name: TermName = field.name.toTermName
      val decoded = name.decodedName.toString
      val returnType: Type = tpe.decl(name).typeSignature

      (q"$decoded → t.$name", q"map($decoded).asInstanceOf[$returnType]")
    }.unzip

    c.Expr[Mappable[T]] {
      q"""
      new Mappable[$tpe] {
        def toMap(t: $tpe): Map[String, Any] = Map(..$toMapParams)
        def fromMap(map: Map[String, Any]): $tpe = $companion(..$fromMapParams)
      }
    """
    }
  }
}

object DerivedFormatter {

  implicit def materializeFormatter[T]: DerivedFormatter[T] = macro materializeMappableImpl[T]

  def materializeMappableImpl[T: c.WeakTypeTag](c: Context): c.Expr[DerivedFormatter[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    c.info(c.enclosingPosition, s"type is $tpe", true)
    c.Expr[DerivedFormatter[T]](
      q"""new DerivedFormatter[$tpe] { def debug(x: $tpe)  = "hi" } """)
  }
}

trait DerivedFormatter[A] {
  def debug(a: A): String
}

//trait DerivedFormatter[A] extends Formatter[A]

/*
  implicit def formatHList[R <: HList]: DerivedFormatter[R] = macro DerivationMacros.encodeHList[R]

  implicit def caseClassFormatter[A, R <: HList](a: A)(implicit
    gen: LabelledGeneric.Aux[A, R],
    encode: DerivedFormatter[R]): DerivedFormatter[A] = new DerivedFormatter[A] {
    final def debug(a: A): String = encode.debug(gen.to(a))
  }
*/
//def test[A, R <: HList](a: A)(implicit gen: LabelledGeneric.Aux[A, R]) = gen
//def test2[A, R <: HList](a: A)(implicit gen: LabelledGeneric.Aux[A, R], encode: DerivedFormatter[R]) = encode
//}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class identity extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro identityMacro.impl
}

object identityMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val (annottee, expandees) = inputs match {
      case (param: ValDef) :: (rest @ (_ :: _)) => (param, rest)
      case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
      case _ => (EmptyTree, inputs)
    }
    println((annottee, expandees))
    val outputs = expandees
    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}
