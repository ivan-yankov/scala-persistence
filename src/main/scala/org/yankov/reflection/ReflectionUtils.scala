package org.yankov.reflection

import org.slf4j.LoggerFactory

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

case class ClassDescription(name: String, className: String)

object ReflectionUtils {
  private val log = LoggerFactory.getLogger(ReflectionUtils.getClass)
  private val runtimeUniverse = scala.reflect.runtime.universe

  object Types {
    val short: String = "scala.Short"
    val int: String = "scala.Int"
    val long: String = "scala.Long"
    val float: String = "scala.Float"
    val double: String = "scala.Double"
    val char: String = "scala.Char"
    val boolean: String = "scala.Boolean"
    val byte: String = "scala.Byte"
    val string: String = "java.lang.String"
    val seq: String = "scala.collection.immutable.Seq"
    val list: String = "scala.collection.immutable.List"
    val vector: String = "scala.collection.immutable.Vector"
    val set: String = "scala.collection.immutable.Set"
    val map: String = "scala.collection.immutable.Map"
    val option: String = "scala.Option"

    val asList: List[String] = List(
      short, int, long, float, double, char, boolean, byte, string, seq, list, vector, set, map, option
    )
  }

  private def defaultValue(className: String): Any = className match {
    case Types.short => 0.toShort
    case Types.int => 0.toInt
    case Types.long => 0.toLong
    case Types.float => 0.0.toFloat
    case Types.double => 0.0.toDouble
    case Types.char => ' '
    case Types.boolean => false
    case Types.byte => 0.toByte
    case Types.string => ""
    case Types.seq => Seq()
    case Types.list => List()
    case Types.vector => Vector()
    case Types.set => Set()
    case Types.map => Map()
    case Types.option => Option.empty
    case _ => log.error(s"Undefined default value for type [$className]")
  }

  def getFields(className: String): List[ClassDescription] = {
    implicit class StringExtensions(s: String) {
      def unifyTypeName: String = {
        if (!s.contains(".")) "scala." + s.substring(0, 1).toUpperCase() + s.substring(1, s.length)
        else s
      }
    }

    Class
      .forName(className)
      .getDeclaredFields
      .map(x => ClassDescription(x.getName, x.getType.getName.unifyTypeName))
      .toList
  }

  def createDefaultInstance(className: String): Any = {
    Class
      .forName(className)
      .getConstructors
      .head
      .newInstance(getFields(className).map(x => defaultValue(x.className)): _*)
  }

  def setField[T: ClassTag, V](instance: T, name: String, value: V)(implicit t: TypeTag[T]): T = {
    val runtimeMirror = runtimeUniverse.runtimeMirror(instance.getClass.getClassLoader)
    val instanceMirror = runtimeMirror.reflect(instance)
    val fieldTerm = runtimeUniverse.TermName(name)
    val fieldSymbol = runtimeUniverse.typeOf[T].decl(fieldTerm).asTerm.accessed.asTerm
    val fieldMirror = instanceMirror.reflectField(fieldSymbol)
    fieldMirror.set(value)
    instance
  }
}
