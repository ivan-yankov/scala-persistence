package org.yankov.reflection

import org.slf4j.LoggerFactory

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

case class Field(name: String, cls: Class[_])

object ReflectionUtils {
  private val log = LoggerFactory.getLogger(ReflectionUtils.getClass)
  private val runtimeUniverse = scala.reflect.runtime.universe

  implicit class StringExtensions(s: String) {
    def unifyTypeName: String = {
      if (!s.contains(".")) "scala." + s.substring(0, 1).toUpperCase() + s.substring(1, s.length)
      else s
    }
  }

  object Classes {
    val short: Class[Short] = classOf[Short]
    val int: Class[Int] = classOf[Int]
    val long: Class[Long] = classOf[Long]
    val float: Class[Float] = classOf[Float]
    val double: Class[Double] = classOf[Double]
    val char: Class[Char] = classOf[Char]
    val boolean: Class[Boolean] = classOf[Boolean]
    val byte: Class[Byte] = classOf[Byte]
    val string: Class[String] = classOf[String]
    val seq: Class[Seq[_]] = classOf[Seq[_]]
    val list: Class[List[_]] = classOf[List[_]]
    val vector: Class[Vector[_]] = classOf[Vector[_]]
    val set: Class[Set[_]] = classOf[Set[_]]
    val map: Class[Map[_, _]] = classOf[Map[_, _]]
    val option: Class[Option[_]] = classOf[Option[_]]

    val asList: List[Class[_]] = List(
      short, int, long, float, double, char, boolean, byte, string, seq, list, vector, set, map, option
    )
  }

  private def defaultValue[T](cls: Class[T]): T = {
    val value = cls match {
      case Classes.short => 0.toShort
      case Classes.int => 0.toInt
      case Classes.long => 0.toLong
      case Classes.float => 0.0.toFloat
      case Classes.double => 0.0.toDouble
      case Classes.char => ' '
      case Classes.boolean => false
      case Classes.byte => 0.toByte
      case Classes.string => ""
      case Classes.seq => Seq()
      case Classes.list => List()
      case Classes.vector => Vector()
      case Classes.set => Set()
      case Classes.map => Map()
      case Classes.option => Option.empty
      case _ => log.error(s"Undefined default value for type [${cls.getName}]")
    }
    value.asInstanceOf[T]
  }

  def getFields(cls: Class[_]): List[Field] = {
    cls
      .getDeclaredFields
      .map(x => Field(x.getName, x.getType))
      .toList
  }

  def createDefaultInstance[T](implicit m: Manifest[T]): T = {
    val cls = m.runtimeClass
    cls
      .getConstructors
      .head
      .newInstance(getFields(cls).map(x => defaultValue(x.cls)): _*)
      .asInstanceOf[T]
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
