package org.yankov.reflection

import org.yankov.serialization.json.JsonDataModel.Bytes

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

case class Field(name: String, cls: Class[_])

object ReflectionUtils {
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
    val bytes: Class[Bytes] = classOf[Bytes]
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

  def defaultValue[T](cls: Class[T]): Option[T] = {
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
      case _ => ()
    }
    value match {
      case _: Unit => Option.empty
      case _ => Option(value.asInstanceOf[T])
    }
  }

  def getFields(cls: Class[_]): List[Field] = {
    cls
      .getDeclaredFields
      .map(x => Field(x.getName, x.getType))
      .toList
  }

  def createInstance[T](parameters: List[Any])(implicit m: Manifest[T]): T = {
    createInstance(m.runtimeClass, parameters).asInstanceOf[T]
  }

  def createInstance[T](cls: Class[T], parameters: List[Any]): T = {
    cls
      .getConstructors
      .head
      .newInstance(parameters: _*)
      .asInstanceOf[T]
  }

  def setField[T, V](instance: T, name: String, value: V): T = {
    val field = instance.getClass.getDeclaredField(name)
    field.setAccessible(true)
    field.set(instance, value)
    instance
  }

  def getType[T](x: T)(implicit tag: TypeTag[T]): List[Type] = tag.tpe match {
    case TypeRef(_, _, args) => args
  }
}
