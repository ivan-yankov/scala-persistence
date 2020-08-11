package org.yankov.reflection

import org.slf4j.LoggerFactory

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

case class FieldDescription(name: String, typeName: String)

case class ClassDescription[T](defaultInstance: T, fieldDescriptions: List[FieldDescription])

object ReflectionUtils {
  private val log = LoggerFactory.getLogger(ReflectionUtils.getClass)

  private def defaultValue(className: String, defaultInstances: List[(String, Any)]): Any = className match {
    case "Short" => 0.toShort
    case "Int" => 0.toInt
    case "Long" => 0.toLong
    case "Float" => 0.0.toFloat
    case "Double" => 0.0.toDouble
    case "Char" => ' '
    case "Boolean" => false
    case "Byte" => 0.toByte
    case "String" => ""
    case "Seq" => Seq()
    case "List" => List()
    case "Vector" => Vector()
    case "Set" => Set()
    case "Map" => Map()
    case "Option" => Option.empty
    case _ =>
      val found = defaultInstances.find(x => x._1.equals(className))
      if (found.isDefined) found.get._2
      else log.error(s"Undefined default value for type [$className]")
  }

  def describe[T: ClassTag](defaultInstances: List[(String, Any)] = List())(implicit t: TypeTag[T]): ClassDescription[T] = {
    val runtimeUniverse = scala.reflect.runtime.universe
    val mirror = runtimeUniverse.runtimeMirror(getClass.getClassLoader)
    val classT = runtimeUniverse.typeOf[T].typeSymbol.asClass
    val classReflection = mirror.reflectClass(classT)
    val constructor = runtimeUniverse.typeOf[T].decl(runtimeUniverse.termNames.CONSTRUCTOR).asMethod
    val constructorMirror = classReflection.reflectConstructor(constructor)
    val fields = constructorMirror
      .symbol
      .typeSignature
      .paramLists
      .sortWith((x, y) => x.size <= y.size)
      .head
      .map(x => FieldDescription(x.name.toString, x.typeSignature.typeSymbol.name.toString))
    val defaultValues = fields.map(x => defaultValue(x.typeName, defaultInstances))
    val defaultInstance = constructorMirror(defaultValues: _*).asInstanceOf[T]
    ClassDescription(defaultInstance, fields)
  }

  def setField[T: ClassTag, V](instance: T, name: String, value: V)(implicit t: TypeTag[T]): T = {
    val runtimeUniverse = scala.reflect.runtime.universe
    val runtimeMirror = runtimeUniverse.runtimeMirror(instance.getClass.getClassLoader)
    val instanceMirror = runtimeMirror.reflect(instance)
    val fieldTerm = runtimeUniverse.TermName(name)
    val fieldSymbol = runtimeUniverse.typeOf[T].decl(fieldTerm).asTerm.accessed.asTerm
    val fieldMirror = instanceMirror.reflectField(fieldSymbol)
    fieldMirror.set(value)
    instance
  }
}
