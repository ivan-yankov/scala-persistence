package org.yankov.reflection

import org.slf4j.LoggerFactory

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

case class FieldDescription(name: String, typeName: String)

case class ClassDescription(typeName: String, fieldDescriptions: List[FieldDescription])

object ReflectionUtils {
  private val log = LoggerFactory.getLogger(ReflectionUtils.getClass)
  private val runtimeUniverse = scala.reflect.runtime.universe

  private def defaultValue(className: String, dependencies: List[(String, Any)]): Any = className match {
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
      val found = dependencies.find(x => x._1.equals(className))
      if (found.isDefined) found.get._2
      else log.error(s"Undefined default value for type [$className]")
  }

  private def getClassReflection[T](implicit t: TypeTag[T]): runtimeUniverse.ClassMirror = {
    val mirror = runtimeUniverse.runtimeMirror(getClass.getClassLoader)
    val classT = runtimeUniverse.typeOf[T].typeSymbol.asClass
    mirror.reflectClass(classT)
  }

  private def getConstructorMirror[T](implicit t: TypeTag[T]): runtimeUniverse.MethodMirror = {
    val constructor = runtimeUniverse.typeOf[T].decl(runtimeUniverse.termNames.CONSTRUCTOR).asMethod
    getClassReflection[T].reflectConstructor(constructor)
  }

  private def getFields[T](implicit t: TypeTag[T]): List[FieldDescription] = {
    getConstructorMirror[T]
      .symbol
      .typeSignature
      .paramLists
      .sortWith((x, y) => x.size <= y.size)
      .head
      .map(x => FieldDescription(x.name.toString, x.typeSignature.typeSymbol.name.toString))
  }

  def createDefaultInstance[T](dependencies: List[(String, Any)] = List())(implicit t: TypeTag[T]): T = {
    val defaultValues = getFields[T].map(x => defaultValue(x.typeName, dependencies))
    val constructorMirror = getConstructorMirror[T]
    constructorMirror(defaultValues: _*).asInstanceOf[T]
  }

  def describe[T](implicit t: TypeTag[T]): ClassDescription = {
    val classReflection = getClassReflection[T]
    ClassDescription(
      classReflection.symbol.typeSignature.typeSymbol.name.toString,
      getFields[T]
    )
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
