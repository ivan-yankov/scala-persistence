package org.yankov.serialization.json

import org.slf4j.LoggerFactory

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

object InstanceFactory {
  private val log = LoggerFactory.getLogger(InstanceFactory.getClass)

  private object ClassNames {
    val short: String = classOf[Short].getName
    val int: String = classOf[Int].getName
    val long: String = classOf[Long].getName
    val float: String = classOf[Float].getName
    val double: String = classOf[Double].getName
    val char: String = classOf[Char].getName
    val boolean: String = classOf[Boolean].getName
    val byte: String = classOf[Byte].getName
    val string: String = classOf[String].getSimpleName.toLowerCase
    val seq: String = classOf[Seq[_]].getSimpleName.toLowerCase
    val list: String = classOf[List[_]].getSimpleName.toLowerCase
    val vector: String = classOf[Vector[_]].getSimpleName.toLowerCase
    val set: String = classOf[Set[_]].getSimpleName.toLowerCase
    val map: String = classOf[Map[_, _]].getSimpleName.toLowerCase
    val option: String = classOf[Option[_]].getSimpleName.toLowerCase
  }

  private def defaultValue(className: String): Any = className match {
    case ClassNames.short => 0.toShort
    case ClassNames.int => 0.toInt
    case ClassNames.long => 0.toLong
    case ClassNames.float => 0.0.toFloat
    case ClassNames.double => 0.0.toDouble
    case ClassNames.char => ' '
    case ClassNames.boolean => false
    case ClassNames.byte => 0.toByte
    case ClassNames.string => ""
    case ClassNames.seq => Seq()
    case ClassNames.list => List()
    case ClassNames.vector => Vector()
    case ClassNames.set => Set()
    case ClassNames.map => Map()
    case ClassNames.option => Option.empty
    case _ => log.error(s"Undefined default value for type [$className]")
  }

  def createDefaultInstance[T: ClassTag](implicit t: TypeTag[T]): T = {
    val ru = scala.reflect.runtime.universe
    val mirror = ru.runtimeMirror(getClass.getClassLoader)
    val classT = ru.typeOf[T].typeSymbol.asClass
    val classReflection = mirror.reflectClass(classT)
    val constructor = ru.typeOf[T].decl(ru.termNames.CONSTRUCTOR).asMethod
    val constructorMirror = classReflection.reflectConstructor(constructor)
    val parameterTypes = constructorMirror
      .symbol
      .typeSignature
      .paramLists
      .sortWith((x, y) => x.size <= y.size)
      .head
      .map(x => x.typeSignature.typeSymbol)
    val defaultValues = parameterTypes.map(x => defaultValue(x.name.toString.toLowerCase))
    constructorMirror(defaultValues: _*).asInstanceOf[T]
  }

  def setField[T: ClassTag, V](instance: T, name: String, value: V)(implicit t: TypeTag[T]): T = {
    val ru = scala.reflect.runtime.universe
    val runtimeMirror = ru.runtimeMirror(instance.getClass.getClassLoader)
    val instanceMirror = runtimeMirror.reflect(instance)
    val fieldTerm = ru.TermName(name)
    val fieldSymbol = ru.typeOf[T].decl(fieldTerm).asTerm.accessed.asTerm
    val fieldMirror = instanceMirror.reflectField(fieldSymbol)
    fieldMirror.set(value)
    instance
  }
}
