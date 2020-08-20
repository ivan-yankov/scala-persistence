package org.yankov.serialization.json

import java.util.Base64

import org.yankov.datastructures.Tree
import org.yankov.datastructures.TreeModel._
import org.yankov.reflection.ReflectionUtils.Classes
import org.yankov.reflection.{Field, ReflectionUtils}
import org.yankov.serialization.json.JsonCommons._
import org.yankov.serialization.json.JsonDataModel.{Bytes, FieldNotFoundError, FieldValue, JsonDeserializationError, JsonNodeString, JsonParseError}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.Type

object JsonDeserializer {

  private class FieldNotFoundException(message: String) extends Exception(message)

  private class JsonParseException(message: String) extends Exception(message)

  implicit def getChildren(node: JsonNodeString): List[JsonNodeString] = {
    if (node.value.startsWith(openObject) && node.value.endsWith(closeObject)) {
      node.value
        .trim
        .unwrapJsonObject()
        .trim
        .replace("\n", "")
        .splitElements()
        .filter(x => x.nonEmpty)
        .map(x => x.splitPair())
        .map(x => JsonNodeString(x._1, x._2))
    }
    else List()
  }

  implicit def getChildren(parent: Field): List[Field] = {
    if (ReflectionUtils.Classes.asList.contains(parent.cls)) List()
    else ReflectionUtils.getFields(parent.cls)
  }

  implicit def aggregate(parent: FieldValue, children: List[FieldValue]): FieldValue = {
    val defaultValues = children
      .map(x => {
        if (x.value.nonEmpty) x.value
        else ReflectionUtils.defaultValue(x.cls)
      })
      .map(x => x.get)

    val instance = FieldValue(
      parent.name,
      parent.cls,
      parent.jsonNodes,
      Option(ReflectionUtils.createInstance(parent.cls, defaultValues))
    )

    children.foreach(x => {
      val value = x.jsonNodes.find(y => x.name.equals(y.name))
      if (value.nonEmpty) {
        val parsedValue = fromJsonString(value.get.value, x.cls)
        if (parsedValue.nonEmpty) ReflectionUtils.setField(instance.value.get, x.name, parsedValue.get)
        else throw new JsonParseException(s"Cannot parse value [${value.get.value}] to [${x.cls.getName}]")
      }
      else throw new FieldNotFoundException(s"Field [${x.name}] is not found in json object")
    })

    instance
  }

  def fromJson[T: ClassTag](json: String)(implicit m: Manifest[T]): Either[JsonDeserializationError, Unit] = {
    try {
      val jsonFlatten = Tree(JsonNodeString("", json)).flat()
      val result = Tree(Field("", m.runtimeClass))
        .flat()
        .map(x => Node[FieldValue](
          x.level,
          x.index,
          x.parentIndex,
          FieldValue(x.data.name, x.data.cls, jsonFlatten.filter(y => x.level == y.level).map(x => x.data)))
        )
        .build()
      Right(result)
    } catch {
      case e: FieldNotFoundException => Left(FieldNotFoundError(e.getMessage))
      case e: JsonParseException => Left(JsonParseError(e.getMessage))
    }
  }

  private def fromJsonString[T](x: String, cls: Class[T]): Option[Any] = {
    cls match {
      case Classes.short => x.toShortOption
      case Classes.int => x.toIntOption
      case Classes.long => x.toLongOption
      case Classes.float => x.toFloatOption
      case Classes.double => x.toDoubleOption
      case Classes.char => if (x.nonEmpty) Option(x.charAt(0)) else Option.empty
      case Classes.boolean =>
        if (x.equals("true")) Option(true)
        else if (x.equals("false")) Option(false)
        else Option.empty
      case Classes.byte => x.toByteOption
      case Classes.bytes => Option(decodeBytes(x))
      case Classes.string => Option(x)
      case Classes.seq => Option(stringToList(x, ReflectionUtils.getType(x)))
      case Classes.list => Option(stringToList(x, ReflectionUtils.getType(x)))
      case Classes.vector => Option(stringToList(x, ReflectionUtils.getType(x)).toVector)
      case Classes.set => Option(stringToList(x, ReflectionUtils.getType(x)).toSet)
      //      case Classes.map => Option(stringToList(x, ReflectionUtils.getType(x)).toMap)
      case _ => Option.empty
    }
  }

  private def stringToList(s: String, t: List[Type]): List[_] = {
    List()
  }

  private def decodeBytes(s: String): Bytes = Bytes(Base64.getDecoder.decode(s).toList)
}
