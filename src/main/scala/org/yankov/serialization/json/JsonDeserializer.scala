package org.yankov.serialization.json

import org.yankov.datastructures.Tree
import org.yankov.datastructures.TreeModel._
import org.yankov.reflection.{Field, ReflectionUtils}
import org.yankov.serialization.json.JsonCommons._
import org.yankov.serialization.json.JsonDataModel.JsonNodeString

import scala.reflect.ClassTag

object JsonDeserializer {
  case class FieldValue(name: String, cls: Class[_], jsonNodes: List[JsonNodeString], value: Option[Any] = Option.empty)

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
    val defaultValues = children.map(x => {
      if (x.value.nonEmpty) x.value
      else ReflectionUtils.defaultValue(x.cls)
    })
    .map(x => x.get)

    FieldValue(
      parent.name,
      parent.cls,
      parent.jsonNodes,
      Option(ReflectionUtils.createInstance(parent.cls, defaultValues))
    )
  }

  def fromJson[T: ClassTag](json: String)(implicit m: Manifest[T]): Unit = {
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
    result
  }
}
