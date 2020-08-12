package org.yankov.serialization.json

import org.yankov.datastructures.Tree
import org.yankov.reflection.{Field, ReflectionUtils}
import org.yankov.serialization.json.JsonCommons._
import org.yankov.serialization.json.JsonDataModel.JsonNodeString

import scala.reflect.ClassTag

object JsonDeserializer {
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

  def fromJson[T: ClassTag](json: String)(implicit m: Manifest[T]): Unit = {
    val jsonFlatten = Tree(JsonNodeString("", json)).flat()
    val classFlatten = Tree(Field("", m.runtimeClass)).flat()
    classFlatten
  }
}
