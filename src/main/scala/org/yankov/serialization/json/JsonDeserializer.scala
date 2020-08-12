package org.yankov.serialization.json

import org.yankov.datastructures.Tree
import org.yankov.reflection.{ClassDescription, ReflectionUtils}
import org.yankov.serialization.json.JsonCommons._
import org.yankov.serialization.json.JsonDataModel.JsonNodeString

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

  implicit def getChildren(parent: ClassDescription): List[ClassDescription] = {
    if (ReflectionUtils.Types.asList.contains(parent.className)) List()
    else ReflectionUtils.getFields(parent.className)
  }

  def fromJson(className: String, json: String): Unit = {
    val jsonFlatten = Tree(JsonNodeString("", json)).flat()
    val classFlatten = Tree(ClassDescription("", className)).flat()
  }
}
