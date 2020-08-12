package org.yankov.serialization.json

import org.yankov.datastructures.Tree
import org.yankov.reflection.{Field, ReflectionUtils}
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

//  implicit def getChildren[_](parent: Field[_]): List[Field[_]] = {
//    if (ReflectionUtils.Types.asList.contains(parent.cls)) List()
//    else ReflectionUtils.getFields(parent.cls)
//  }

  def fromJson[T](json: String): Unit = {
    val jsonFlatten = Tree(JsonNodeString("", json)).flat()
//    val classFlatten = Tree(Field("", classOf[T])).flat()
  }
}
