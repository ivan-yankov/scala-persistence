package org.yankov.serialization.json

import org.yankov.datastructures.Tree
import org.yankov.datastructures.TreeModel.Node
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

  def fromJson[T](json: String): List[Node[JsonNodeString]] = {
    Tree(JsonNodeString("", json)).flat()
  }
}
