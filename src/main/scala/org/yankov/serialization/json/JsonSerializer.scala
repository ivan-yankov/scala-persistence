package org.yankov.serialization.json

import java.util.Base64

import org.yankov.datastructures.{FlattenNode, FlattenTree, Node, Tree}
import org.yankov.serialization.json.JsonCommons._
import org.yankov.serialization.json.JsonDataModel._

object JsonSerializer {
  private val numberOfDecimalPlaces = 16

  implicit def getJsonNodeStringChildren(node: Node[JsonNodeString]): List[Node[JsonNodeString]] = List()

  implicit def aggregateJsonNodeStrings(parent: Node[JsonNodeString], children: List[Node[JsonNodeString]]): Node[JsonNodeString] = {
    val value = children.map(x => printPair(wrapJsonString(x.data.name), x.data.value)).mkString(elementSeparator)
    Node(JsonNodeString(parent.data.name, wrapJsonObject(value)))
  }

  implicit def aggregateJsonNodes(parent: Node[JsonNode], children: List[Node[JsonNode]]): Node[JsonNode] = Node(JsonNode("", List()))

  implicit def getJsonNodeChildren(node: Node[JsonNode]): List[Node[JsonNode]] = {
    node.data.value match {
      case _: Seq[_] => List()
      case _: List[_] => List()
      case _: Set[_] => List()
      case _: Array[_] => List()
      case product: Product =>
        Range(0, product.productIterator.size)
          .toList
          .map(x => (product.productElementName(x), product.productElement(x)))
          .map(x => Node(JsonNode(x._1, x._2)))
      case _ => List()
    }
  }

  def toJson(product: Product): String = {
    def toFlattenNodeString(x: FlattenNode[JsonNode], toString: Any => String): FlattenNode[JsonNodeString] =
      FlattenNode(x.level, x.index, x.parentIndex, Node(JsonNodeString(x.node.data.name, toString(x.node.data.value))))

    val jsonStrings = Tree(Node(JsonNode("", product)))
      .flat()
      .nodes
      .map(x => x.node.data.value match {
        case _: Seq[_] => toFlattenNodeString(x, toJsonString)
        case _: Option[Nothing] => toFlattenNodeString(x, _ => wrapJsonObject(""))
        case _: Product => toFlattenNodeString(x, _ => "product")
        case _ => toFlattenNodeString(x, toJsonString)
      })

    FlattenTree(jsonStrings)
      .build()
      .root
      .data
      .value
  }

  def printPair(key: String, value: String) = s"$key${keyValueSeparator}$value"

  private def toJsonString(x: Any): String = x match {
    case value: Short => value.toString
    case value: Int => value.toString
    case value: Long => value.toString
    case value: Float => printDouble(value)
    case value: Double => printDouble(value)
    case value: Char => wrapJsonString(value.toString)
    case value: Boolean => if (value) "true" else "false"
    case value: Byte => wrapJsonString(encodeBytes(Array(value)))
    case value: Bytes => wrapJsonString(encodeBytes(value))
    case value: String => wrapJsonString(value)
    case value: Seq[_] => collectionToString(value)
    case value: List[_] => collectionToString(value)
    case value: Vector[_] => collectionToString(value)
    case value: Set[_] => collectionToString(value.toList)
    case value: Array[_] => collectionToString(value.toList)
    case value: Map[_, _] => collectionToString(value.toList)
    case _ => `null`
  }

  private def collectionToString(items: Seq[_]): String = {
    val r = {
      if (items.isEmpty) ""
      else {
        items
          .map {
            case x: Product => toJson(x)
            case x: Any => toJsonString(x)
          }
          .mkString(elementSeparator)
      }
    }
    wrapJsonArray(r)
  }

  private def printDouble(d: Double): String = String.format(s"%.${numberOfDecimalPlaces}f", d)

  private def encodeBytes(bytes: Bytes): String = Base64.getEncoder.withoutPadding.encodeToString(bytes)

  private def wrapJsonString(s: String): String = s"$stringWrapper$s$stringWrapper"

  private def wrapJsonArray(s: String) = s"$openArray$s$closeArray"

  private def wrapJsonObject(s: String) = s"$openObject$s$closeObject"
}
