package org.yankov.serialization.json

import java.util.Base64

import org.yankov.datastructures.Tree
import org.yankov.serialization.json.JsonCommons._
import org.yankov.serialization.json.JsonDataModel._

import scala.annotation.tailrec

object JsonSerializer {
  private val numberOfDecimalPlaces = 16
  private val indentation = "  "

  implicit def getJsonNodeStringChildren(node: JsonNodeString): List[JsonNodeString] = List()

  implicit def aggregateJsonNodeStrings(parent: JsonNodeString, children: List[JsonNodeString]): JsonNodeString = {
    val value = children.map(x => printPair(wrapJsonString(x.name), x.value)).mkString(elementSeparator)
    JsonNodeString(parent.name, wrapJsonObject(value))
  }

  implicit def getJsonNodeChildren(node: JsonNode): List[JsonNode] = {
    node.value match {
      case _: Seq[_] => List()
      case _: List[_] => List()
      case _: Set[_] => List()
      case _: Array[_] => List()
      case product: Product =>
        Range(0, product.productIterator.size)
          .toList
          .map(x => (product.productElementName(x), product.productElement(x)))
          .map(x => JsonNode(x._1, x._2))
      case _ => List()
    }
  }

  implicit def aggregateJsonNodes(parent: JsonNode, children: List[JsonNode]): JsonNode = JsonNode("", List())

  def toJson(product: Product, format: Boolean = false): String = {
    val result = Tree(JsonNode("", product))
      .map(x => x.value match {
        case value: Seq[_] => JsonNodeString(x.name, toJsonString(value))
        case _: Option[Nothing] => JsonNodeString(x.name, wrapJsonObject(""))
        case _: Product => JsonNodeString(x.name, "product")
        case value => JsonNodeString(x.name, toJsonString(value))
      })
      .root
      .value

    if (format) formatJsonString(result)
    else result
  }

  private def printPair(key: String, value: String) = s"$key$keyValueSeparator$value"

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

  private def formatJsonString(json: String): String = {
    val lines = json
      .toList
      .map(x => x.toString)
      .map(x => x match {
        case JsonCommons.openObject => x + "\n"
        case JsonCommons.closeObject => "\n" + x
        case JsonCommons.openArray => x + "\n"
        case JsonCommons.closeArray => "\n" + x
        case JsonCommons.elementSeparator => x + "\n"
        case JsonCommons.keyValueSeparator => x + " "
        case _ => x
      })
      .mkString("")
      .split("\n")

    def indentLevel(s: String): Int = {
      val openBracesCount = s.count(x => x.toString.equals(openObject) || x.toString.equals(openArray))
      val closeBracesCount = s.count(x => x.toString.equals(closeObject) || x.toString.equals(closeArray))
      openBracesCount - closeBracesCount
    }

    def indent(index: Int, s: String): String = {
      if (s.isEmpty) s
      else {
        val level = indentLevel(lines.take(index).mkString(""))
        @tailrec
        def iterate(level: Int, acc: String): String = {
          if (level == 0) acc else iterate(level - 1, acc + indentation)
        }
        val lineIndentation = iterate(level, "")
        if (s.contains(closeObject) || s.contains(closeArray)) {
          lineIndentation.substring(0, lineIndentation.length - indentation.length) + s
        }
        else {
          lineIndentation + s
        }
      }
    }

    Range(0, lines.length)
      .toList
      .zip(lines)
      .map(x => (x._1, x._2))
      .map(x => indent(x._1, x._2))
      .mkString("\n")
  }

  private def printDouble(d: Double): String = String.format(s"%.${numberOfDecimalPlaces}f", d)

  private def encodeBytes(bytes: Bytes): String = Base64.getEncoder.withoutPadding.encodeToString(bytes)

  private def wrapJsonString(s: String): String = s"$stringWrapper$s$stringWrapper"

  private def wrapJsonArray(s: String) = s"$openArray$s$closeArray"

  private def wrapJsonObject(s: String) = s"$openObject$s$closeObject"
}
