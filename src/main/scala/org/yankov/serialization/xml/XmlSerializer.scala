package org.yankov.serialization.xml

import java.util.Base64

import org.yankov.datastructures.Tree
import org.yankov.datastructures.TreeModel.Node
import org.yankov.serialization.xml.XmlCommons._
import org.yankov.serialization.xml.XmlDataModel.Bytes

import scala.annotation.tailrec

object XmlSerializer {
  case class ObjectNode(name: String, value: Any)

  case class XmlNode(name: String,
                     typeName: String,
                     className: Option[String],
                     level: Int,
                     value: String)

  case class MapElement[K, V](key: K, value: V)

  private val numberOfDecimalPlaces = 16
  private val indentation = "  "

  implicit def getChildren(node: ObjectNode): List[ObjectNode] = {
    node.value match {
      case _: Bytes => List()
      case _: Seq[_] => List()
      case _: Set[_] => List()
      case product: Product =>
        Range(0, product.productIterator.size)
          .toList
          .map(x => (product.productElementName(x), product.productElement(x)))
          .map(x => ObjectNode(x._1, x._2))
      case _ => List()
    }
  }

  implicit def aggregate(parent: XmlNode, children: List[XmlNode]): XmlNode = {
    val value = children.map(x => printXmlNode(x)).mkString("")
    XmlNode(parent.name, parent.typeName, parent.className, parent.level, value)
  }

  def serialize(product: Product): String = printXmlNode(serialize(product, 0)).trim

  private def serialize(product: Product, initialLevel: Int): XmlNode = {
    Tree(ObjectNode("object", product))
      .flat()
      .map(x =>
        Node(
          x.level + initialLevel,
          x.index,
          x.parentIndex,
          XmlNode(
            x.data.name,
            getType(x.data.value),
            getClassName(x.data.value),
            x.level + initialLevel,
            printValue(x.data.value, x.level + initialLevel)
          )
        )
      )
      .build()
      .root
  }

  private def levelIndentation(level: Int): String = {
    @tailrec
    def iterate(level: Int, acc: String): String = {
      if (level == 0) acc else iterate(level - 1, acc + indentation)
    }
    iterate(level, "")
  }

  private def printXmlNode(xmlNode: XmlNode): String = {
    val typeAttribute = " " + typeAttributeName + "=\"" + xmlNode.typeName + "\""
    val classNameAttribute = if (xmlNode.className.isDefined) " " + classNameAttributeName + "=\"" + xmlNode.className.get + "\"" else ""
    val beforeCloseTag = {
      if (baseTypes.contains(xmlNode.typeName)) ""
      else "\n" + levelIndentation(xmlNode.level)
    }

    "\n" + levelIndentation(xmlNode.level) + openTag + xmlNode.name + typeAttribute + classNameAttribute + closeTag +
      xmlNode.value +
      beforeCloseTag + openTag + closeTagNamePrefix + xmlNode.name + closeTag
  }

  private def getClassName(x: Any): Option[String] =
    if (getType(x).equals(Types.obj)) Option(x.getClass.getCanonicalName) else Option.empty

  private def getType(x: Any): String = x match {
    case _: Short => Types.short
    case _: Int => Types.int
    case _: Long => Types.long
    case _: Float => Types.float
    case _: Double => Types.double
    case _: Char => Types.char
    case _: Boolean => Types.boolean
    case _: Byte => Types.byte
    case _: Bytes => Types.bytes
    case _: String => Types.string
    case _: List[_] => Types.list
    case _: Vector[_] => Types.vector
    case _: Set[_] => Types.set
    case _: Map[_, _] => Types.map
    case _: Option[_] => Types.option
    case _ => Types.obj
  }

  private def printValue(x: Any, level: Int): String = x match {
    case value: Short => value.toString
    case value: Int => value.toString
    case value: Long => value.toString
    case value: Float => printDouble(value)
    case value: Double => printDouble(value)
    case value: Char => value.toString
    case value: Boolean => if (value) "true" else "false"
    case value: Byte => encodeBytes(Bytes(List(value.toByte)))
    case value: Bytes => encodeBytes(value)
    case value: String => value
    case value: List[_] => collectionToString(value, level)
    case value: Vector[_] => collectionToString(value, level)
    case value: Set[_] => collectionToString(value.toList, level)
    case value: Map[_, _] => mapToString(value, level)
    case _ => ""
  }

  private def collectionToString(items: Seq[_], level: Int): String = {
    if (items.isEmpty) ""
    else {
      items.map {
        case x: Product => XmlNode("element", getType(x), getClassName(x), level + 1, serialize(x, level + 1).value)
        case x: Any => XmlNode("element", getType(x), getClassName(x), level + 1, printValue(x, level))
      }
      .map(x => printXmlNode(x))
      .mkString("")
    }
  }

  private def mapToString(map: Map[_, _], level: Int): String = {
    val mapElements = map
      .toList
      .map(x => MapElement(x._1, x._2))
    collectionToString(mapElements, level)
  }

  private def printDouble(d: Double): String = String.format(s"%.${numberOfDecimalPlaces}f", d)

  private def encodeBytes(bytes: Bytes): String = Base64.getEncoder.withoutPadding.encodeToString(bytes.value.toArray)
}
