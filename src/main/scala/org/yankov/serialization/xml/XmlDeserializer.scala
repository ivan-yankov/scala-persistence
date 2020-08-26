package org.yankov.serialization.xml

import java.util.Base64

import org.yankov.datastructures.Tree
import org.yankov.datastructures.TreeModel._
import org.yankov.reflection.{Field, ReflectionUtils}
import org.yankov.serialization.xml.XmlCommons._
import org.yankov.serialization.xml.XmlDataModel._
import org.yankov.serialization.xml.XmlParser._

import scala.reflect.ClassTag

object XmlDeserializer {
  private val key = "key"
  private val value = "value"

  class FieldNotFoundException(message: String) extends Exception(message)

  class XmlParseException(message: String) extends Exception(message)

  case class XmlNode(name: String, attributes: Map[String, String], children: List[String])

  implicit def getChildren(node: String): List[String] = {
    val t = getAttributes(node).getOrElse(typeAttributeName, "")
    if (t.equals(Types.obj)) getNodeChildren(node)
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
      parent.xmlNodes,
      Option(ReflectionUtils.createInstance(parent.cls, defaultValues))
    )

    children.foreach(x => {
      val node = x.xmlNodes.find(y => x.name.equals(getTagName(y)))
      if (node.nonEmpty) {
        val parsedValue = parseNodeValue(node.get)
        if (parsedValue.nonEmpty) ReflectionUtils.setField(instance.value.get, x.name, parsedValue.get)
        else throw new XmlParseException(s"Error on parsing [${node.get}]")
      }
      else throw new FieldNotFoundException(s"Field [${x.name}] is not found in the serialized data")
    })

    instance
  }

  def deserialize[T: ClassTag](xml: String, cls: Class[T]): Either[XmlDeserializationError, T] = {
    try {
      val flatten = Tree(xml).flat()
      val result = Tree(Field("", cls))
        .flat()
        .map(x => Node[FieldValue](
          x.level,
          x.index,
          x.parentIndex,
          FieldValue(x.data.name, x.data.cls, flatten.filter(y => x.level == y.level).map(x => x.data)))
        )
        .build()
      Right(result.root.value.get.asInstanceOf[T])
    } catch {
      case e: FieldNotFoundException => Left(FieldNotFoundError(e.getMessage))
      case e: XmlParseException => Left(XmlParseError(e.getMessage))
    }
  }

  private def parseNodeValue[T](node: String): Option[Any] = {
    if (node.isEmpty) Option.empty
    else {
      val value = getNodeValue(node)
      val typeName = getAttributes(node).getOrElse(typeAttributeName, "")
      typeName match {
        case Types.short => value.toShortOption
        case Types.int => value.toIntOption
        case Types.long => value.toLongOption
        case Types.float => value.toFloatOption
        case Types.double => value.toDoubleOption
        case Types.char => if (value.isEmpty) Option.empty else Option(value.charAt(0))
        case Types.boolean =>
          if (value.equals("true")) Option(true)
          else if (value.equals("false")) Option(false)
          else Option.empty
        case Types.byte => Option(decodeBytes(value).value.head)
        case Types.bytes => Option(decodeBytes(value))
        case Types.string => Option(value)
        case Types.list => parseList(node)
        case Types.vector =>
          val result = parseList(node)
          if (result.isDefined) Option(result.get.toVector)
          else Option.empty
        case Types.set =>
          val result = parseList(node)
          if (result.isDefined) Option(result.get.toSet)
          else Option.empty
        case Types.map => parseMap(node)
        case Types.option => Option(parseNodeValue(value))
        case Types.obj =>
          val className = getAttributes(node).get(classNameAttributeName)
          if (className.isEmpty) Option.empty
          else {
            deserialize(node, Class.forName(className.get)) match {
              case Left(_) => Option.empty
              case Right(value) => Option(value)
            }
          }
        case _ => Option.empty
      }
    }
  }

  private def parseList(node: String): Option[List[_]] = {
    val result = getNodeChildren(node).map(x => parseNodeValue(x))
    if (result.forall(x => x.isDefined)) Option(result.map(x => x.get))
    else Option.empty
  }

  private def parseMap(node: String): Option[Map[_, _]] = {
    val result = getNodeChildren(node)
      .map(x => {
        val children = getNodeChildren(x)
        val parsedKey = parseNodeValue(children.find(y => getTagName(y).equals(key)).getOrElse(""))
        val parsedValue = parseNodeValue(children.find(y => getTagName(y).equals(value)).getOrElse(""))
        if (parsedKey.isEmpty || parsedValue.isEmpty) Option.empty
        else Option((parsedKey.get, parsedValue.get))
      })
    if (result.forall(x => x.isDefined)) Option(result.map(x => x.get).toMap)
    else Option.empty
  }

  private def decodeBytes(s: String): Bytes = Bytes(Base64.getDecoder.decode(s).toList)
}
