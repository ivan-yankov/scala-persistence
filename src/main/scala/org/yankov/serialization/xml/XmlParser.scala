package org.yankov.serialization.xml

import org.yankov.serialization.xml.XmlCommons._
import org.yankov.serialization.xml.XmlDataModel.{XmlNode, XmlParseException}

object XmlParser {
  trait ParseState

  case object InitialState extends ParseState

  case object EndState extends ParseState

  case object ReadTagName extends ParseState

  case object ReadCloseTag extends ParseState

  case object ReadAttributeKey extends ParseState

  case object ReadAttributeValue extends ParseState

  case object ReadTagValue extends ParseState

  case class ParseResult(node: XmlNode, acc: String, state: ParseState)

  def parse(xml: String): XmlNode = doParse(xml).head

  def getNodeChildren(node: XmlNode): List[XmlNode] = {
    val r = doParse(node.value.trim)
    r
  }

  private def doParse(xml: String): List[XmlNode] = {
    case class Result(parseResult: ParseResult, nodeAcc: List[XmlNode])
    val init = Result(ParseResult(XmlNode("", Map(), ""), "", InitialState), List())
    xml
      .trim
      .foldLeft(init)((result, char) => {
        val parseResult = process(result.parseResult, char)
        parseResult.state match {
          case EndState =>
            Result(ParseResult(XmlNode("", Map(), ""), "", InitialState), result.nodeAcc.appended(parseResult.node))
          case _ =>
            Result(parseResult, result.nodeAcc)
        }
      })
      .nodeAcc
  }

  private def process(parseResult: ParseResult, char: Char): ParseResult = {
    def parseAttribute(s: String): (String, String) = {
      val pair = s.replace("\"", "").trim.split(attributeKeyValueSeparator)
      (pair(0), pair(1))
    }

    def openTagExists(s: String, tag: String): Boolean = {
      val open = openTag + tag
      val close = openTag + closeTagNamePrefix + tag
      val openCount = s.toSeq.sliding(open.length).count(x => x.toString.equals(open))
      val closeCount = s.toSeq.sliding(close.length).count(x => x.toString.equals(close))
      openCount > closeCount
    }

    val node = parseResult.node
    val acc = parseResult.acc
    parseResult.state match {
      case InitialState =>
        if (char.toString.equals(openTag)) ParseResult(node, "", ReadTagName)
        else ParseResult(node, "", InitialState)
      case ReadTagName =>
        if (char.equals(' ')) ParseResult(XmlNode(acc, node.attributes, node.value), "", ReadAttributeKey)
        else ParseResult(node, acc + char.toString, ReadTagName)
      case ReadCloseTag =>
        if (char.toString.equals(closeTag)) {
          if (node.tag.equals(acc) && !openTagExists(node.value, acc)) {
            val value = if (isStringNode(node)) node.value else node.value.trim
            ParseResult(XmlNode(node.tag, node.attributes, value), "", EndState)
          }
          else ParseResult(node, node.value + openTag + closeTagNamePrefix + acc + closeTag, ReadTagValue)
        }
        else ParseResult(node, acc + char.toString, ReadCloseTag)
      case ReadAttributeKey =>
        if (char.toString.equals(attributeKeyValueSeparator)) ParseResult(node, acc + char.toString, ReadAttributeValue)
        else ParseResult(node, acc + char.toString, ReadAttributeKey)
      case ReadAttributeValue =>
        if (char.equals(' ') && acc.count(x => x.toString.equals(attributeValueWrapper)) == 2) {
          val att = parseAttribute(acc)
          ParseResult(XmlNode(node.tag, node.attributes + (att._1 -> att._2), node.value), "", ReadAttributeKey)
        }
        else if (char.toString.equals(closeTag)) {
          val att = parseAttribute(acc)
          ParseResult(XmlNode(node.tag, node.attributes + (att._1 -> att._2), node.value), "", ReadTagValue)
        }
        else ParseResult(node, acc + char.toString, ReadAttributeValue)
      case ReadTagValue =>
        if (char.toString.equals(closeTagNamePrefix) && acc.endsWith(openTag))
          ParseResult(XmlNode(node.tag, node.attributes, acc.take(acc.length - 1)), "", ReadCloseTag)
        else ParseResult(node, acc + char.toString, ReadTagValue)
    }
  }

  private def isStringNode(node: XmlNode): Boolean =
    node.attributes.getOrElse(typeAttributeName, "").equals(Types.string)
}
