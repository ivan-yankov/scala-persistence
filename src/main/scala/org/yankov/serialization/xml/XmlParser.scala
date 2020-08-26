package org.yankov.serialization.xml

import org.yankov.serialization.xml.XmlCommons.{Types, closeTag, closeTagNamePrefix, openTag, typeAttributeName}

import scala.annotation.tailrec

object XmlParser {
  def getTagName(node: String): String = {
    val tagInfo = getTagInfo(node)
    tagInfo.substring(0, tagInfo.indexOf(" "))
  }

  def getAttributes(node: String): Map[String, String] = {
    @tailrec
    def iterate(s: String, acc: Map[String, String]): Map[String, String] = {
      if (s.isEmpty) acc
      else {
        val splitIndex = s.indexOf("=")
        val split = splitAt(s, splitIndex)
        val name = split._1
        val firstQuotesIndex = split._2.indexOf("\"")
        val secondQuotesIndex = split._2.indexOf("\"", firstQuotesIndex + 1)
        val value = split._2.substring(firstQuotesIndex + 1, secondQuotesIndex)
        val newString = split._2.substring(secondQuotesIndex + 1).trim
        val newAcc = acc + (name -> value)
        iterate(newString, newAcc)
      }
    }

    val tagInfo = getTagInfo(node)
    val index = tagInfo.indexOf(" ")
    if (index < 0) Map()
    else {
      val attributes = tagInfo.substring(index + 1, tagInfo.length)
      iterate(attributes, Map())
    }
  }

  def getNodeChildren(node: String): List[String] = {
    @tailrec
    def iterate(s: String, acc: List[String]): List[String] = {
      if (s.isEmpty) acc
      else {
        val index = s.indexOf(closeTag, closeTagIndex(s, getTagName(s))) + 1
        val split = splitAt(s, index)
        iterate(split._2.trim, acc.appended(split._1))
      }
    }
    val nodeValue = getNodeValue(node)
    iterate(nodeValue, List())
  }

  def getNodeValue(node: String): String = {
    val startIndex = node.indexOf(closeTag) + 1
    val tagName = getTagName(node)
    val endIndex = closeTagIndex(node, tagName)
    val value = node.substring(startIndex, endIndex)
    val t = getAttributes(node).getOrElse(typeAttributeName, "")
    if (t.equals(Types.string)) value
    else removeIndentation(value)
  }

  private def splitAt(s: String, index: Int): (String, String) = {
    if (index >= (s.length - 1)) (s, "")
    else (s.substring(0, index), s.substring(index, s.length))
  }

  private def getTagInfo(node: String): String = node.substring(openTag.length, node.indexOf(closeTag))

  private def closeTagIndex(s: String, tagName: String): Int = {
    @tailrec
    def countSubstringMatch(s: String, sub: String, acc: Int): Int = {
      if (s.isEmpty) acc
      else {
        val newAcc = if (s.startsWith(sub)) acc + 1 else acc
        countSubstringMatch(s.substring(1), sub, newAcc)
      }
    }

    @tailrec
    def iterate(index: Int): Int = {
      val numberOfOpenTags = countSubstringMatch(s.substring(0, index), s"$openTag$tagName ", 0)
      val numberOfCloseTags = countSubstringMatch(s.substring(0, index), createCloseTag(tagName), 0)
      if (numberOfOpenTags - numberOfCloseTags == 1) index
      else {
        val newIndex = s.indexOf(createCloseTag(tagName), index + 1)
        iterate(newIndex)
      }
    }

    val initialIndex = s.indexOf(createCloseTag(tagName))
    iterate(initialIndex)
  }

  private def removeIndentation(s: String): String = {
    def notInTag(index: Int): Boolean = {
      val str = s.substring(0, index)
      val openTagCount = str.count(x => x.toString.equals(openTag))
      val closeTagCount = str.count(x => x.toString.equals(closeTag))
      openTagCount == closeTagCount
    }

    Range(0, s.length)
      .toList
      .zip(s)
      .map(x => {
        if (x._2.equals(' ')) if (notInTag(x._1)) '\n' else x._2
        else x._2
      })
      .mkString
      .replace("\n", "")
      .trim
  }

  private def createCloseTag(tagName: String): String = s"$openTag$closeTagNamePrefix$tagName$closeTag"
}
