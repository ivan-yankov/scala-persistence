package org.yankov.serialization.xml

import org.scalatest.{Matchers, WordSpec}
import org.yankov.serialization.xml.XmlDataModel.XmlNode

class XmlParserTest extends WordSpec with Matchers {
  private val objectString =
    """|<object type="Object" className="org.yankov.serialization.xml.Entity">
       |  <short type="Short">10</short>
       |  <int type="Int">20</int>
       |  <long type="Long">30</long>
       |</object>""".stripMargin

  private val objectNode = XmlNode(
    "object",
    Map("type" -> "Object", "className" -> "org.yankov.serialization.xml.Entity"),
    """
      |  <short type="Short">10</short>
      |  <int type="Int">20</int>
      |  <long type="Long">30</long>
      |""".stripMargin
  )

  private val emptyListString =
    """<emptyList type="List">
      |</emptyList>""".stripMargin

  private val emptyListNode = XmlNode("emptyList", Map("type" -> "List"), "")

  private val nonEmptyListString =
    """<valuesList type="List">
      |  <element type="Int">1</element>
      |  <element type="Int">2</element>
      |</valuesList>""".stripMargin

  private val nonEmptyListNode = XmlNode(
    "valuesList",
    Map("type" -> "List"),
    """
      |  <element type="Int">1</element>
      |  <element type="Int">2</element>
      |""".stripMargin
  )

  private val listOfListString =
    """<valuesList type="List">
      |  <element type="List">
      |    <element type="Int">1</element>
      |    <element type="Int">2</element>
      |  </element>
      |  <element type="List">
      |    <element type="Int">3</element>
      |    <element type="Int">4</element>
      |  </element>
      |</valuesList>""".stripMargin

  private val listOfListNode = XmlNode(
    "valuesList",
    Map("type" -> "List"),
    """
      |  <element type="List">
      |    <element type="Int">1</element>
      |    <element type="Int">2</element>
      |  </element>
      |  <element type="List">
      |    <element type="Int">3</element>
      |    <element type="Int">4</element>
      |  </element>
      |""".stripMargin
  )

  private val stringWithSpecialCharsString =
    """<value type="String">string with \n special characters <> </value>"""

  "parse should succeed" in {
    val result = XmlParser.parse(objectString)
    result.tag shouldBe "object"
    result.attributes shouldBe Map("type" -> "Object", "className" -> "org.yankov.serialization.xml.Entity")
    result.value shouldBe
      """<short type="Short">10</short>
        |  <int type="Int">20</int>
        |  <long type="Long">30</long>""".stripMargin
  }

  "parse node value for empty list should succeed" in {
    XmlParser.parse(emptyListString).value shouldBe ""
  }

  "parse node value for non-empty list should succeed" in {
    XmlParser.parse(nonEmptyListString).value shouldBe
      """<element type="Int">1</element>
        |  <element type="Int">2</element>""".stripMargin
  }

  "parse node value for string with special characters should succeed" in {
    XmlParser.parse(stringWithSpecialCharsString).value shouldBe """string with \n special characters <> """
  }

  "parse node value for list of lists should succeed" in {
    XmlParser.parse(listOfListString).value shouldBe
      """<element type="List">
        |    <element type="Int">1</element>
        |    <element type="Int">2</element>
        |  </element>
        |  <element type="List">
        |    <element type="Int">3</element>
        |    <element type="Int">4</element>
        |  </element>""".stripMargin
  }

  "get node children for object should succeed" in {
    XmlParser.getNodeChildren(objectNode) shouldBe List(
      XmlNode("short", Map("type" -> "Short"), "10"),
      XmlNode("int", Map("type" -> "Int"), "20"),
      XmlNode("long", Map("type" -> "Long"), "30")
    )
  }

  "get node children for empty list should succeed" in {
    XmlParser.getNodeChildren(emptyListNode) shouldBe List()
  }

  "get node children for non-empty list should succeed" in {
    XmlParser.getNodeChildren(nonEmptyListNode) shouldBe List(
      XmlNode("element", Map("type" -> "Int"), "1"),
      XmlNode("element", Map("type" -> "Int"), "2")
    )
  }

  "get node children for list of lists should succeed" in {
    val v1 = "<element type=\"Int\">1</element>\n    <element type=\"Int\">2</element>"
    val v2 = "<element type=\"Int\">3</element>\n    <element type=\"Int\">4</element>"
    XmlParser.getNodeChildren(listOfListNode) shouldBe List(
      XmlNode("element", Map("type" -> "List"), v1),
      XmlNode("element", Map("type" -> "List"), v2)
    )
  }
}
