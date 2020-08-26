package org.yankov.serialization.xml

import org.scalatest.{Matchers, WordSpec}

class XmlParserTest extends WordSpec with Matchers {
  private val objectNode =
    """|<object type="Object" className="org.yankov.serialization.xml.Entity">
      |  <short type="Short">10</short>
      |  <int type="Int">20</int>
      |  <long type="Long">30</long>
      |</object>
      |""".stripMargin

  private val emptyListNode =
    """<emptyList type="List">
      |</emptyList>
      |""".stripMargin

  private val nonEmptyListNode =
    """<valuesList type="List">
      |  <element type="Int">1</element>
      |  <element type="Int">2</element>
      |</valuesList>
      |""".stripMargin

  private val listOfListNode =
    """<valuesList type="List">
      |  <element type="List">
      |    <element type="Int">1</element>
      |    <element type="Int">2</element>
      |  </element>
      |  <element type="List">
      |    <element type="Int">3</element>
      |    <element type="Int">4</element>
      |  </element>
      |</valuesList>
      |""".stripMargin

  private val stringWithSpecialCharsNode =
    """<value type="String">string with \n special characters <> </value>"""

  "get tag name should succeed" in {
    XmlParser.getTagName(objectNode) shouldBe "object"
  }

  "get attributes should succeed" in {
    XmlParser.getAttributes(objectNode) shouldBe Map("type" -> "Object", "className" -> "org.yankov.serialization.xml.Entity")
  }

  "get node value for object should succeed" in {
    XmlParser.getNodeValue(objectNode) shouldBe """<short type="Short">10</short>
                                                  |  <int type="Int">20</int>
                                                  |  <long type="Long">30</long>""".stripMargin
  }

  "get node value for empty list should succeed" in {
    XmlParser.getNodeValue(emptyListNode) shouldBe ""
  }

  "get node value for non-empty list should succeed" in {
    XmlParser.getNodeValue(nonEmptyListNode) shouldBe """<element type="Int">1</element>
                                                        |  <element type="Int">2</element>""".stripMargin
  }

  "get node value for string with special characters should succeed" in {
    XmlParser.getNodeValue(stringWithSpecialCharsNode) shouldBe """string with \n special characters <> """
  }

  "get node value for list of lists should succeed" in {
    XmlParser.getNodeValue(listOfListNode) shouldBe """<element type="List">
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
      """<short type="Short">10</short>""",
      """<int type="Int">20</int>""",
      """<long type="Long">30</long>"""
    )
  }

  "get node children for empty list should succeed" in {
    XmlParser.getNodeChildren(emptyListNode) shouldBe List()
  }

  "get node children for non-empty list should succeed" in {
    XmlParser.getNodeChildren(nonEmptyListNode) shouldBe List(
      """<element type="Int">1</element>""",
      """<element type="Int">2</element>"""
    )
  }

  "get node children for list of lists should succeed" in {
    XmlParser.getNodeChildren(listOfListNode) shouldBe List(
      """<element type="List">
        |    <element type="Int">1</element>
        |    <element type="Int">2</element>
        |  </element>""".stripMargin,
        """<element type="List">
        |    <element type="Int">3</element>
        |    <element type="Int">4</element>
        |  </element>""".stripMargin
    )
  }
}
