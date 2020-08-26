package org.yankov.serialization.xml

import org.scalatest.{Ignore, Matchers, WordSpec}
import org.yankov.serialization.xml.XmlDataModel.Bytes

import scala.io.Source

class XmlSerializerTest extends WordSpec with Matchers {
  "serialize should succeed" in {
    val entity = TestData.createEntity(0)
    val result = XmlSerializer.serialize(entity)
    result shouldBe Source.fromResource("entity.xml").getLines.toList.mkString("\n")
  }

  "serialize with recursion should succeed" in {
    val entity = TestData.createEntity(5)
    val result = XmlSerializer.serialize(entity)
    result shouldBe Source.fromResource("entity-recursion.xml").getLines.toList.mkString("\n")
  }

  "serialize with deep recursion should not throw StackOverflowException" in {
    val entity = TestData.createEntity(200)
    XmlSerializer.serialize(entity)
  }
}
