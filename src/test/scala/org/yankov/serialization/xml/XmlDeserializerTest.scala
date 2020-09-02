package org.yankov.serialization.xml

import org.scalatest.{Matchers, WordSpec}
import org.yankov.serialization.xml.XmlDataModel.Bytes

import scala.io.Source

class XmlDeserializerTest extends WordSpec with Matchers {
  "deserialize should succeed" in {
    val result = XmlDeserializer.deserialize(
      Source.fromResource("entity.xml").getLines.toList.mkString,
      Class.forName("org.yankov.serialization.xml.Entity")
    )
    result.isRight shouldBe true
    result.getOrElse() shouldBe TestData.createEntity(0)
  }

  "deserialize with recursion should succeed" in {
    val result = XmlDeserializer.deserialize(
      Source.fromResource("entity-recursion.xml").getLines.toList.mkString,
      Class.forName("org.yankov.serialization.xml.Entity")
    )
    result.isRight shouldBe true
    result.getOrElse() shouldBe TestData.createEntity(5)
  }
}
