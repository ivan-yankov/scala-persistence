package org.yankov.serialization.xml

import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class XmlDeserializerTest extends WordSpec with Matchers {
  //TODO test deserialization when json string field contains {}[],":
  //TODO test field not found exception path
//  "deserialize should succeed" in {
//    val result = XmlDeserializer.fromJson[Entity](Source.fromResource("entity.xml").getLines.toList.head)
//    result
//  }
}
