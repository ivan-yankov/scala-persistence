package org.yankov.serialization.json

import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class JsonDeserializerTest extends WordSpec with Matchers {
  //TODO test deserialization when json string field contains {}[],":
  "deserialize should succeed" in {
    val result = JsonDeserializer.fromJson[Entity](Source.fromResource("serialization-expected.json").getLines.toList.head)
  }
}
