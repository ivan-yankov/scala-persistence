package org.yankov.serialization.json

import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class JsonDeserializerTest extends WordSpec with Matchers {
  //TODO test deserialization when json string field contains {}[],":
  "deserialize should succeed" in {
    val r = JsonDeserializer.fromJson("org.yankov.serialization.json.Entity", Source.fromResource("serialization-expected.json").getLines.toList.head)
  }
}
