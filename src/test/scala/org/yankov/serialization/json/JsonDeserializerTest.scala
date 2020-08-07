package org.yankov.serialization.json

import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class JsonDeserializerTest extends WordSpec with Matchers {
  "deserialize should succeed" in {
    val r = JsonDeserializer.fromJson(Source.fromResource("serialization-expected.json").getLines.toList.head)
    val a = 0
  }
}
