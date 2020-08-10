package org.yankov.serialization.json

import org.scalatest.{Matchers, WordSpec}
import org.yankov.serialization.json.Model._

import scala.io.Source

class JsonDeserializerTest extends WordSpec with Matchers {
  //TODO test deserialization when json string field contains {}[],":
  "deserialize should succeed" in {
    val r = JsonDeserializer.fromJson[Pair](Source.fromResource("serialization-expected.json").getLines.toList.head)
    val a = 0
  }
}
