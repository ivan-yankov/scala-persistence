package org.yankov.serialization.json

import org.scalatest.{Matchers, WordSpec}
import org.yankov.serialization.json.JsonDataModel.Bytes
import org.yankov.serialization.json.Model._

import scala.io.Source

class JsonSerializerTest extends WordSpec with Matchers {
  private def createEntity(numberOfChildren: Int): Entity = Entity(
    short = 1,
    int = 30000,
    long = 5000000000L,
    float = 123.456789F,
    double = 123456789.123456789123456789,
    char = 'A',
    boolean1 = true,
    boolean2 = false,
    byte = 34.toByte,
    bytes = Bytes(List(1, 2, 3, 4, 5).map(x => x.toByte)),
    string = "serialization test",
    emptySeq = Seq(),
    valuesSeq = Seq.tabulate(5)(x => x + 1),
    objectSeq = Seq(Dependency("1", "d1"), Dependency("2", "d2"), Dependency("3", "d3")),
    arraySeq = Seq.tabulate(5)(x => Seq.tabulate(3)(y => Math.pow(x, y))),
    valuesList = List.tabulate(5)(x => x + 1),
    objectList = List(Dependency("1", "d1"), Dependency("2", "d2"), Dependency("3", "d3")),
    arrayList = List.tabulate(5)(x => List.tabulate(3)(y => Math.pow(x, y))),
    valuesVector = Vector.tabulate(5)(x => x + 1),
    objectVector = Vector(Dependency("1", "d1"), Dependency("2", "d2"), Dependency("3", "d3")),
    arrayVector = Vector.tabulate(5)(x => Vector.tabulate(3)(y => Math.pow(x, y))),
    pair = Pair(1, Dependency("id", "description")),
    option1 = Option("present"),
    option2 = Option.empty,
    child = if (numberOfChildren == 0) Option.empty else Option(createEntity(numberOfChildren - 1)),
    map = Map(1 -> "one", 2 -> "two", 3 -> "three")
  )

  "json serialization should succeed" in {
    val entity = createEntity(0)
    val result = JsonSerializer.toJson(entity)
    result shouldBe Source.fromResource("serialization-expected.json").getLines.toList.head
  }

  "json serialization with recursion should succeed" in {
    val entity = createEntity(5)
    val result = JsonSerializer.toJson(entity)
    result shouldBe Source.fromResource("serialization-recursion-expected.json").getLines.toList.head
  }

  "json serialization with deep recursion should not throw StackOverflowException" in {
    val entity = createEntity(1000)
    JsonSerializer.toJson(entity)
  }

  "json serialization should produce formatted json" in {
    val entity = createEntity(0)
    val result = JsonSerializer.toJson(entity, format = true)
    result shouldBe Source.fromResource("serialization-formatted-expected.json").getLines.toList.mkString("\n")
  }
}
