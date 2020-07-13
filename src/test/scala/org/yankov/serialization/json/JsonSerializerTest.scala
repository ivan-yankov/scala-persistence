package org.yankov.serialization.json

import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class JsonSerializerTest extends WordSpec with Matchers {

  case class Dependency(id: String, description: String)

  case class Pair(id: Int, dependency: Dependency)

  case class Entity(short: Short,
                    int: Int,
                    long: Long,
                    float: Float,
                    double: Double,
                    char: Char,
                    boolean1: Boolean,
                    boolean2: Boolean,
                    byte: Byte,
                    bytes: Array[Byte],
                    string: String,
                    emptySeq: Seq[Int],
                    valuesSeq: Seq[Int],
                    objectSeq: Seq[Dependency],
                    arraySeq: Seq[Seq[Double]],
                    valuesList: List[Int],
                    objectList: List[Dependency],
                    arrayList: List[List[Double]],
                    valuesVector: Vector[Int],
                    objectVector: Vector[Dependency],
                    arrayVector: Vector[Vector[Double]],
                    pair: Pair,
                    option1: Option[String],
                    option2: Option[String],
                    child: Option[Entity],
                    cannotSerialize: Map[Int, String])

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
    bytes = Array(1, 2, 3, 4, 5).map(x => x.toByte),
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
    cannotSerialize = Map(1 -> "1", 2 -> "2")
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
    val entity = createEntity(400)
    JsonSerializer.toJson(entity)
  }
}
