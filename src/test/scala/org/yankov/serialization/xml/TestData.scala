package org.yankov.serialization.xml

import org.yankov.serialization.xml.XmlDataModel.Bytes

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
                  bytes: Bytes,
                  string: String,
                  emptyList: List[Int],
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
                  map: Map[Int, String])

object TestData {
  def createEntity(numberOfChildren: Int): Entity = Entity(
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
    emptyList = List(),
    valuesList = List.tabulate(5)(x => x + 1),
    objectList = List(
      Dependency("object-list-id-1", "object-list-description-1"),
      Dependency("object-list-id-2", "object-list-description-2"),
      Dependency("object-list-id-3", "object-list-description-3")
    ),
    arrayList = List.tabulate(5)(x => List.tabulate(3)(y => Math.pow(x, y))),
    valuesVector = Vector.tabulate(5)(x => x + 1),
    objectVector = Vector(
      Dependency("object-vector-id-1", "object-vector-description-1"),
      Dependency("object-vector-id-2", "object-vector-description-2"),
      Dependency("object-vector-id-3", "object-vector-description-3")
    ),
    arrayVector = Vector.tabulate(5)(x => Vector.tabulate(3)(y => Math.pow(x, y))),
    pair = Pair(1, Dependency("pair-dependency-id", "pair-dependency-description")),
    option1 = Option("present"),
    option2 = Option.empty,
    child = if (numberOfChildren == 0) Option.empty else Option(createEntity(numberOfChildren - 1)),
    map = Map(1 -> "one", 2 -> "two", 3 -> "three")
  )
}