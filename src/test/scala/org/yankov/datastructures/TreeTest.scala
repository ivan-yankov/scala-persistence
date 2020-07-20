package org.yankov.datastructures

import org.scalatest.{Matchers, WordSpec}

class TreeTest extends WordSpec with Matchers {
  private def createTree: Tree[String] = {
    val c = List.tabulate(8)(x => Node(Option(s"C$x"), List()))
    val bChildren = List(
      List(c.head, c(1), c(2)),
      List(c(3), c(4), c(5)),
      List(c(6), c(7))
    )
    val b = List.tabulate(3)(x => Node(Option(s"B$x"), bChildren(x)))
    val aChildren = List(
      List(b.head, b(1)),
      List(b(2)),
      List()
    )
    val a = List.tabulate(3)(x => Node(Option(s"A$x"), aChildren(x)))
    Tree(Node(Option("ROOT"), a))
  }

  "traverse should succeed" in {
    createTree.traverse(x => x.data.get).map(x => x._2) shouldBe List("A2", "C7", "C6", "B2", "A1", "C5", "C4", "C3", "B1", "C2", "C1", "C0", "B0", "A0", "ROOT")
  }

  "printToString should succeed" in {
    def indent(nodeData: String): String = {
      if (nodeData.startsWith("A")) s"  $nodeData"
      else if (nodeData.startsWith("B")) s"    $nodeData"
      else if (nodeData.startsWith("C")) s"      $nodeData"
      else nodeData
    }

    createTree.printToString(x => indent(x.data.get)) shouldBe "ROOT\n  A0\n    B0\n      C0\n      C1\n      C2\n    B1\n      C3\n      C4\n      C5\n  A1\n    B2\n      C6\n      C7\n  A2"
  }

  "find should succeed for all nodes" in {
    val tree = createTree
    List("ROOT", "A0", "A1", "A2", "B0", "B1", "B2", "C0", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
      .map(x => (x, tree.find(y => y.data.get.equals(x))))
      .forall(z => z._2.isDefined && z._1.equals(z._2.get.data.get)) shouldBe true
  }

  "find should fail to find node, which does not exist" in {
    createTree.find(x => x.data.get.equals("SOMETHING")).isEmpty shouldBe true
  }

  "flat should succeed and return list of nodes with corresponding levels in the tree" in {
    val result = createTree.flat.map(x => (x._1, x._2.data.get))
    result.size shouldBe 15
    result.filter(x => x._1 == 0).map(x => x._2) shouldBe List("ROOT")
    result.filter(x => x._1 == 1).map(x => x._2).forall(x => x.startsWith("A")) shouldBe true
    result.filter(x => x._1 == 2).map(x => x._2).forall(x => x.startsWith("B")) shouldBe true
    result.filter(x => x._1 == 3).map(x => x._2).forall(x => x.startsWith("C")) shouldBe true
  }
}
