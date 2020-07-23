package org.yankov.datastructures

import org.scalatest.{Matchers, WordSpec}

class TreeTest extends WordSpec with Matchers {
  implicit def sortChildren: (Node[String], Node[String]) => Boolean =
    (x, y) => x.data.get.compareTo(y.data.get) <= 0

  private def createTree(): Tree[String] = {
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
    Tree(Node(Option(s"ROOT"), a))
  }

  private def createMergedTree1: Tree[String] = {
    val c = List(
      Node(Option("C0"), List()),
      Node(Option("C1"), List()),
      Node(Option("C2"), List()),
      Node(Option("C3"), List()),
      Node(Option("C4"), List()),
      Node(Option("C5"), List()),
      Node(Option("C6"), List(Node(Option("C6C"), List(
        Node(Option("C6C0"), List()),
        Node(Option("C6C1"), List()),
        Node(Option("C6C2"), List())
      )))),
      Node(Option("C7"), List()),
    )
    val bChildren = List(
      List(c.head, c(1), c(2)),
      List(Node(Option("B1C"), List()), c(3), c(4), c(5)),
      List(c(6), c(7))
    )
    val b = List.tabulate(3)(x => Node(Option(s"B$x"), bChildren(x)))
    val aChildren = List(
      List(b.head, b(1)),
      List(b(2)),
      List(Node(Option("A2C"), List()))
    )
    val a = List.tabulate(3)(x => Node(Option(s"A$x"), aChildren(x)))
    Tree(Node(Option("ROOT"), a.appended(Node(Option("RC"), List()))))
  }

  private def createMergedTree2: Tree[String] = {
    val subTree = createTree()

    val c = List.tabulate(8)(x => Node(Option(s"C$x"), List()))
    val bChildren = List(
      List(c.head, c(1), c(2), subTree.root),
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

  private def createFlattenTree: List[FlattenNode[String]] = {
    List(
      FlattenNode(0, 0, -1, Node(Option("ROOT"), List(Node(Option("A0"), List(Node(Option("B0"), List(Node(Option("C0"), List()), Node(Option("C1"), List()), Node(Option("C2"), List()))), Node(Option("B1"), List(Node(Option("C3"), List()), Node(Option("C4"), List()), Node(Option("C5"), List()))))), Node(Option("A1"), List(Node(Option("B2"), List(Node(Option("C6"), List()), Node(Option("C7"), List()))))), Node(Option("A2"), List())))),
      FlattenNode(1, 1, 0, Node(Option("A0"), List(Node(Option("B0"), List(Node(Option("C0"), List()), Node(Option("C1"), List()), Node(Option("C2"), List()))), Node(Option("B1"), List(Node(Option("C3"), List()), Node(Option("C4"), List()), Node(Option("C5"), List())))))),
      FlattenNode(2, 2, 1, Node(Option("B0"), List(Node(Option("C0"), List()), Node(Option("C1"), List()), Node(Option("C2"), List())))),
      FlattenNode(3, 3, 2, Node(Option("C0"), List())),
      FlattenNode(3, 4, 2, Node(Option("C1"), List())),
      FlattenNode(3, 5, 2, Node(Option("C2"), List())),
      FlattenNode(2, 6, 1, Node(Option("B1"), List(Node(Option("C3"), List()), Node(Option("C4"), List()), Node(Option("C5"), List())))),
      FlattenNode(3, 7, 6, Node(Option("C3"), List())),
      FlattenNode(3, 8, 6, Node(Option("C4"), List())),
      FlattenNode(3, 9, 6, Node(Option("C5"), List())),
      FlattenNode(1, 10, 0, Node(Option("A1"), List(Node(Option("B2"), List(Node(Option("C6"), List()), Node(Option("C7"), List())))))),
      FlattenNode(2, 11, 10, Node(Option("B2"), List(Node(Option("C6"), List()), Node(Option("C7"), List())))),
      FlattenNode(3, 12, 11, Node(Option("C6"), List())),
      FlattenNode(3, 13, 11, Node(Option("C7"), List())),
      FlattenNode(1, 14, 0, Node(Option("A2"), List()))
    )
  }

  "printToString should succeed" in {
    createTree().printToString(x => x.data.get) shouldBe "ROOT\n  A0\n    B0\n      C0\n      C1\n      C2\n    B1\n      C3\n      C4\n      C5\n  A1\n    B2\n      C6\n      C7\n  A2"
  }

  "find should succeed for all nodes" in {
    val tree = createTree()
    List("ROOT", "A0", "A1", "A2", "B0", "B1", "B2", "C0", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
      .map(x => (x, tree.find(y => y.data.get.equals(x))))
      .forall(z => z._2.isDefined && z._1.equals(z._2.get.data.get)) shouldBe true
  }

  "find should fail to find node, which does not exist" in {
    createTree().find(x => x.data.get.equals("SOMETHING")).isEmpty shouldBe true
  }

  "flat should succeed and return list of nodes with corresponding flatten key in the tree" in {
    val result = createTree().flat().map(x => (List(x.level, x.index, x.parentIndex), x.node.data.get))

    result.size shouldBe 15

    result.find(x => x._2.equals("ROOT")).get._1 shouldBe List(0, 0, -1)

    result.find(x => x._2.equals("A0")).get._1 shouldBe List(1, 1, 0)
    result.find(x => x._2.equals("A1")).get._1 shouldBe List(1, 10, 0)
    result.find(x => x._2.equals("A2")).get._1 shouldBe List(1, 14, 0)

    result.find(x => x._2.equals("B0")).get._1 shouldBe List(2, 2, 1)
    result.find(x => x._2.equals("B1")).get._1 shouldBe List(2, 6, 1)
    result.find(x => x._2.equals("B2")).get._1 shouldBe List(2, 11, 10)

    result.find(x => x._2.equals("C0")).get._1 shouldBe List(3, 3, 2)
    result.find(x => x._2.equals("C1")).get._1 shouldBe List(3, 4, 2)
    result.find(x => x._2.equals("C2")).get._1 shouldBe List(3, 5, 2)
    result.find(x => x._2.equals("C3")).get._1 shouldBe List(3, 7, 6)
    result.find(x => x._2.equals("C4")).get._1 shouldBe List(3, 8, 6)
    result.find(x => x._2.equals("C5")).get._1 shouldBe List(3, 9, 6)
    result.find(x => x._2.equals("C6")).get._1 shouldBe List(3, 12, 11)
    result.find(x => x._2.equals("C7")).get._1 shouldBe List(3, 13, 11)
  }

  "flat subtree should succeed" in {
    val flatten = createTree().merge(x => x.data.get.equals("B0"), createTree()).flat()
    Tree.build(flatten) shouldBe createMergedTree2
  }

  "build should succeed" in {
    Tree.build(createFlattenTree) shouldBe createTree()
  }

  "merge should succeed in chain merges" in {
    val trees = List(
      Tree(Node(Option("RC"), List())),
      Tree(Node(Option("A2C"), List())),
      Tree(Node(Option("B1C"), List())),
      Tree(Node(Option("C6C"), List(
        Node(Option("C6C0"), List()),
        Node(Option("C6C1"), List()),
        Node(Option("C6C2"), List())))
      )
    )

    val tree = createTree()
      .merge(x => x.data.get.equals("ROOT"), trees.head)
      .merge(x => x.data.get.equals("A2"), trees(1))
      .merge(x => x.data.get.equals("B1"), trees(2))
      .merge(x => x.data.get.equals("C6"), trees(3))

    tree shouldBe createMergedTree1
  }

  "merge should succeed in merging subtree" in {
    createTree().merge(x => x.data.get.equals("B0"), createTree()) shouldBe createMergedTree2
  }

  "merge should return same tree if parent is not found" in {
    createTree().merge(x => x.data.get.equals("SOMETHING"), Tree(Node(Option("CHILD"), List()))) shouldBe createTree()
  }
}
