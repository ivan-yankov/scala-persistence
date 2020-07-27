package org.yankov.datastructures

import org.scalatest.{Matchers, WordSpec}

class TreeTest extends WordSpec with Matchers {
  case class StringNode(name: String, children: List[StringNode] = List())

  implicit def getChildren(parent: Node[StringNode]): List[Node[StringNode]] =
    parent.data.children.map(y => Node(y))

  implicit def aggregate(parent: Node[StringNode], children: List[Node[StringNode]]): Node[StringNode] =
    Node(StringNode(parent.data.name, children.map(y => y.data).sortWith((x, y) => x.name.compareTo(y.name) <= 0)))

  private def createStringNode(): StringNode = StringNode(
    "ROOT",
    List(
      StringNode(
        "A0",
        List(
          StringNode("B0", List(StringNode("C0"), StringNode("C1"), StringNode("C2"))),
          StringNode("B1", List(StringNode("C3"), StringNode("C4"), StringNode("C5")))
        )
      ),
      StringNode(
        "A1",
        List(
          StringNode("B2", List(StringNode("C6"), StringNode("C7")))
        )
      ),
      StringNode("A2")
    )
  )

  private def createTree(root: StringNode = createStringNode()): Tree[StringNode] = Tree(Node(root))

  private def createMergedTree1: Tree[StringNode] = {
    val root = StringNode(
      "ROOT",
      List(
        StringNode(
          "A0",
          List(
            StringNode("B0", List(StringNode("C0"), StringNode("C1"), StringNode("C2"))),
            StringNode("B1", List(StringNode("B1C"), StringNode("C3"), StringNode("C4"), StringNode("C5")))
          )
        ),
        StringNode(
          "A1",
          List(
            StringNode("B2", List(StringNode("C6", List(StringNode("C6C", List(StringNode("C6C0"), StringNode("C6C1"), StringNode("C6C2"))))), StringNode("C7")))
          )
        ),
        StringNode("A2", List(StringNode("A2C"))),
        StringNode("RC")
      )
    )

    createTree(root)
  }

  private def createMergedTree2: Tree[StringNode] = {
    val root = StringNode(
      "ROOT",
      List(
        StringNode(
          "A0",
          List(
            StringNode("B0", List(StringNode("C0"), StringNode("C1"), StringNode("C2"), createStringNode())),
            StringNode("B1", List(StringNode("C3"), StringNode("C4"), StringNode("C5")))
          )
        ),
        StringNode(
          "A1",
          List(
            StringNode("B2", List(StringNode("C6"), StringNode("C7")))
          )
        ),
        StringNode("A2")
      )
    )

    createTree(root)
  }

  "printToString should succeed" in {
    createTree().printToString(x => x.data.name, "  ") shouldBe "ROOT\n  A0\n    B0\n      C0\n      C1\n      C2\n    B1\n      C3\n      C4\n      C5\n  A1\n    B2\n      C6\n      C7\n  A2"
  }

  "find should succeed for all nodes" in {
    val tree = createTree()
    List("ROOT", "A0", "A1", "A2", "B0", "B1", "B2", "C0", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
      .map(x => (x, tree.find(y => y.data.name.equals(x))))
      .forall(z => z._2.isDefined && z._1.equals(z._2.get.data.name)) shouldBe true
  }

  "find should fail to find node, which does not exist" in {
    createTree().find(x => x.data.name.equals("SOMETHING")).isEmpty shouldBe true
  }

  "flat should succeed and return list of nodes with corresponding flatten key in the tree" in {
    val result = createTree().flat().nodes.map(x => (List(x.level, x.index, x.parentIndex), x.node.data.name))

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
    createTree().merge(x => x.name.equals("B0"), createTree()).flat().build().root shouldBe createMergedTree2.root
  }

  "build should succeed" in {
    createTree().flat().build().root shouldBe createTree().root
  }

  "merge should succeed in chain merges" in {
    val trees = List(
      Tree(Node(StringNode("RC"))),
      Tree(Node(StringNode("A2C"))),
      Tree(Node(StringNode("B1C"))),
      Tree(Node(StringNode("C6C", List(StringNode("C6C0"), StringNode("C6C1"), StringNode("C6C2")))))
    )

    val tree = createTree()
      .merge(x => x.name.equals("ROOT"), trees.head)
      .merge(x => x.name.equals("A2"), trees(1))
      .merge(x => x.name.equals("B1"), trees(2))
      .merge(x => x.name.equals("C6"), trees(3))

    tree.root shouldBe createMergedTree1.root
  }

  "merge should succeed in merging subtree" in {
    createTree().merge(x => x.name.equals("B0"), createTree()).root shouldBe createMergedTree2.root
  }

  "merge should return same tree if parent is not found" in {
    createTree().merge(x => x.name.equals("SOMETHING"), Tree(Node(StringNode("CHILD")))).root shouldBe createTree().root
  }
}
