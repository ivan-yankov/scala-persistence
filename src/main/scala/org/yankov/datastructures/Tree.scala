package org.yankov.datastructures

import scala.annotation.tailrec

case class Node[T](data: Option[T], children: List[Node[T]])

case class FlattenNode[T](level: Int, index: Int, parentIndex: Int, node: Node[T])

case class Tree[T](root: Node[T]) {
  @tailrec
  final def traverse[R](f: Node[T] => R,
                        node: Node[T] = root,
                        acc: List[Node[T]] = List(),
                        rAcc: List[(Node[T], R)] = List()): List[(Node[T], R)] = {
    val newResultAcc = (node, f(node)) :: rAcc
    (node.children, acc) match {
      case (Nil, Nil) => newResultAcc
      case (Nil, x :: rest) => traverse(f, x, rest, newResultAcc)
      case (child :: otherChildren, xs) => traverse(f, child, otherChildren.appendedAll(xs), newResultAcc)
    }
  }

  def printToString(printNode: Node[T] => String, indentation: String = "  "): String = {
    @tailrec
    def indent(level: Int, node: String, acc: String = ""): String = {
      if (level == 0) s"$acc$node"
      else indent(level - 1, node, s"$acc$indentation")
    }

    flat().map(x => s"${indent(x.level, printNode(x.node))}").mkString("\n")
  }

  def find(matcher: Node[T] => Boolean): Option[Node[T]] = {
    val result = traverse(matcher).find(x => x._2)
    if (result.isDefined) Option(result.get._1) else Option.empty
  }

  def flat(rootLevel: Int = 0, rootIndex: Int = 0, rootParentIndex: Int = -1): List[FlattenNode[T]] = {
    @tailrec
    def iterate(nodes: List[Node[T]], index: Int, acc: List[FlattenNode[T]]): List[FlattenNode[T]] = {
      if (nodes.isEmpty) acc
      else {
        val node = nodes.head
        val parent = acc.find(x => x.node.children.contains(node)).get
        val level = parent.level + 1
        val parentIndex = parent.index
        iterate(nodes.tail, index + 1, acc.appended(FlattenNode(level, index, parentIndex, node)))
      }
    }

    val nodes = traverse(x => x).map(x => x._1).reverse
    iterate(nodes.tail, rootIndex + 1, List(FlattenNode(rootLevel, rootIndex, rootParentIndex, nodes.head)))
  }

  def merge(parentMatch: Node[T] => Boolean, that: Tree[T]): Tree[T] = {
    val thisFlatten = flat()
    val parent = thisFlatten.find(x => parentMatch(x.node))
    if (parent.isEmpty) this
    else {
      val thatRootLevel = parent.get.level + 1
      val thatRootIndex = thisFlatten.size
      val thatRootParentIndex = parent.get.index
      val thatFlatten = that.flat(thatRootLevel, thatRootIndex, thatRootParentIndex)
      Tree.build(thisFlatten.appendedAll(thatFlatten))
    }
  }
}

object Tree {
  def build[T](flatten: List[FlattenNode[T]]): Tree[T] = {
    @tailrec
    def iterate(nodes: List[FlattenNode[T]], acc: List[FlattenNode[T]]): List[FlattenNode[T]] = {
      if (nodes.isEmpty) acc
      else {
        val depth = nodes.map(x => x.level).max
        val parentIndexes = acc.map(x => x.parentIndex).distinct
        val nodesAtDepth = nodes.filter(x => x.level == depth)//.groupMap(x => parentIndexes.contains(x.index))(x => x)
        val parents = nodesAtDepth.filter(x => parentIndexes.contains(x.index))
        val leaves = nodesAtDepth.filter(x => !parentIndexes.contains(x.index))
        val newAcc = acc
          .map(x => (parents.find(y => y.index == x.parentIndex).get, x))
          .groupMap(x => x._1)(x => x._2)
          .map(x => FlattenNode(x._1.level, x._1.index, x._1.parentIndex, Node(x._1.node.data, x._2.map(y => y.node))))
          .toList
          .appendedAll(leaves)
          .sortWith((x, y) => x.index <= y.index)

        iterate(nodes.filter(x => x.level != depth), newAcc)
      }
    }

    val depth = flatten.map(x => x.level).max

    val result = iterate(flatten.filter(x => x.level != depth), flatten.filter(x => x.level == depth))
    Tree(result.head.node)
  }
}