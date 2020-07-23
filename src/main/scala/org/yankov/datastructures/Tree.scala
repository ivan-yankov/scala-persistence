package org.yankov.datastructures

import scala.annotation.tailrec

case class Node[T](data: Option[T], children: List[Node[T]]) {
  def asFlatten(level: Int, index: Int, parentIndex: Int): FlattenNode[T] = FlattenNode(
    level = level,
    index = index,
    parentIndex = parentIndex,
    Node(data, children)
  )
}

case class FlattenNode[T](level: Int, index: Int, parentIndex: Int, node: Node[T]) {
  def withIndex(newIndex: Int): FlattenNode[T] = FlattenNode(
    level = level,
    index = newIndex,
    parentIndex = parentIndex,
    node = node
  )
}

case class Tree[T](root: Node[T]) {
  @tailrec
  final def flat(node: FlattenNode[T] = root.asFlatten(0, -1, -1),
                 index: Int = 0,
                 acc: List[FlattenNode[T]] = List(),
                 resultAcc: List[FlattenNode[T]] = List()): List[FlattenNode[T]] = {
    val indexedNode = node.withIndex(index)
    val newResultAcc = indexedNode :: resultAcc
    (indexedNode.node.children, acc) match {
      case (Nil, Nil) => newResultAcc.reverse
      case (Nil, x :: rest) => flat(x, index + 1, rest, newResultAcc)
      case (children, xs) =>
        val flattenChildren = children.map(x => x.asFlatten(node.level + 1, -1, indexedNode.index))
        flat(flattenChildren.head, index + 1, flattenChildren.tail.appendedAll(xs), newResultAcc)
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
    val result = flat().find(x => matcher(x.node))
    if (result.isDefined) Option(result.get.node) else Option.empty
  }

  def merge(parentMatch: Node[T] => Boolean, that: Tree[T])
           (implicit sortChildren: (Node[T], Node[T]) => Boolean): Tree[T] = {
    val thisFlatten = flat()
    val parent = thisFlatten.find(x => parentMatch(x.node))
    if (parent.isEmpty) this
    else {
      val thatRootLevel = parent.get.level + 1
      val thatRootIndex = thisFlatten.size
      val thatRootParentIndex = parent.get.index
      val thatFlatten = that.flat(that.root.asFlatten(thatRootLevel, thatRootIndex, thatRootParentIndex))
      Tree.build(thisFlatten.appendedAll(thatFlatten))
    }
  }
}

object Tree {
  def build[T](flatten: List[FlattenNode[T]])
              (implicit sortChildren: (Node[T], Node[T]) => Boolean): Tree[T] = {
    @tailrec
    def iterate(nodes: List[FlattenNode[T]], acc: List[FlattenNode[T]]): List[FlattenNode[T]] = {
      if (nodes.isEmpty) acc
      else {
        val depth = nodes.map(x => x.level).max
        val parentIndexes = acc.map(x => x.parentIndex).distinct
        val nodesAtDepth = nodes.filter(x => x.level == depth)
        val parents = nodesAtDepth.filter(x => parentIndexes.contains(x.index))
        val leaves = nodesAtDepth.filter(x => !parentIndexes.contains(x.index))
        val newAcc = acc
          .map(x => (parents.find(y => y.index == x.parentIndex).get, x))
          .groupMap(x => x._1)(x => x._2)
          .map(x => FlattenNode(
            x._1.level,
            x._1.index,
            x._1.parentIndex,
            Node(x._1.node.data, x._2.map(y => y.node).sortWith(sortChildren))))
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