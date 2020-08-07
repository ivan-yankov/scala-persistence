package org.yankov.datastructures

import scala.annotation.tailrec

case class Node[T](level: Int, index: Int, parentIndex: Int, data: T) {
  def withIndex(newIndex: Int): Node[T] = Node(
    level = level,
    index = newIndex,
    parentIndex = parentIndex,
    data = data
  )
}

case class Tree[T](root: T) {

  def flat(node: Node[T] = Node(0, -1, -1, root))
          (implicit getChildren: T => List[T]): FlattenTree[T] = {
    @tailrec
    def iterate(node: Node[T],
                index: Int,
                acc: List[Node[T]],
                resultAcc: List[Node[T]]): List[Node[T]] = {
      val indexedNode = node.withIndex(index)
      val newResultAcc = indexedNode :: resultAcc
      (getChildren(indexedNode.data), acc) match {
        case (Nil, Nil) => newResultAcc.reverse
        case (Nil, x :: rest) => iterate(x, index + 1, rest, newResultAcc)
        case (children, xs) =>
          val childrenNodes = children.map(x => Node(node.level + 1, -1, indexedNode.index, x))
          iterate(childrenNodes.head, index + 1, childrenNodes.tail.appendedAll(xs), newResultAcc)
      }
    }

    FlattenTree(iterate(node, node.index + 1, List(), List()))
  }

  def printToString(printNode: T => String, indentation: String = "")
                   (implicit getChildren: T => List[T]): String = {
    @tailrec
    def indent(level: Int, node: String, acc: String = ""): String = {
      if (level == 0) s"$acc$node"
      else indent(level - 1, node, s"$acc$indentation")
    }

    flat().nodes.map(x => s"${indent(x.level, printNode(x.data))}").mkString("\n")
  }

  def find(matcher: T => Boolean)
          (implicit getChildren: T => List[T]): Option[T] = {
    val result = flat().nodes.find(x => matcher(x.data))
    if (result.isDefined) Option(result.get.data) else Option.empty
  }

  def merge(parentMatch: T => Boolean, that: Tree[T])
           (implicit getChildren: T => List[T],
            aggregate: (T, List[T]) => T): Tree[T] = {
    val thisFlatten = flat()
    val parent = thisFlatten.nodes.find(x => parentMatch(x.data))
    if (parent.isEmpty) this
    else {
      val thatRootLevel = parent.get.level + 1
      val thatRootIndex = thisFlatten.nodes.size
      val thatRootParentIndex = parent.get.index
      val thatFlatten = that.flat(Node(thatRootLevel, thatRootIndex, thatRootParentIndex, that.root))
      FlattenTree(thisFlatten.nodes.appendedAll(thatFlatten.nodes)).build()
    }
  }
}

case class FlattenTree[T](nodes: List[Node[T]]) {
  def build()(implicit aggregate: (T, List[T]) => T): Tree[T] = {
    @tailrec
    def iterate(nodes: List[Node[T]], acc: List[Node[T]]): List[Node[T]] = {
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
          .map(x => Node(
            x._1.level,
            x._1.index,
            x._1.parentIndex,
            aggregate(x._1.data, x._2.map(y => y.data))
          ))
          .toList
          .appendedAll(leaves)
          .sortWith((x, y) => x.index <= y.index)

        iterate(nodes.filter(x => x.level != depth), newAcc)
      }
    }

    val depth = nodes.map(x => x.level).max

    val result = iterate(nodes.filter(x => x.level != depth), nodes.filter(x => x.level == depth))
    Tree(result.head.data)
  }
}