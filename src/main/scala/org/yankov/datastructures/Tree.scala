package org.yankov.datastructures

import scala.annotation.tailrec

case class Node[T](data: T) {
  def asFlatten(level: Int, index: Int, parentIndex: Int): FlattenNode[T] = FlattenNode(
    level = level,
    index = index,
    parentIndex = parentIndex,
    Node(data)
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

case class Tree[T](root: Node[T],
                   getChildren: Node[T] => List[Node[T]],
                   aggregate: (Node[T], List[Node[T]]) => Node[T]) {

  def flat(node: FlattenNode[T] = root.asFlatten(0, -1, -1)): FlattenTree[T] = {
    @tailrec
    def iterate(node: FlattenNode[T],
                index: Int,
                acc: List[FlattenNode[T]],
                resultAcc: List[FlattenNode[T]]): List[FlattenNode[T]] = {
      val indexedNode = node.withIndex(index)
      val newResultAcc = indexedNode :: resultAcc
      (getChildren(indexedNode.node), acc) match {
        case (Nil, Nil) => newResultAcc.reverse
        case (Nil, x :: rest) => iterate(x, index + 1, rest, newResultAcc)
        case (children, xs) =>
          val flattenChildren = children.map(x => x.asFlatten(node.level + 1, -1, indexedNode.index))
          iterate(flattenChildren.head, index + 1, flattenChildren.tail.appendedAll(xs), newResultAcc)
      }
    }

    FlattenTree(
      iterate(node, node.index + 1, List(), List()),
      getChildren,
      aggregate
    )
  }

  def printToString(printNode: Node[T] => String, indentation: String = ""): String = {
    @tailrec
    def indent(level: Int, node: String, acc: String = ""): String = {
      if (level == 0) s"$acc$node"
      else indent(level - 1, node, s"$acc$indentation")
    }

    flat().nodes.map(x => s"${indent(x.level, printNode(x.node))}").mkString("\n")
  }

  def find(matcher: Node[T] => Boolean): Option[Node[T]] = {
    val result = flat().nodes.find(x => matcher(x.node))
    if (result.isDefined) Option(result.get.node) else Option.empty
  }

  def merge(parentMatch: T => Boolean, that: Tree[T]): Tree[T] = {
    val thisFlatten = flat()
    val parent = thisFlatten.nodes.find(x => parentMatch(x.node.data))
    if (parent.isEmpty) this
    else {
      val thatRootLevel = parent.get.level + 1
      val thatRootIndex = thisFlatten.nodes.size
      val thatRootParentIndex = parent.get.index
      val thatFlatten = that.flat(that.root.asFlatten(thatRootLevel, thatRootIndex, thatRootParentIndex))
      FlattenTree(thisFlatten.nodes.appendedAll(thatFlatten.nodes), getChildren, aggregate).build()
    }
  }
}

case class FlattenTree[T](nodes: List[FlattenNode[T]],
                          getChildren: Node[T] => List[Node[T]],
                          aggregate: (Node[T], List[Node[T]]) => Node[T]) {
  def build(): Tree[T] = {
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
            aggregate(x._1.node, x._2.map(y => y.node))
          ))
          .toList
          .appendedAll(leaves)
          .sortWith((x, y) => x.index <= y.index)

        iterate(nodes.filter(x => x.level != depth), newAcc)
      }
    }

    val depth = nodes.map(x => x.level).max

    val result = iterate(nodes.filter(x => x.level != depth), nodes.filter(x => x.level == depth))
    Tree(result.head.node, getChildren, aggregate)
  }
}