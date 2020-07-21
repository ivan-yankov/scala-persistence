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
    def indent(level: Int, acc: String = ""): String = {
      if (level == 0) acc
      else indent(level - 1, s"$acc$indentation")
    }

    flat().map(x => s"${indent(x.level)}${printNode(x.node)}").mkString("\n")
  }

  def find(matcher: Node[T] => Boolean): Option[Node[T]] = {
    val result = traverse(matcher).find(x => x._2)
    if (result.isDefined) Option(result.get._1) else Option.empty
  }

  def flat(rootLevel: Int = 0, rootIndex: Int = 0, rootParentIndex: Int = -1): List[FlattenNode[T]] = {
    @tailrec
    def iterate(nodes: List[Node[T]], acc: List[FlattenNode[T]]): List[FlattenNode[T]] = {
      if (nodes.isEmpty) acc
      else {
        val node = nodes.head
        if (!acc.map(x => x.node).contains(node)) {
          val parent = acc.find(x => x.node.children.contains(node)).get
          val level = parent.level + 1
          val index = acc.count(x => x.level == level)
          val parentIndex = parent.index
          iterate(nodes.tail, acc.appended(FlattenNode(level, index, parentIndex, node)))
        }
        else {
          iterate(nodes.tail, acc)
        }
      }
    }

    val nodes = traverse(x => x).map(x => x._1).reverse
    iterate(nodes.tail, List(FlattenNode(rootLevel, rootIndex, rootParentIndex, nodes.head)))
  }

  def merge(parentMatch: Node[T] => Boolean, that: Tree[T]): Tree[T] = {
    ???
  }
}

object Tree {
  def build[T](flatten: List[FlattenNode[T]]): Tree[T] = {
    @tailrec
    def iterate(nodes: List[FlattenNode[T]], acc: List[FlattenNode[T]]): List[FlattenNode[T]] = {
      if (nodes.isEmpty) acc
      else {
        val depth = nodes.map(x => x.level).max
        val nodesAtDepth = nodes.filter(x => x.level == depth).groupMap(x => x.node.children.nonEmpty)(x => x)
        val parents = nodesAtDepth.getOrElse(true, List())
        val leaves = nodesAtDepth.getOrElse(false, List())
        val newAcc = acc
          .map(x => (parents.find(y => y.index == x.parentIndex).get, x))
          .groupMap(x => x._1)(x => x._2)
          .map(x => FlattenNode(x._1.level, x._1.index, x._1.parentIndex, Node(x._1.node.data, x._2.map(y => y.node))))
          .toList

        iterate(nodes.filter(x => x.level != depth), newAcc.appendedAll(leaves).sortWith((x, y) => x.index <= y.index))
      }
    }

    val depth = flatten.map(x => x.level).max
    val nodesByLevels = flatten.groupMap(x => x.level != depth)(x => x)

    val result = iterate(nodesByLevels(true), nodesByLevels(false))
    Tree(result.head.node)
  }
}