package org.yankov.datastructures

import scala.annotation.tailrec

case class Node[T](data: Option[T], children: List[Node[T]])

case class Tree[T](root: Node[T]) {
  type NodeType = Node[T]

  @tailrec
  final def traverse[R](f: NodeType => R,
                        node: NodeType = root,
                        acc: List[NodeType] = List(),
                        rAcc: List[(NodeType, R)] = List()): List[(NodeType, R)] = {
    val newResultAcc = (node, f(node)) :: rAcc
    (node.children, acc) match {
      case (Nil, Nil) => newResultAcc
      case (Nil, x :: rest) => traverse(f, x, rest, newResultAcc)
      case (child :: otherChildren, xs) => traverse(f, child, otherChildren.appendedAll(xs), newResultAcc)
    }
  }

  def printToString(printNode: NodeType => String): String = {
    traverse(x => printNode(x)).map(x => x._2).reverse.mkString("\n")
  }

  def find(matcher: NodeType => Boolean): Option[NodeType] = {
    val result = traverse(matcher).find(x => x._2)
    if (result.isDefined) Option(result.get._1) else Option.empty
  }

  def flat: List[(Int, NodeType)] = {
    @tailrec
    def iterate(nodes: List[NodeType], acc: List[(Int, NodeType)]): List[(Int, NodeType)] = {
      if (nodes.isEmpty) acc
      else {
        val node = nodes.head
        if (!acc.map(x => x._2).contains(node)) {
          val level = acc.find(x => x._2.children.contains(node)).get._1 + 1
          iterate(nodes.tail, acc.appended((level, node)))
        }
        else {
          iterate(nodes.tail, acc)
        }
      }
    }

    val nodes = traverse(x => x).map(x => x._1).reverse

    iterate(nodes.tail, List((0, nodes.head)))
  }

  def add(parent: NodeType, child: NodeType): Tree[T] = ???
}
