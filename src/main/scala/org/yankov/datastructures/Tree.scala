package org.yankov.datastructures

case class Node[K, T](key: K, data: Option[T], children: List[Node[K, T]])

case class Tree[K, T](root: Node[K, T]) {
  type NodeType = Node[K, T]

  def find(matcher: NodeType => Boolean): Option[NodeType] = {
    val k = traverse(root, List(), matcher, Map())
      .find(x => x._2)
      .get._1
    find(k)
  }

  def find(key: K): Option[NodeType] = {
    traverse(
      root,
      List(),
      x => if (x.key.equals(key)) Option(x) else Option.empty,
      Map())
      .find(x => x._2.isDefined)
      .get._2
  }

  def add(parent: NodeType, child: NodeType): Tree[K, T] = ???

  @scala.annotation.tailrec
  private def traverse[R](node: NodeType, acc: List[NodeType], f: NodeType => R, rAcc: Map[K, R]): Map[K, R] = {
    val newResultAcc = rAcc + (node.key -> f(node))
    (node.children, acc) match {
      case (Nil, Nil) => newResultAcc
      case (Nil, x :: rest) => traverse(x, rest, f, newResultAcc)
      case (child :: otherChildren, xs) => traverse(child, otherChildren ++ xs, f, newResultAcc)
    }
  }
}
