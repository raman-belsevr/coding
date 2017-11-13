package com.grover.practice.graph

import scala.collection.mutable


case class Node[T](value: T,
                   name: String,
                   neighbors: mutable.ListBuffer[Node[T]]) {

  override def toString(): String = {
    s"${name} [${(neighbors.map(_.name)).mkString(", ")}]"
  }
}

case class Edge[T](src: Node[T], dest: Node[T]) {

}

case class Graph[T](nodes: mutable.ListBuffer[Node[T]],
                    edges: mutable.ListBuffer[Edge[T]]) {

  private val nodeMap = mutable.HashMap.empty[String, Node[T]]

  nodes.foreach{
    node => {
      nodeMap += (node.name -> node)
    }
  }

  edges.foreach {
    edge => {
      edge.src.neighbors += edge.dest
      edge.dest.neighbors += edge.src
    }
  }

  def node(name: String) : Option[Node[T]] = {
    nodeMap.get(name)
  }
}

object Graph {

  def connect[T](graph: Graph[T], src: String, dst: String) : Boolean = {
     val srcNode = graph.node(src)
     val dstNode = graph.node(dst)
     if (srcNode.isDefined && dstNode.isDefined) {
       srcNode.get.neighbors += dstNode.get
       dstNode.get.neighbors += srcNode.get
       graph.edges += Edge(srcNode.get, dstNode.get)
       true
     } else {
       false
     }
  }

  def search[T](graph: Graph[T], value: T) : Option[Node[T]] = {
    None
  }

  def isConnected[T](graph: Graph[T], src: String, dst: String) : Boolean = {
    false
  }

  def path[T](graph: Graph[T], src: String, dst: String) : List[Node[T]] = {
    List.empty[Node[T]]
  }

  def dfs[T](): List[Node[T]] = {


    List.empty[Node[T]]
  }

  def bfs[T](): List[Node[T]] = {
    List.empty[Node[T]]
  }
}
