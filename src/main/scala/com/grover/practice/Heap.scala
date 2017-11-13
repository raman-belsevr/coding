package com.grover.practice

import scala.collection.mutable.ArrayBuffer


case class Node[T <: Ordered[T]](value: T, left: Option[Node[T]] = None, right: Option[Node[T]] = None) extends Ordered[Node[T]] {
  override def compare(that: Node[T]): Int = {
    this.value.compare(that.value)
  }
}

object Mode extends Enumeration {
  val MIN, MAX = Value
}
case class Heap[T <: Ordered[T]](mode: Mode.Value) {

  private val content = ArrayBuffer.empty[T]
  private var next: Int = 0;

  private def inPlaceWrt(index1: Int, index2: Int): Boolean = {
    mode match {
      case Mode.MIN => {
        content(index1) <= content(index2)
      }
      case Mode.MAX => {
        content(index1) > content(index2)
      }
    }
  }

  private def higher(index1: Int, index2: Int) = {
    mode match {
      case Mode.MIN => {
        if(content(index1) <= content(index2)){index1} else {index2}
      }
      case Mode.MAX => {
        if(content(index1) > content(index2)){index1} else {index2}
      }
    }
  }

  def add(value: T) = {

    def bubbleUp(idx: Int, value: T) : Unit = {
      if (idx == 0){
        return
      }
      val parent = parentOf(idx)
      if(inPlaceWrt(parent, idx)){
        return
      } else {
        val temp = content(parent)
        content(parent) = value
        content(idx) = temp
        bubbleUp(parent, value)
      }
    }

    if (content.isEmpty) {
      content += value
      next = next + 1
    } else {
      content  += value
      bubbleUp(next, value)
      next = next + 1
    }
  }

  def remove() : Option[T] = {
    if (content.isEmpty || next == 0){
      None
    } else {
      val removed = content(0)
      // copy last element into root
      content(0) = content(next - 1)
      next = next - 1

      // now tree is broken
      if (next > 0) {
        trickleDown(0)
      }
      Some(removed)
    }
  }

  private def trickleDown(idx: Int) : Unit = {
    val left = 2 * idx + 1
    val right = left + 1

    // both children present
    if (left < next  && right < next) {
      // choose amongst children
      if (inPlaceWrt(idx, left) && inPlaceWrt(idx, right)){
        return
      }
      val toMoveUp = higher(left, right)

      // push up the one that needs to be higher
      val temp = content(idx)
      content(idx) = content(toMoveUp)
      content(toMoveUp) = temp

      // fix the sub-heap
      trickleDown(toMoveUp)

    } else if (left < next && right >= next){ // only left child present
      if (inPlaceWrt(idx, left)){
        return
      }
      val toMoveUp = left
      val temp = content(idx)
      content(idx) = content(toMoveUp)
      content(toMoveUp) = temp
    }


  }

  def display() : String = {
    content.slice(0, next).mkString("->")
  }

  private def parentOf(idx: Int) : Int = (idx - 1)/2

}

case class Value(v: Int) extends Ordered[Value] {
  override def compare(that: Value): Int = {
    return this.v.compare(that.v)
  }
}

object HeapApp extends App {
  val minHeap = Heap[Value](Mode.MAX)


  minHeap.add(Value(5))
  minHeap.add(Value(2))
  minHeap.add(Value(1))
  minHeap.add(Value(4))
  minHeap.add(Value(3))
  println(minHeap.display())


  var removed = Option(Value(-1))
  removed = minHeap.remove()
  println(removed)

  removed = minHeap.remove()
  println(removed)

  removed = minHeap.remove()
  println(removed)

  removed = minHeap.remove()
  println(removed)

  removed = minHeap.remove()
  println(removed)


}
