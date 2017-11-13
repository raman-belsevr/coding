package com.grover.practice

import scala.collection.mutable.ListBuffer

case class Stack[T] () {

  private val content = ListBuffer.empty[T]
  private var head = -1;

  def pop () : Option[T] = {
    if (head < 0) {
      None
    } else {
      val popped = content(head)
      content.remove(head)
      head = head - 1
      Some(popped)
    }
  }

  def push(value: T) : Unit = {
    content += value
    head = head + 1
  }

  def display(): String = content.mkString(" -> ")

}


object StackApp extends App {
  val stk = Stack[Int]()
  stk.push(1)
  stk.push(2)

  stk.pop()
  stk.pop()
  println(stk.display())
}
