package com.grover.practice

import scala.collection.mutable.ListBuffer


case class Queue[T]() {

  private val content = ListBuffer.empty[T]
  private var head = -1
  private var tail = -1

  def display(): String = {
    val list = ListBuffer.empty[T]
    for (index <- head to tail){
        list.append(content(index))
    }
    list.mkString("->")
  }

  def enqueue(value: T) = {
    content += value
    if (head < 0) { head = 0 }
    tail = tail + 1
  }

  def dequeue() : Option[T] = {
    if (head < 0 || head > tail) {
      None
    }
    else {
      val first = content(head)
      head = head + 1
      Some(first)
    }
  }

  def size = if (head < 0) 0 else {tail - head + 1}

}

object QueueApp extends App {
  val q = Queue[Int]()
  q.enqueue(1)
  q.enqueue(2)
  q.enqueue(3)

  println(q.display())
  q.dequeue()
  println(q.size)
  q.dequeue()
  println(q.size)
  println(q.display())


}
