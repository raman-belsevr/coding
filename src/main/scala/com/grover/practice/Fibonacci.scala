package com.grover.practice

import scala.collection.mutable.ListBuffer

/**
  * Created by raman on 11/6/17.
  */
class Fibonacci {

  def series(n: Int) : List[Int] = {
    var a = 0
    var b = 1
    var num = 2

    if (n == 1){
      List(a)
    } else if (n == 2) {
      List (a, b)
    } else {
      var fib = 0
      var result = ListBuffer.empty[Int]
      result ++= List(a, b)
      while(num < n) {
        fib = a + b
        a = b
        b = fib
        num = num + 1
        result += fib
      }
      result.toList
    }
  }

  def nfibonacci(n: Int) : Int = {
    if (n == 1){
      0
    } else if (n == 2) {
      1
    } else {
      var a = 0
      var b = 1
      var next = a + b
      var num = 3
      while (num <= n){
        next = a + b
        a = b
        b = next
        num = num + 1
      }
      next
    }
  }
}

object FibonacciApp  extends App {
  val obj = new Fibonacci()
  println(obj.series(10).mkString(", "))
  println(obj.nfibonacci(9))
}
