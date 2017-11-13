package com.grover.practice

import scala.collection.mutable.ListBuffer


class ThreeSum {

  def threeSum(input: Array[Int]) : List[List[Int]]= {
    if  (input.length < 3) {
      List.empty[List[Int]]
    }
    else {
      val sorted = input.sorted
      var first = 0

      val results = ListBuffer.empty[List[Int]]

      while (first < input.length - 2) {
        var second = first + 1
        var third = first + 2
        while (second < sorted.length - 1 && third < sorted.length){

          val desired = (sorted(first) + sorted(second))

          while (third < sorted.length && sorted(third) < desired ) {
            third = third + 1
          }
          if (third < sorted.length && sorted(third) == desired){
            val result = List(sorted(first), sorted(second), sorted(third))
            if(!results.contains(result)) {
              results += result
            }
          }

          second = second + 1
        }
        if (second == sorted.length-1) {
          first = first + 1
        }
      }

      results.toList
    }
  }
}

object ThreeSumApp extends App {
  val obj = new ThreeSum()
  val input = Array[Int](-2,0,1,1,2)
//  val input = Array[Int](0, 0, 0, 0)

  val triplets = obj.threeSum(input)
  triplets.foreach{println}
}

//