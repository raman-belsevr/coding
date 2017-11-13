package com.grover.practice

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by raman on 11/4/17.
  */
case class MergeSort() {

    def sort(first: Array[Int], second: Array[Int], mode: Sorting.Mode.Value) = {


      def pickRemaining(first: Array[Int], second: Array[Int], indexFirst: Int, indexSecond: Int) : (Array[Int],  Int) = {
        if (indexFirst < first.length){
          (first, indexFirst)
        } else {
          (second, indexSecond)
        }
      }

      var left: Int   = 0
      var right: Int  = 0
      var target: Int = 0

      val sorted = Array.fill[Int](first.length + second.length){0}
      while(left < first.length && right < second.length){
        if (first(left) < second(right)){
            sorted(target) = first(left)
            left = left + 1
        } else {
            sorted(target) = second(right)
            right = right + 1
        }
        target = target  + 1
      }

      val (remainingValue, remainingIndex) = pickRemaining(first, second, left, right)
      for (index <- remainingIndex to remainingValue.length-1){
        sorted(target) = remainingValue(index)
        target = target + 1
      }

      sorted
    }

}

object MergeSortApp extends App {

  val sortedFirst  = Array[Int](1,3, 5, 7, 9, 12)
  val sortedSecond = Array[Int](0,2, 4, 6, 8, 10)

  val mergeSorter = MergeSort()
  val sorted = mergeSorter.sort(sortedFirst, sortedSecond, Sorting.Mode.ASC)
  println(sorted. mkString(", "))

}