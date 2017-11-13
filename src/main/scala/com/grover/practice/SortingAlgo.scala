package com.grover.practice

trait SortingAlgo {

  def needsToMoveToRight(mode: Sorting.Mode.Value) = {
    mode match {
      case Sorting.Mode.ASC => (a: Int, b: Int) => a > b
      case Sorting.Mode.DESC => (a: Int, b: Int) => a < b
    }
  }

  def sort(values: Array[Int], mode: Sorting.Mode.Value)
}
