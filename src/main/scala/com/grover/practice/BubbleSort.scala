package com.grover.practice


object Sorting {

  object Mode extends Enumeration {
    val ASC, DESC = Value
  }

}

/**
  * Created by raman on 11/3/17.
  */
class BubbleSort extends SortingAlgo {

  def sort(values: Array[Int], mode: Sorting.Mode.Value) = {

    _sort(values, needsToMoveToRight(mode))
  }

  def _sort(values: Array[Int], comparison: (Int, Int) => Boolean) = {
    var continue = true
    while(continue){
      var swaps = 0
      for (i <- 0 to values.length - 2){
        if (comparison(values(i), values(i+1))){
          val temp = values(i)
          values(i) = values(i+1)
          values(i+1) = temp
          swaps = swaps + 1
        }
      }
      continue = swaps > 0
    }
  }

}

class SelectionSort extends SortingAlgo {

  override def sort(values: Array[Int], mode: Sorting.Mode.Value) = {
    _sort(values, needsToMoveToRight(mode))
  }

  private def _sort(values: Array[Int], comparison: (Int, Int) => Boolean) = {

    for (i <- 0 to values.size -1){
      val selected = _select(values, i, comparison)
      if(selected != i) {
        val temp = values(i)
        values(i) = values(selected)
        values(selected) = temp
      }
    }
  }

  private def _select(values: Array[Int], start: Int, needsToMoveToRight: (Int, Int) => Boolean) = {
    var selected = start
    for (i <- start to values.size -1){
      if(needsToMoveToRight(values(selected), values(i))){
        selected = i
      }
    }
    selected
  }
}

class InsertionSort extends SortingAlgo  {

  override def sort(values: Array[Int], mode: Sorting.Mode.Value) = {
    _sort(values, needsToMoveToRight(mode))
  }

  private def _sort(values: Array[Int], comparisonOp: (Int, Int) => Boolean) = {
    for (i <- 1 to values.size-1){
      insertIntoSorted(values, 0, i-1, values(i), comparisonOp)
    }
  }

  private def  insertIntoSorted(values: Array[Int], begin: Int, end: Int, value:  Int, comparisonOp: (Int, Int) => Boolean) = {
   var j = begin
   while(!comparisonOp(values(j), value) && j <= end){
     j = j + 1
   }
   if (j <= end) {
     for (k <- ((j+1) to end+1).reverse){
       values(k) = values(k-1)
     }
     values(j) = value
   }

  }

}

object BubbleSortApp extends App {
  var values= Array[Int](4, 3, 8, 2, 10, 1)
  val bubbleSorter = new BubbleSort()
  bubbleSorter.sort(values, Sorting.Mode.DESC)
  println(values.mkString(", "))

  values= Array[Int](4, 3, 8, 2, 10, 1)
  val selectionSorter = new SelectionSort()
  selectionSorter.sort(values, Sorting.Mode.DESC)
  println(values.mkString(", "))

  values= Array[Int](4, 3, 8, 2, 10, 1)
  val insertionSorter = new InsertionSort()
  insertionSorter.sort(values, Sorting.Mode.ASC)

  println(values.mkString(", "))
}
