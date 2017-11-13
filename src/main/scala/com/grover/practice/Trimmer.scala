package com.grover.practice

import scala.collection.mutable.ArrayBuffer


class Trimmer {

  def trim(input: String) : String = {
    val inputArray = input.toCharArray
    val target = ArrayBuffer.empty[Char]

    var prevSpace = true
    for (index <- 0 to inputArray.length-1){
      if(inputArray(index) != ' '){
        target += inputArray(index)
        prevSpace = false
      }
      if (inputArray(index) == ' '){
        if(!prevSpace){
          target += inputArray(index)
        }
        prevSpace = true
      }
    }
     target.mkString("")
  }

}

object Trimmer extends App {

  val input = "Raman Grover     lives     in     Milpitas  "
  val trimmer = new Trimmer()
  println(trimmer.trim(input))
}
