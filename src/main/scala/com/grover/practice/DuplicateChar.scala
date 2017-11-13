package com.grover.practice

import scala.collection.mutable.ArrayBuffer

class DuplicateChar {

  def removeDuplicates(input: String) : String = {
    val inputArray = input.toCharArray()
    var prevChar: Option[Char] = None
    val target = ArrayBuffer.empty[Char]
    for (index <- 0 to inputArray.length-1){
       if (!prevChar.isDefined || !prevChar.get.equals(inputArray(index))){
         target += inputArray(index)
         prevChar = Some(inputArray(index))
       }
    }
    target.mkString("")
  }

}

object DuplicateChar extends App {

  val input = "aaa  nfdfdfd   nnnnnn def"
  val obj = new DuplicateChar()
  println(obj.removeDuplicates(input))
}
