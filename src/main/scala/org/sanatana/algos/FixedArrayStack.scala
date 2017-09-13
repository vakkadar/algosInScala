package org.sanatana.algos
import scala.io.StdIn._
case class FixedArrayStack(capacity:Int){

  private[this] var internal = new Array[String](capacity)

  private [this] var positionOfHead = -1

  def isEmpty :Boolean = positionOfHead == -1

  def push(item:String):Unit = {
    positionOfHead += 1
    internal(positionOfHead) = item
  }

  def pop():String = {
    if (isEmpty)
      throw new IllegalStateException("Cannot call pop on a empty stack")
    else {
      val res = internal(positionOfHead)
      internal(positionOfHead) = null
      positionOfHead -= 1
      res
    }

  }
}




object ArrayStackApp extends App{

  val stack =  FixedArrayStack(5)

  def loop(input:String):Unit = {
    if(input != ""){
      if(input == "-")
        println(stack.pop())
      else
        stack.push(input)
      loop(readLine())
    }
    else
      println("Exiting")
  }
  loop(readLine())


}
