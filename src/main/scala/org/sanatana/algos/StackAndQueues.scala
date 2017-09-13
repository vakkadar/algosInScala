package org.sanatanaalgos


import scala.io.StdIn._

case class Node(item:String, next:Node)

case class MutableLinkedListStackOfStrings() {
  private[this] var firstNode:Node = null


  def isEmpty:Boolean = firstNode == null
  def pop():String = {
    if(isEmpty)
      throw new IllegalStateException("Cannot call pop on a empty stack")
    else {
      val res = firstNode.item
      firstNode = firstNode.next
      res
    }
  }
  def push(s:String):Unit = {
    val node = Node(s, firstNode)
    firstNode = node
  }
}
case class ImmutableNode(item:String, next:Option[ImmutableNode])
case class ImmutableStack(nodeOpt:Option[ImmutableNode] = None) {

  def isEmpty:Boolean = nodeOpt.isEmpty
  def pop():(String,ImmutableStack)  = {
    if(isEmpty)
      throw new IllegalStateException("Cannot call pop on a empty stack")
    else {
      val node = nodeOpt.get
     (node.item, ImmutableStack(node.next))
    }
  }
  def push(s:String):ImmutableStack = {
    val node = ImmutableNode(s, nodeOpt)
    ImmutableStack(Some(node))
  }
}


object StackApp extends App{

  val stack =  MutableLinkedListStackOfStrings()

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

object ImmutableStackApp extends App{
  def loop(input:String, stack:ImmutableStack):Unit = {
    if(input != ""){
      val s =if(input == "-") {
        val (item, s2) = stack.pop
        println(item)
        s2
      }
      else
        stack.push(input)
      loop(readLine(), s)
    }
    else
      println("Exiting")
  }
  loop(readLine(),ImmutableStack() )


}
